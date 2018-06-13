History
=======

Originally mentally-forked from BorgBackup to support sub-block updates, which adds complexity for questionable general purpose gain.  Then pulled in some ideas from decade-plus-old thoughts about making Squashfs writable - what if you could mount a borg backup and just write to it, instead of using the tool?  Joining some ideas from Logicblox, which has a fully-functional database backend, that would be safe if blocks are never rewritten, only changed.  So let's have a series of files, searching is a bit slower, but merging them in Fibonacci-heap style keeps things bounded.  But Logicblox also exposed me to B-epsilon trees (there called alpha trees), which do the same thing but in a more graceful way, and naturally provide versioning capabilities (though this seems to get tricky...).  And then I found Betrfs, which uses a B-espilon tree for *performance*; maybe this could actually be fast?

Plus, I want to use this for backups / filesystem, so it needs to be safe.  Let's use Rust, or, even better, prove it correct in Coq or a similar system...


Goals
=====

Efficient differential backup system (like Borg), ideally also useable as a (possibly somewhat slow) general purpose filesystem.  There doesn't seem to be a core reason why the two have to be distinct; they just require different tradeoffs.  Optimizing for efficient backup with filesystem use in mind should give something useable as a filesystem, especially given how fast modern NVMe SSDs can be; when you've got >1.5GB/s sequential and >100K IOPs, standard desktop usage can afford to have several seeks per op and still be responsive.

- Functional storage for safety and recovery; ideally, every change should create a new "generation" (similar to Btrfs), and cleanup should only be via user-controlled GC.
- Primary interface is as a standard Fuse FS; rsync, etc. work fine for backups.
- Should "work" on eventually-consistent storage (S3); with functional backend, data delay is safe; can either check for everything there, or fail/delay if something missing.
- Efficient sub-file diffs; borg/librsync-style chunking, compression, but with sub-block changes allowed (see my borg issue for details)
- As safe as possible - ideally correctness proofs, at least a safe language with lots of tests
- Copying a directory should be nearly free; if directories are first-class items and storage is fully functional, two references to identical directories can point to the same dentry.  Changes to either directory causes CoW to kick in.
  - This allows for free snapshots with no special infrastructure; just copy the directory and you have a snapshot.
  - Possibly via ioctl-ish interface, since AFAIK a recursive directory copy walks the whole tree.
  - Walking the whole tree *should* be free diskwise due to Borg/git-like object store; all hashes are the same, so duplicates are noted.
    - This requires directories to be objects as well, which may be a performanceh hit.
- Efficient diffs of directories/roots; this mostly falls out of CoW practically for free - if directories are unchanged, they should point at the same node.
- Reasonable (tunable) memory usage - should be sub-1MB for core datastructures (not counting cache/dirty that can be flushed).


Implementation
==============

Storage tree
------------

- Fully functional/CoW: B-epsilon trees seem promising
  - if you refer to not just the node, but the number of deltas, expanding a node can be done in-place with old data remaining unchanged (assuming no bugs in offsets).
    - CoW is tricky when nodes are shared between parents; if a node has two parents, updating from one needs to leave the other unchanged.
      - Number of deltas makes this safe; if going from 0->1, copy it if it has any, otherwise just add them on.
      - Or can split delta space into multiple segments and do ranges; more complex, theoretically better.
      - Not an issue for the core object store; if used for directories, messy.
  - For extra safety, can copy on each delta addition; usually only copies the root node; when the propagate down, lower nodes are bulk-updated, and the root node is also automatically updated.
    - Still potentially expensive depending on node size.  Pointers to number of deltas seems much more efficient, though makes CoW tricky.


Data blocks
-----------

The primary storage tree stores chunks.  Chunks can refer to other chunks for diffs.

- Most simply, a chunk can be LZ77-style: a sequence of elements, each of which is either a literal with length or a reference to a range within another block.
  - This allows for efficient small-updates.  References can be either by updates or by prefix/suffix matching as in my borg issue.
  - Compression can be either per-literal or per-chunk.
- Alternatively, a chunk could be an explicit diff from another chunk; the same as that one, but with these blocks updated.
- Generalizing both, take LZ77-style but hierarchical; this range is from that chunk, but with these subranges as literals (or copied from other chunks)
- It might also be nice to do hierarchical chunks: chunk them fairly small for deduping purposes, but store them combined together for better compression.  (E.g., store hashes for every 4k, but compress blocks of 128k.)
  - This can be done naturally via ranges: store the 128k block, then store 4k blocks as references.
    - Performance hit, though, since each 4k block will need to be looked up in the storage tree, versus some native hack which would allow the 128k to be read first.
    - Though the file could store a pointer to the 128k; if something changes, the 4k blocks would be deduped.  Tuneable performance/storage tradeoff.

Storing files/dirs
------------------

A file is a list of blocks, a directory is... a list of files/dirs?  A tree of files/dirs?  What?

- A file is a list of chunks
  - As a B-epsilson tree?  sequence of chunk hashes stored in a chunk?
    - sequence of chunk hashes in a chunk lets all of the chunk difference mechanisms Just Work.
    - B-espilon tree seems cleaner, if part of a larger tree, has batch-write properties for free.
- A directory is a list of files and dirs
  - If files and dirs are objects stored in main tree, identical to files, just pointing to file/dir chunks instead of chunks
    - Copying a directory is trivial; just a new pointer to the chunk.
    - CoW Just Works; adding a file changes the directory, creates a new chunk (with potential references to existing directory for small diffs.
    - Performance may suffer?  Maybe?
  - Could do standard B-tree style, with dirs/files as nodes.
    - Need to support multiple nodes in a single directory, a bit ugly.
    - Makes copy-on-write sharing a bit tricky.  Copying a directory points at the same b-tree node.  Can do simple ref counting, if >1 and change make a duplicate.


Root
----

Store a list of roots, last / first is current.  Allows time travel, reading from old versions in case of eventual consistency delay.

- Simplest implementation: list of them, checksums, pointer to most recent.
  - Writing a new root is atomically read/increment most recent pointer (to avoid stomping on someone else), write new root at pointer.
  - Mounting looks at most recent pointer, walks backward until a valid one is found.
  - Wraps around at end; can include previous node checksum...  maybe?  Tricky.

### Alternatives:

- Files could simply be stored as path->file maps, indexed by a B-epsilon tree.  Unfortunately, this makes copying directories expensive.

- Stratified b-trees?  Support versioning, but seem to want sparse version?
  - based on COLA...


GC
--

Want to be able to clear out old data.  If o

Misc Features
-------------

- send/receive
  - btrfs-compatible "send diff from these two roots"
    - A bit more flexible, since roots are defined by hashes, not generations.  Divergence is fine; it will just search down to diffs and make it work.
  - two-sided "send this over there"
    - figure out what hashes already exist, only send diffs
    - could use reversible-bloom-filter fanciness, or just naive hierarchical check of what's there.
- Metadata
  - Can store in a separate chunk, with filedata, or in the node pointer; the latter bloats the b-epsilon tree, but speeds up common uses.
- Historic snapshots vs. explicit copy snapshots
  - A generational/CoW system gives historical snapshots for free - just look at the CoW before the write.
  - Explicit copy snapshots copy a node, and anything in there is snapshotted.  Nodes that are modified without copies can be modified in place.
- FUSE-pure-userspace layer?
  - According to betrfs, FUSE really slows things down.  They moved in-kernel to avoid that, but that limits the runtime dramatically (e.g., probably can't use Haskell/Caml/Coq.
  - Instead, why not move to userspace?  Add a shim (LD_PRELOAD?) that replaces IO's with IPC directly to the process (pipe/ unix domain socket)?

Notes on the two trees
======================

There are two distinct trees here - the filesystem tree (directories + files) and the chunk storage tree.  The former is inherently tree-like; the latter is only a tree as an implementation detail.

- The filesystem tree can be stored either as an actual tree or as a set of chunk pointers.
  - The former is more standard and probably has much better performance properties, but wants versioning for historical snaphosts.
  - The latter is straightforward.
- The chunk tree is just a hash->chunk mapping.  It could be any datastructure with the requisite safety properties, and doesn't need fancy verisoning or history.  B-epsilon tree only makes sense for safety/performance/space tradeoff.



Round 2...
==========

Filesystem tree
---------------

Key idea - we can either lock or copy to ensure no concurrency issues: on a local filesystem, we can assume exclusion or lock; on S3, we can always copy nodes to ensure safety.  B-epsilon trees make this reasonable, hopefully.

On mount, we ensure a safe root by either locking (local/synchronous filesystems) or copying the root and marking the old root as parent.  This is a good general mechanism as well for tracking history.  Also, by naming roots, we can have multiple machines backing up simultaneously to different roots, while still sharing deduping.

We also track, for each node, how big its buffer is.  We also store in each pointer how big a buffer we expect.  This allows efficient CoW, assuming exclusive access to nodes.

On updates, we can safely write to the root node's buffer because it's exclusive one way or another.  Then on flush, we have two options:

- if it's synchronous, we can compare the length expected in the flushing node to the actual; if they match we can just extend the buffer below.  Otherwise, someone else extended it already, so we have to copy it.

- if we're on S3, we can't assume that the length is up to date, so we have to always copy the block.  This will waste some space, but keeps things fully functional and safe.

With this, we not only keep history, but can share nodes in multiple points in the tree, since they're updated safely:

- Suppose A/C and B/C point to the same node.  Updates to C can happen on both A and B, and be buffered there.  When A flushes, it will extend C's buffer.  Since B knows how much buffer it has, it will keep looking at the same node.  If A/C has to flush at some point, it'll be copied.  And when B/C gets flushed to, it will copy C since A/C already filled up the buffer.

Alternatively, we could have multiple buffers per node, but that seems complicated, and is still unsafe on S3.

So we can store the directory tree effectively entirely in the B-epsilon tree - each directory is a node (with potential sub-nodes if it's full).  This lets us batch updates via the buffers, keep CoW, etc.  Then files point to chunks in the hashed tree...  maybe we can point directly to the buffers, and just use the hash lookup tree for deduping on writes...

Note that we get history for free - recent history is just ignoring the buffer in the root node.  Then for older history, go to the root's parent; all of its children will still work properly assuming the node+buffer-size scheme above.

The multiple-roots needs some sort of merging; if two nodes update the same tree, we probably want to be able to merge them.  This has the usual auto-merging issues; we can expose to the user various (last-wins, this-one wins, rename-older, rename-both).  Though hopefully this is fairly rare.  At the very least, just allowing them both to be mounted simultaneously and/or diffed allows for reasonable manual/userspace merging.

Optimization - each node is "owned" by the root that created it.  A mount from that root can update it in place - this is safe, since that root can only be mounted once.  Everyone else still copies it.  This makes bulk updates *much* more efficient.

### Packing directories

Naively, we need to give each directory a node to enable CoW/deduping.  An alternative like Betrfs that stores a list of paths doesn't work at all.

Unfortunately, this may make the tree a lot taller and waste a lot of space on small directories that are rarely updated.

But a directory doesn't need to be a full node; if a directory is a node and an offset within the node, we can have an "item" in the tree that's not a pointer, but a directory metadata node, and then all nodes after it are in it.

Logically, a B-tree is just a sorted list.  So imagine each node is either a directory, or a file... issues with ending a directory and having to scan from start.  If we have a, a/b, a/c, d, d/e in one node, and d/f in the next... this gets weird.  More thought required.
  At the very least, it seems feasible to pack small directories: a, a/b, a/c, d, d/e, d/f could all be in one node.

Or we can use variable-length buffers and grow them on-demand; this trades of performance for potential storage savings, though CoW could easily completely negate them.

One elegant solution is that each node has a "base directory", then has paths relative to that.  So a typically node per directory tree would just point to the new directory node and append the directory to that base.  But we could also pack multiple directories, either breadth or depth wise: if a only contains b, b/c, and b/d, the node for a can contain entries for b, b/c, and b/d.  There might need to be some notation that b doesn't have a child directory; though it's also feasible to just store b's metadata, then a pointer to a child node based at b (which would then contain b/d).  Then, if a/e and a/f are created, we can add pointer under a to e and f; this seems pretty elegant, pending details.


Deduping
--------

The problem with a traditional directory tree is that it allows CoW, but doesn't dedupe directories - if two directories have the same files from a different source, the (chunks of the) files may be deduped, but the directory won't.  For deep trees, this may be significant.

One possibility is to hash directories as well and put them in the chunk tree (or a separate tree?); this keeps access fast, since it's direct, but still allows deduping by checking the chunk tree.  This seems similar to standard online-deduping, just on disk instead of in RAM (*cough* ZFS *cough*).  Sadly, this seems to require either an update to the chunk tree on every update, plus plays poorly with B-epsilon tree buffering - if we add a file to a directory, it will be buffered in the root until it flushes.  So there's no way to check for the hash.  This is sad.

Maybe check for directory dedupe on flush?  This could buffer a fair amount before it notices, but would still eventually solve the problem.  This is also kind of an inherent issue with continuous writing (vs something like borg that does specific generations) - to keep history, we have to keep the un-deduped history.  In theory, though, we could rewrite that history to be deletions from the directory...  scary to do that rewrite, though.  Though history is a perk; it takes up space, and there's no way around that.

And can we dedupe (or even CoW) directories that haven't yet been flushed?  If it lives in a buffer, there's nothing for a copy to point to.  Though if a directory is in a buffer, any copy of it would also be in a buffer; we just need to have a recursive-copy upsert.

To keep space efficiency, a high-epsilon tree needs to use the buffers; forcing a flush for deduping breaks that; if each node has 256 entries and only 16 pointers (epsilon = 1/2), a fully-flushed tree will waste 15/16 of its space.  One approach would be to have an explicit "dedupe dir tree" operation that walks the tree and builds up each node in a DFS, which should be fairly memory-efficient, since we only have to store the path to the root and direct descendents.  If we assume efficient GC, this may be feasible.

It's conceivably possible to use some sort of linear unordered hash; if a directory is, e.g., the hash of all paths it contains combined with the hash of each path, the buffer is enough to update it.  This still won't account for buffers above it, and still has the frequent update issue, but seems vaguely plausible.

Also, the deduping tree should really be shared across all roots to be fully effective; we could always allow read-only access to other deduping trees.

GC
--

Reference counting might work locally, maybe.  Otherwise, have to do a full mark and sweep or copy...  copy actually seems pretty feasible for S3 - if we have to read everything, anyway, writing it back isn't a terrible idea.

Reference counting seems plausible if we have exclusive access, we can reference count nodes, and keep a list of nodes to GC.  If the nodes are fairly large, it's entirely plausible to track free space (bitmap, malloc-style linked list, etc.) for on-disk; for fs-backed, we can probably make each node its own file, which passes the buck to the underlying filesystem.

Generational copying doesn't really make sense, because old nodes can be updated to point at new nodes (alpha-tree).  Incremental copying may kind of work, but it's awkward.  Could add rewrites as alpha-tree upserts, but naively they'd have to spread to every node, so would be huge.

Incremental - could add a hash of movements - like pushing down, but global.  Then rewrite at leisure.  Sadly, only works once you know every root has it.

Metadata
--------

Since tree nodes are rarely copied, we may be able to get away with metadata in the nodes (e.g. a node item is a key, pointer, and block of metadata or pointer to metadata at end of node).  This would speed up common operations; it would be nice to ls -l without having to look below the directory.  This is also good for writes; modifying metadata on a node would require updating the parent directory anyway due to CoW, so it avoids having to write a node lower.

