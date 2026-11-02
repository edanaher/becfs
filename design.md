Overview
========

becfs is a B-epsilon-tree based backup system/filesystem.  It is designed to be reasonably performant, safe, and store mulitiple deduplicated copies of data.  It is optimized for the linear-backups case, where each backup is an update of the one before it.  However, it will work with any topology of snaphosts, it just may not perform as well.

The Metadata Tree
=================

The core of becfs is the b-epsilon tree storing the file hierarchy, metadata, and small data.  This is heavily based on Betrfs: the tree maps full paths to directory metadata, file metadata, and small file contents, and full path plus offset to large file contents, stored externally (likely in a simple integer-indexed store.)

However, it's not quite a standard b-epsilon tree; it's a safe copy-on-write-ish version.  Child nodes are referred to not only by id/location, but also by how much of their buffer to use.  This gives nice pseudo-CoW semantics, where appending to a nodes' buffer doesn't automatically update all pointers to it.    A node is never rewritten; flushing creates a copy of the node.  Thus, a node's children will never change; a node plus a prefix of its buffer is constant.  This allows for sharing of nodes between multiple commits as well as within "snapshots".

The Tree is a tree of commits, like git or mercurial, though it will typically be very (or completely) linear.  This does, however, allow for safe concurrent writes from multiple independent writers (using the CoW-ish semantics), though efficiency may suffer, and this complicates management.  Each commit can have a "branch" name (using mercurial teminology, not git); this would likely indicate a unique client name that wrote it.  As long as each "branch" is only writable exclusively, each branch will have a single head, and can be progressed forward linearly.  Merging is interesting, and currently left to the client, (though "unioning" might make this automatic?)

Each child reference may also have a "prefix" associated with it.  E.g., if a every entry in a node is under the directory /path/to/directory, its parent can note that in its link, and the prefix can be stripped from entries.  Besides saving space, this allows for efficient sharing of directories at multiple points in the filesystem by sharing node references (assuming the node is fully flushed).  (Note that this is CoW semantics, not hard-links; due to the general CoW-ish semantics of the tree, updates to one reference will be handled by buffers above the node, after the reference, or in a copied node.)

Note also that these shared references complicate buffer-appends; if two parents try to flush to the same child, the buffer would be a mix of both.  So on flushes, we make sure that the length of the child's buffer is identical to the length of the parent's pointer; if it isn't, we copy the node, doing an explicit CoW.  Note that in non-exclusive scenarios, we also have to copy the node if we are not the nodes' owner (hopefully to be discussed later).

Files
-----
Since nodes are large, it makes sense to store small files directly in them.  Small files, e.g., <4k, can be stored directly in the tree.  Large files can be stored as ranges pointing into a large byte-store, with optional deduping.  File data can be stored as logical keys (file, start, length, pointer); the start and length are logical to the file, and length and pointer are used in the extent store.


Moving files/directories
------------------------
As with Betrfs zones, if a file/directory is stored as a prefix, only that prefix needs to be changed.  Otherwise, all entries need to be moved.  This is tricky.  We can cheat a bit by adding a `mv` message to the buffer, and flushing it down to the least common ancestor of the old and new locations, but eventually we need to move the data to keep any semblence of locality.

Note, though, that a file/dir can only have so many top-level pointers - a directory may have 1M descendents, but with fanout B and height H, there will be at most 2\*(B-1)\*H nodes to move - there are at most B-1 entries for this directoy in the first and last leaf nodes for this directory.  Then all leaves between these two will be entirely this path, so we can move the pointers above them instead of each leaf entry.  In this fashion, we move at most 2\*(B-1) (and B or so in expection) nodes at each level of the tree.

That's actually a slightly lie; since metadata is sorted by path prefix, this applies to each depth of directories underneath the move.  Still, if we assume, say, B=32, 10M entries (so H=5), that's expected 160 entries to move.  By using a pair of messages (rm /path/to/old/directory and addnode /path/to/new/directory nodeid), this should still be just a couple writes.  It's probably a bigger issue to deal with moving or flushing buffers.

And more importantly, the B multiple shouldn't affect actual time, since each seek reads B of those entries.  So really, we're just seaking to each node in the tree, which is 2\*H\*depth of directory.  E.g., the linux-4.15.3 kernel source tree has 123k files and directories, split among depths at roughly 5 70 3100 26000 42000 33000 13000 3000, 2000, 300, 160, 250.  If we assume 32-wide nodes, we approximage as 1 level < 32 taking 5 moves, 4 levels < 1024 taking 60 moves,  5 levels < 32k taking 90 moves, and 2 larger levels taking 100 moves.  That's a total of something like 1000 moves total, which would be 5 seconds on spinning rust.  But with the batching by nodes as noted above, it shoud still be sub-second.  Nice.

Consistency
-----------
Logically, as in Betrfs, the metadata and data trees are separate.  However, since Betrfs does in-place updates, it needs a journal for consistency.  In this case, the journal is effectively the buffers.  But if we have two trees, they can get out of sync.  So we combine them into a single logical tree with two types of messages - data and metadata - sorted such that they are separate.  (Or we could put them together, but that would likely hurt performance.)

To allow "transactions", we could add a field to the buffer allowing writes to be grouped together - in the simplest case, 1/3, 2/3, 3/3; if the transaction isn't all there, it won't be counted.  This might be useful, though it's not really exposed to the user in the VFS, and it's not clear if it's required for internal use (e.g., just write the data before the metadata; if the metadata write fails, the data will be GC'd eventually.)

References
----------
For large directory copies (e.g., snapshots), it's convenient to be able to point a directory at another one.  If a directory is its own fully-flusehd zone, this is trivial; we just point at the node, and CoW semantics Just Work.  Otherwise, we can point at the path in the current commit, this adds a layer of indirection, but is extremely cheap.  As changes mount up, the buffer in the reference node fills, and we eventually flush it, creating a new CoW node.

So reads to this reference require walking the tree down to the reference, then walking again from the appropriate commit to the referenced directory.  Chains of references could get expensive; a standard snapshot-based backup would need to be careful which direction the snapshots go.  There should be an action to explicitly delink this; it would cost some space but potentially dramatically improve performance.

Hard links
----------
Sadly, there doesn't seem to be a way to do hard links reasonably due to CoW.  So add a layer of indirection; use something like references, but that always points to the newest version.  Something like .hardlinks/[random-id].  This could also have interesting uses for a unionfs-like system, which may also make merging multiple heads somewhat-free...

Note that writes have to go to the .hardlinks directory to avoid writes being buffered on different paths to links.  This is fine, because opening the file will require it to be noted as a hardlink, so we can save that in the file handle.  However, it does mean that adding a hard link requires flushing to it.


Eventual-consistency
====================
One goal of becfs is to be usable on eventually-consistent stores like S3, as well as local disks.  Of course, it can't guarantee data is available immediately, but it can guarantee that data written will be available eventually without loss or corruption.  This also permits multiple simultaneous writers (within reason) to the same file store.

This is the reason for the multiple heads; if a head is not yet written (or two heads are being written simultaneously)

Note: outoforderfs (github/vi/outoforderfs) could be fun for testing this.
