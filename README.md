Becfs - An Bε-tree Copy-on-write filesystem.
===========================================

This is not a work-in-progress so much as an unfinished prototype from a few years back.  It was originally inspired by the Borg backup system, but intending to make something that could be used directly on S3 or another high-latency backend, it kind of evolved into something that would be potentially very cool if it existed.  I started implementing a Bε-tree in Coq, with the intent to prove it correct so that I could trust it with data, but never finished it.  Current interesting bits:

- design.md: Describes the plan for the project; sadly I only have a partial Bε-tree implemntation and minimal filesystem built on top of it.  (Fun note: I never added this to the git repo.  Whoops.  But the last-edit time on disk is Sep 27 2019, so I would have written this up around the time I was starting on code.)
- thoughts.md: Putting my thoughs for the project into words to help figure it out.  Largely stream-of-consciousness with some organization.
- src/Tree: The coq code for the various approximations to a Bε-tree (along with the code extracted to Haskell): the idea was to build up from a simple list, to a B-Tree, to a Bε-tree, and show that each behaves identically to the simpler version; then proving the list storage correct proves the Bε-tree correct.
- src: The rest of src is Haskell scaffolding to make a filesystem around the various Coq tree implementations.

Potential issue
===============
One reason I abandoned this (aside from getting distracted by life and other projects) is that after working on this for a bit, I got worried about write amplication.  One downside of a Bε-tree is that data is copied multiple times as it gets flushed down the tree, and I convinced myself that it would probably end up being problematic.  I made a brief attempt to try to simulate this, but didn't get too far before getting distracted.
