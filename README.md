# directory-contents

Recursively list the contents of a directory while avoiding symlink loops.

## Description

Modeled after the linux @tree@ command (when invoked with the follow-symlinks
option), this module recursively lists the contents of a directory while
avoiding symlink loops. See the documentation of `buildPath` for an example.

In addition to building the directory-contents tree, this module provides
facilities for filtering, displaying, and navigating the directory hierarchy.
