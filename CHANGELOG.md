# Revision history for directory-contents

## 0.2.0.0

* Add `Data.Data` instance for `DirTree` and `Symlink`
* Add `System.Directory.Contents.Zipper` for convenient navigation and modification of directory contents
* Add more examples to readme
* Breaking change: `DirTree_Dir` and `Symlink_External` child nodes are now stored as a `Map`
* Fix: buildDirTree fails as a file gets deleted quickly.

## 0.1.0.0

* Build recursive directory trees and try to avoid symlink cycles. Includes functions to walk a directory tree, filter it, and display it.
