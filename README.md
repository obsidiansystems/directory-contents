directory-contents
==================
[![Haskell](https://img.shields.io/badge/language-Haskell-orange.svg)](https://haskell.org) [![Hackage](https://img.shields.io/hackage/v/directory-contents.svg)](https://hackage.haskell.org/package/directory-contents) [![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/directory-contents/badge)](https://matrix.hackage.haskell.org/#/package/directory-contents) [![Github CI](https://github.com/obsidiansystems/directory-contents/workflows/github-action/badge.svg)](https://github.com/obsidiansystems/directory-contents/actions) [![BSD3 License](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/obsidiansystems/directory-contents/blob/master/LICENSE)

Recursively list the contents of a directory while avoiding symlink loops.

Description
-----------

Modeled after the linux `tree` command (when invoked with the follow-symlinks
option), this module recursively lists the contents of a directory while
avoiding symlink loops. In particular, `tree -l` and `buildDirTree` should
provide the same result. See the documentation of `buildDirTree` for an
example.

In addition to building the directory-contents tree, this module provides
facilities for filtering, displaying, and navigating the directory hierarchy.

Example
-------

```haskell

>
> import Data.Foldable as F
> import Data.List
> import qualified Data.Text as T
> import System.Directory.Contents
> import System.Directory.Contents.Zipper
> import System.FilePath
>
> main :: IO ()
> main = do

```
Building a directory tree is easy. Just call `buildDirTree` on a path of your
choice. It'll recursively enumerate the contents of the directories. If it
encounters symlinks, it'll follow those symlinks if it hasn't yet encountered
the target of the symlink. If it has, it'll store a reference to that
already-seen target.

```haskell

>   mp <- buildDirTree "."
>   case mp of
>     Nothing -> putStrLn "Couldn't find that path."
>     Just p -> do

```
Once you've got a `DirTree` you can fmap, traverse, filter, or
[wither](https://hackage.haskell.org/package/witherable-class) it to transform
it however you like.

Note that the filtering operations generally do not remove empty directories.
You have to call `pruneDirTree` to do that.

```haskell

>       let f = pruneDirTree =<< filterDirTree ((`elem` [".hs", ".lhs"]) . takeExtension) p
>       putStrLn $ case f of
>         Nothing -> "No haskell source files found."
>         Just hs -> unlines
>           [ "Paths that contain haskell source files:"
>           , T.unpack $ drawDirTree hs
>           , ""
>           , "Haskell source files:"
>           , intercalate ", " $ F.toList hs
>           ]

```

You can also use the provided `DirZipper` to browse your directory hierarchy and make
changes wherever you like.

```haskell

>       let displayFocused =  maybe
>             (putStrLn "Couldn't find navigation target")
>             (printDirTree . focused)
>
>       displayFocused $
>         downTo "Directory" =<< downTo "System" =<< downTo "src" (zipped p)
>
>           followRelative "./src/../src/System/Directory/Contents" (zipped p)
>
>       maybe (putStrLn "Couldn't find navigation target") printDirTree $
>         fmap focused $
>           remove =<< followRelative "./src/System" (zipped p)
>

```
