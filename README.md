directory-contents
==================
[![Haskell](https://img.shields.io/badge/language-Haskell-orange.svg)](https://haskell.org) [![Hackage](https://img.shields.io/hackage/v/directory-contents.svg)](https://hackage.haskell.org/package/directory-contents) [![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/directory-contents/badge)](https://matrix.hackage.haskell.org/#/package/directory-contents) [![Github CI](https://github.com/obsidiansystems/directory-contents/workflows/github-action/badge.svg)](https://github.com/obsidiansystems/directory-contents/actions) [![BSD3 License](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/obsidiansystems/directory-contents/blob/master/LICENSE)

Recursively list the contents of a directory while avoiding symlink loops.

Description
-----------

Modeled after the linux `tree` command (when invoked with the follow-symlinks
option), this module recursively lists the contents of a directory while
avoiding symlink loops. See the documentation of `buildPath` for an example.

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
> import System.FilePath
>
> main :: IO ()
> main = do
>   mp <- buildPath "."
>   case mp of
>     Nothing -> putStrLn "Couldn't find that path."
>     Just p -> do
>       let f = prunePath =<< filterPath ((`elem` [".hs", ".lhs"]) . takeExtension) p
>       putStrLn $ case f of
>         Nothing -> "No haskell source files found."
>         Just hs -> unlines
>           [ "Paths that contain haskell source files:"
>           , T.unpack $ drawPath hs
>           , ""
>           , "Haskell source files:"
>           , intercalate ", " $ F.toList hs
>           ]
>

```
