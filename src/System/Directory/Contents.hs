{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveGeneric #-}
{-# language DeriveTraversable #-}
{-# language LambdaCase #-}
{-# language MultiWayIf #-}
{-|
Description:
  Recursively list the contents of a directory while avoiding
  symlink loops.

Modeled after the linux @tree@ command (when invoked with the follow-symlinks
option), this module recursively lists the contents of a directory while
avoiding symlink loops. See the documentation of 'buildPath' for an example.

In addition to building the directory-contents tree, this module provides
facilities for filtering, displaying, and navigating the directory hierarchy.

-}
module System.Directory.Contents where

import Control.Applicative
import Control.Monad
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree as DataTree
import Data.Witherable
import GHC.Generics
import System.Directory
import System.FilePath

-- | The contents of a directory, represented as a tree. See 'Symlink' for
-- special handling of symlinks.
data Path a = Path_Dir FilePath [Path a]
            | Path_File FilePath a
            | Path_Symlink FilePath (Symlink a)
  deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable, Generic)

-- | Symlink cycles are prevented by separating symlinks into two categories:
-- those that point to paths already within the directory hierarchy being
-- recursively listed, and those that are not. In the former case, rather than
-- following the symlink and listing the target redundantly, we simply store
-- the symlink reference itself. In the latter case, we treat the symlink as we
-- would any other folder and produce a list of its contents.
--
-- The 'String' argument represents the symlink reference (e.g., "../somefile").
data Symlink a = Symlink_Internal String
               | Symlink_External String [Path a]
  deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable, Generic)

-- * Constructing a tree

-- | Recursively list the contents of a 'FilePath', representing the results as
-- a 'Path' tree. This function should produce results similar to the linux command
-- @tree -l@.
--
-- For example, given this directory and symlink structure (as shown by @tree -l@):
--
-- > test
-- > ├── A
-- > │   ├── a
-- > │   ├── A -> ../A  [recursive, not followed]
-- > │   └── B -> ../B
-- > │       ├── A -> ../A  [recursive, not followed]
-- > │       └── b
-- > ├── B
-- > │   ├── A -> ../A  [recursive, not followed]
-- > │   └── b
-- > └── C -> ../C
-- >     └── c
--
-- this function will produce the following (as displayed by 'printPath'):
--
-- > test
-- > |
-- > +- A
-- > |  |
-- > |  +- A -> ../A
-- > |  |
-- > |  +- B -> ../B
-- > |  |
-- > |  `- a
-- > |
-- > +- B
-- > |  |
-- > |  +- A -> ../A
-- > |  |
-- > |  `- b
-- > |
-- > `- C -> ../C
-- >    |
-- >    `- c
--
buildPath :: FilePath -> IO (Maybe (Path FilePath))
buildPath = build Map.empty
  where
    build seen path = do
      canon <- canonicalizePath path
      isPath <- doesPathExist path
      isDir <- doesDirectoryExist path
      isSym <- pathIsSymbolicLink path
      subpaths <- if isDir then listDirectory path else pure []
      subcanons <- mapM canonicalizePath <=< filterM (fmap not . pathIsSymbolicLink) $
        (path </>) <$> subpaths
      let seen' = Map.union seen $ Map.fromList $ zip subcanons subpaths
      if | not isPath -> pure Nothing
         | isSym -> do
            case Map.lookup canon seen' of
              Nothing -> do
                  a <- build (Map.insert canon path seen') canon
                  s <- getSymbolicLinkTarget path
                  pure $ Just $ Path_Symlink path $ Symlink_External s $ case a of
                    Nothing -> []
                    Just (Path_Dir _ ps) -> ps
                    Just x -> [x]
              Just _ -> do
                s <- getSymbolicLinkTarget path
                pure $ Just $ Path_Symlink path (Symlink_Internal s)
         | isDir -> do
            Just . Path_Dir path . catMaybes <$>
              mapM (build (Map.insert canon path seen') . (path</>)) subpaths
         | otherwise -> pure $ Just $ Path_File path path

-- * Navigate

-- | Starting from the root directory, try to walk the given filepath and return
-- the 'Path' at the end of the route. For example, given the following tree:
--
-- > src
-- > └── System
-- >     └── Directory
-- >             └── Contents.hs
--
-- @walkPath "src/System"@ should produce
--
-- > Directory
-- > |
-- > `- Contents.hs
--
-- This function does not dereference symlinks, nor does it handle the special
-- paths @.@ and @..@.
walkPath :: FilePath -> Path a -> Maybe (Path a)
walkPath target p =
  let pathSegments = splitDirectories target
      walk :: [FilePath] -> Path a -> Maybe (Path a)
      walk [] path = Just path
      walk (c:gc) path = case path of
        Path_Dir a xs | takeFileName a == c -> foldl (<|>) Nothing $ map (walk gc) xs
        Path_File a f | takeFileName a == c && null gc -> Just $ Path_File a f
        Path_Symlink a (Symlink_Internal s) | takeFileName a == c && null gc ->
          Just $ Path_Symlink a (Symlink_Internal s)
        Path_Symlink a (Symlink_External s xs) | takeFileName a == c -> 
          case foldl (<|>) Nothing (map (walk gc) xs) of
            Nothing -> Nothing
            Just xs' -> Just $ Path_Symlink a $ Symlink_External s [xs']
        _ -> Nothing
  in walk pathSegments p

-- * Filter

-- | This wrapper really just represents the no-path/empty case so that filtering works
newtype WrappedPath a = WrappedPath { unWrappedPath :: Maybe (Path a) }
  deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

instance Filterable WrappedPath where
  catMaybes (WrappedPath Nothing) = WrappedPath Nothing
  catMaybes (WrappedPath (Just x)) = WrappedPath $
    let go :: Path (Maybe a) -> Maybe (Path a)
        go a = case a of
          Path_Dir p xs -> Just $ Path_Dir p $ catMaybes $ go <$> xs
          Path_File p f -> Path_File p <$> f
          Path_Symlink p (Symlink_External s f) -> Just $ Path_Symlink p (Symlink_External s $ mapMaybe go f)
          Path_Symlink p (Symlink_Internal s) -> Just $ Path_Symlink p $ Symlink_Internal s
    in go x

instance Witherable WrappedPath

-- | 'wither' for 'Path'. This represents the case of no paths left after
-- filtering with 'Nothing' (something that the 'Path' type can't represent on
-- its own).
-- NB: Filtering does not remove directories, only files. The directory structure
-- remains intact. To remove empty directories, see 'prunePath'.
witherPath :: Applicative f => (a -> f (Maybe b)) -> Path a -> f (Maybe (Path b))
witherPath f = fmap unWrappedPath . wither f . WrappedPath . Just

-- | 'filterA' for 'Path'. See 'witherPath'.
filterAPath :: Applicative f => (a -> f Bool) -> Path a -> f (Maybe (Path a))
filterAPath f = fmap unWrappedPath . filterA f . WrappedPath . Just

-- | 'mapMaybe' for 'Path'. See 'witherPath'.
mapMaybePath :: (a -> Maybe b) -> Path a -> Maybe (Path b)
mapMaybePath f = unWrappedPath . mapMaybe f . WrappedPath . Just

-- | 'catMaybes' for 'Path'. See 'witherPath'.
catMaybesPath :: Path (Maybe a) -> Maybe (Path a)
catMaybesPath = unWrappedPath . catMaybes . WrappedPath . Just

-- | 'Data.Witherable.filter' for 'Path'. See 'witherPath'.
filterPath :: (a -> Bool) -> Path a -> Maybe (Path a)
filterPath f = unWrappedPath . Data.Witherable.filter f . WrappedPath . Just

-- | Remove empty directories from the 'Path'
prunePath :: Path a -> Maybe (Path a)
prunePath = \case
  Path_Dir a xs -> sub (Path_Dir a) xs
  Path_File a f -> Just $ Path_File a f
  Path_Symlink a (Symlink_External s xs) -> sub (Path_Symlink a . Symlink_External s) xs
  Path_Symlink a s -> Just $ Path_Symlink a s
  where
    sub c xs = case mapMaybe prunePath xs of
      [] -> Nothing
      ys -> Just $ c ys

-- * Display

-- | Produces a tree drawing (using only text) of a 'Path' hierarchy.
drawPath :: Path a -> Text
drawPath = T.pack . DataTree.drawTree . pathToTree
  where
    pathToTree :: Path a -> DataTree.Tree String
    pathToTree = \case
      Path_File p _ -> DataTree.Node (takeFileName p) []
      Path_Dir p ps -> DataTree.Node (takeFileName p) $ pathToTree <$> ps
      Path_Symlink p (Symlink_Internal s) -> DataTree.Node (showSym p s) []
      Path_Symlink p (Symlink_External s xs) -> DataTree.Node (showSym p s) $ pathToTree <$> xs
    showSym p s = takeFileName p <> " -> " <> s

-- | Print the 'Path' as a tree. For example:
--
-- @
--
-- System
-- |
-- `- Directory
--    |
--    `- Contents.hs
--
-- @
printPath :: Path a -> IO ()
printPath = putStrLn . T.unpack . drawPath
