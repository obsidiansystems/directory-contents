{-# language DeriveDataTypeable #-}
{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveGeneric #-}
{-# language DeriveTraversable #-}
{-# language LambdaCase #-}
{-|
Description:
  Contains the tree data structure used to store directory hierarchies
-}
module System.Directory.Contents.Types where

import Data.Data
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics
import System.FilePath

-- | The contents of a directory, represented as a tree. See 'Symlink' for
-- special handling of symlinks.
data DirTree a
  = DirTree_Dir FilePath (Map FileName (DirTree a))
  | DirTree_File FilePath a
  | DirTree_Symlink FilePath (Symlink a)
  deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable, Generic, Data)

-- | Symlink cycles are prevented by separating symlinks into two categories:
-- those that point to paths already within the directory hierarchy being
-- recursively listed, and those that are not. In the former case, rather than
-- following the symlink and listing the target redundantly, we simply store
-- the symlink reference itself. In the latter case, we treat the symlink as we
-- would any other folder and produce a list of its contents.
--
-- The 'String' argument represents the symlink reference (e.g., "../somefile").
-- In the 'Symlink_Internal' case, the second ('FilePath') argument is the path
-- to the symlink target.
-- In the 'Symlink_External' case, the second (@[DirTree a]@) argument contains
-- the contents of the symlink target.
data Symlink a
  = Symlink_Internal String FilePath
  | Symlink_External String (Map FileName (DirTree a))
  deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable, Generic, Data)

-- * Utilities

-- | Extract the 'FilePath' from a 'DirTree' node
filePath :: DirTree a -> FilePath
filePath = \case
  DirTree_Dir f _ -> f
  DirTree_File f _ -> f
  DirTree_Symlink f _ -> f

-- | File names, as opposed to file paths, are used to uniquely identify
-- siblings at each level
type FileName = String

-- | Generate the key used to identify siblings
fileName :: DirTree a -> FileName
fileName = takeFileName . filePath

-- | Construct a map of files indexed by filename. Should only be used for a
-- particular generation or level in the directory hierarchy (since that's the
-- only time we can be sure that names are unique)
fileNameMap :: [DirTree a] -> Map FileName (DirTree a)
fileNameMap xs = Map.fromList $ zip (fileName <$> xs) xs

-- | Add a sibling to a map of files
insertSibling :: DirTree a -> Map FileName (DirTree a) -> Map FileName (DirTree a)
insertSibling a = Map.insert (fileName a) a

-- | Remove sibling from a map of files
removeSibling :: DirTree a -> Map FileName (DirTree a) -> Map FileName (DirTree a)
removeSibling a = Map.delete (fileName a)

-- | Map a function over the first child and the rest of the children
withFirstChild
  :: Map FileName (DirTree a)
  -> (DirTree a -> Map FileName (DirTree a) -> x)
  -> Maybe x
withFirstChild m f = case Map.minView m of
  Nothing -> Nothing
  Just (firstChild, children) -> Just $ f firstChild children
