{-# Language LambdaCase #-}
{-|
Description:
  Cursor-based navigation and modification of 'DirTree's.

This module should be imported qualified due to the very short names it exports.
-}
module System.Directory.Contents.Zipper where

import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as Map
import System.FilePath

import System.Directory.Contents.Types

-- * Zipper

-- | A zipper for a 'DirTree'. As you navigate the tree, this keeps track of where
-- you are (which node is "focused" under your cursor) and how to reach parent, child,
-- and sibling nodes.
data DirZipper a = DirZipper
  { _dirZipper_cursor :: DirTree a -- ^ Cursor position
  , _dirZipper_siblings :: Map FilePath (DirTree a) -- ^ Siblings
  , _dirZipper_elders :: [(DirTree a, Map FilePath (DirTree a))]
  -- ^ Parents and aunts/uncles, in reverse order (i.e., immediate ancestors first)
  }
  deriving (Show, Read, Eq, Ord)

-- | Construct a zipper out of a 'DirTree'. Use 'focused' or 'unzipped' to get back
-- a 'DirTree'
zipped :: DirTree a -> DirZipper a
zipped a = DirZipper a Map.empty []

-- | The currently focused/selected node (and its children).  In other words,
-- where you are in the directory hierarchy.
focused :: DirZipper a -> DirTree a
focused = _dirZipper_cursor

-- | Throws away your current cursor information and returns the entire 'DirTree'
-- contained by the 'DirZipper'.
--
-- > unzipped . zipped == id
--
unzipped :: DirZipper a -> DirTree a
unzipped = focused . home

-- | Move down a level in the directory hierarchy. To move down to a specific child,
-- use 'downTo'.
down :: DirZipper a -> Maybe (DirZipper a)
down dz = case dz of
  DirZipper p@(DirTree_Dir _ xs) siblings parents ->
    withFirstChild xs $ \firstChild children ->
      DirZipper firstChild children $ (p, siblings) : parents
  DirZipper p@(DirTree_Symlink _ (Symlink_External _ xs)) siblings parents ->
    withFirstChild xs $ \firstChild children ->
      DirZipper firstChild children $ (p, siblings) : parents
  DirZipper (DirTree_Symlink _ (Symlink_Internal _ ref)) _ _ ->
    followRelative ref $ home dz
  _ -> Nothing

-- | Move up a level in the directory hierarchy, back to the parent that you
-- previously moved 'down' through.
up :: DirZipper a -> Maybe (DirZipper a)
up = \case
  DirZipper c s ((parent, uncles):ps) ->
    Just $ DirZipper (update c s parent) uncles ps
  _ -> Nothing
  where
    update :: DirTree a -> Map FilePath (DirTree a) -> DirTree a -> DirTree a
    update child siblings parent = case parent of
      DirTree_Dir f _ -> DirTree_Dir f $ insertSibling child siblings
      DirTree_Symlink f (Symlink_External s _) ->
        DirTree_Symlink f $ Symlink_External s $ insertSibling child siblings
      _ -> parent

-- | Go to the top of the directory hierarchy.
home :: DirZipper a -> DirZipper a
home dz =
  let upmost z = maybe z upmost $ up z
  in upmost dz

-- | Navigation directions for sibling nodes
data NavSibling = NavLeft | NavRight

-- | Move to the sibling next to the focused node
nextSibling :: NavSibling -> DirZipper a -> Maybe (DirZipper a)
nextSibling nav (DirZipper cursor siblings parents) =
  let kids = insertSibling cursor siblings
      next = case nav of
        NavRight -> Map.lookupGT (fileName cursor) kids
        NavLeft -> Map.lookupLT (fileName cursor) kids
  in case next of
      Nothing -> Nothing
      Just (_, sibling) -> Just $
        DirZipper sibling (removeSibling sibling kids) parents

-- | Move to the sibling to the left of the focused node
left :: DirZipper a -> Maybe (DirZipper a)
left = nextSibling NavLeft

-- | Move to the sibling to the right of the focused node
right :: DirZipper a -> Maybe (DirZipper a)
right = nextSibling NavRight

-- | Go to a particular sibling
toSibling :: FileName -> DirZipper a -> Maybe (DirZipper a)
toSibling name (DirZipper cursor siblings parents) =
  case Map.lookup name siblings of
    Nothing -> Nothing
    Just sibling ->
      let otherSiblings = insertSibling cursor $
            removeSibling sibling siblings
      in Just $ DirZipper sibling otherSiblings parents

-- | Move down in the directory hierarchy to a particular child
downTo :: FileName -> DirZipper a -> Maybe (DirZipper a)
downTo name z = do
  d <- down z
  if fileName (focused d) == name
    then pure d
    else toSibling name d

-- | Modify the focused node
mapCursor
  :: (DirTree a -> DirTree a)
  -> DirZipper a
  -> DirZipper a
mapCursor f (DirZipper cursor siblings parents) =
  DirZipper (f cursor) siblings parents

-- | Replace the focused node
replaceCursor
  :: DirTree a
  -> DirZipper a
  -> DirZipper a
replaceCursor = mapCursor . const

-- | Add a new sibling to the focused node's generation and focus on it
insert
  :: DirTree a
  -> DirZipper a
  -> DirZipper a
insert d (DirZipper cursor siblings parents) =
  DirZipper
    d
    (insertSibling cursor siblings)
    parents

-- | Remove the focused node
remove
  :: DirZipper a
  -> Maybe (DirZipper a)
remove z@(DirZipper cursor _ _) =
  let rm (DirZipper c s p) =
        DirZipper c (removeSibling cursor s) p
  in case rm <$> (left z <|> right z) of
    Just s -> Just s
    Nothing -> case up z of
      Nothing -> Nothing
      Just dz -> Just $ flip replaceCursor dz $
        case _dirZipper_cursor dz of
          DirTree_Dir f _ -> DirTree_Dir f Map.empty
          DirTree_Symlink f (Symlink_External s _) ->
            DirTree_Symlink f (Symlink_External s Map.empty)
          x -> x

-- | Try to navigate the provided (possibly relative) path.
followRelative
  :: FilePath
  -> DirZipper a
  -> Maybe (DirZipper a)
followRelative path dz =
  let follow r z = case r of
        "." -> Just z
        ".." -> up z
        _ -> downTo r z <|> toSibling r z
      go rs z = case rs of
        [] -> Just z
        (r:more) -> go more =<< follow r z
  in go (splitDirectories path) dz

-- | If the focused node is an internal symlink (see 'Symlink'), try to get
-- to the target.
followLink
  :: DirZipper a
  -> Maybe (DirZipper a)
followLink z = case z of
  DirZipper (DirTree_Symlink _ (Symlink_Internal s _)) _ _ -> followRelative s z
  _ -> Nothing
