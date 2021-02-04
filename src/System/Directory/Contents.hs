{-# Language DeriveFoldable #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveTraversable #-}
{-# Language FlexibleContexts #-}
{-# Language LambdaCase #-}
{-# Language MultiWayIf #-}
{-|
Description:
  Recursively list the contents of a directory while avoiding
  symlink loops.

Modeled after the linux @tree@ command (when invoked with the follow-symlinks
option), this module recursively lists the contents of a directory while
avoiding symlink loops. See the documentation of 'buildDirTree' for an example.

In addition to building the directory-contents tree, this module provides
facilities for filtering, displaying, and navigating the directory hierarchy.

See 'System.Directory.Contents.Zipper.DirZipper' for zipper-based navigation.

-}
module System.Directory.Contents 
  (
  -- * Directory hierarchy tree
    DirTree(..)
  , Symlink(..)
  , FileName
  -- ** Constructing directory trees
  , buildDirTree
  , dereferenceSymlinks
  -- ** Lower level tree construction
  -- *** Extracting basic file information
  , filePath
  , fileName
  -- *** Building and manipulating a map of sibling files
  , fileNameMap
  , insertSibling
  , removeSibling
  , withFirstChild
  -- * Basic directory tree navigation
  , walkDirTree
  , walkContents
  -- * Filtering a directory tree
  , pruneDirTree
  , DirTreeMaybe(..)
  , withDirTreeMaybe
  , withDirTreeMaybeF
  , witherDirTree
  , filterADirTree
  , mapMaybeDirTree
  , catMaybesDirTree
  , filterDirTree
  -- * Displaying a directory tree
  , drawDirTree
  , drawDirTreeWith
  , printDirTree
  -- * Miscellaneous
  , mkRelative
  , alternative
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Writer
import Data.List
import qualified Data.Map as Map
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree as DataTree
import Data.Witherable
import System.Directory
import System.FilePath

import System.Directory.Contents.Types
import System.Directory.Contents.Zipper

-- * Construct

-- | Recursively list the contents of a 'FilePath', representing the results as
-- a hierarchical 'DirTree'. This function should produce results similar to
-- the linux command @tree -l@.
--
-- For example, given this directory and symlink structure
-- (as shown by @tree -l@):
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
-- this function will produce the following (as rendered by 'drawDirTree'):
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
buildDirTree :: FilePath -> IO (Maybe (DirTree FilePath))
buildDirTree root = build Map.empty root
  where
    build seen path = do
      canon <- canonicalizePath path
      isPath <- doesPathExist path
      isDir <- doesDirectoryExist path
      let pathExists = isPath || isDir
      isSym <- if pathExists then fmap Just (pathIsSymbolicLink path) else pure Nothing
      subpaths <- if isDir then listDirectory path else pure []
      subcanons <- mapM canonicalizePath <=<
        filterM (fmap not . pathIsSymbolicLink) $ (path </>) <$> subpaths
      let seen' = Map.union seen $ Map.fromList $ zip subcanons subpaths
          buildSubpaths = catMaybes <$> mapM
            (build (Map.insert canon path seen') . (path </>)) subpaths
      if | not isPath -> pure Nothing
         | isSym == Just True -> case Map.lookup canon seen' of
             Nothing -> do
               s <- getSymbolicLinkTarget path
               Just . DirTree_Symlink path . Symlink_External s . fileNameMap <$> buildSubpaths
             Just _ -> do
               target <- getSymbolicLinkTarget path
               canonRoot <- canonicalizePath root
               let startingPoint = takeFileName root
               canonSym <- canonicalizePath $ takeDirectory path </> target
               pure $ Just $ DirTree_Symlink path $ Symlink_Internal target $
                startingPoint </> mkRelative canonRoot canonSym
         | isDir -> Just . DirTree_Dir path . fileNameMap <$> buildSubpaths
         | otherwise -> pure $ Just $ DirTree_File path path

-- | De-reference one layer of symlinks
{- |
==== __Example__

Given:

> tmp
> |
> +- A
> |  |
> |  `- a
> |
> +- a -> A/a
> |
> `- C
>    |
>    `- A -> ../A

This function will follow one level of symlinks, producing:

> tmp
> |
> +- A
> |  |
> |  `- a
> |
> +- a
> |
> `- C
>    |
>    `- A
>       |
>       `- a

-}
dereferenceSymlinks :: DirTree FilePath -> IO (DirTree FilePath)
dereferenceSymlinks toppath = deref toppath toppath
  where
    deref top cur = case cur of
      DirTree_Dir p xs -> DirTree_Dir p <$> mapM (deref top) xs
      DirTree_File p x -> pure $ DirTree_File p x
      DirTree_Symlink p sym -> case sym of
        Symlink_External _ paths ->
          if Map.null paths
            then do
              isDir <- doesDirectoryExist p
              pure $ if isDir
                then DirTree_Dir p Map.empty
                else DirTree_File p p
            else pure $ DirTree_Dir p paths
        Symlink_Internal _ r -> do
          let startingPoint = takeFileName $ filePath top
          let target = walkDirTree (startingPoint </> r) top
          pure $ case target of
            Nothing -> DirTree_Symlink p sym
            Just t -> t

-- * Navigate
-- | Starting from the root directory, try to walk the given filepath and return
-- the 'DirTree' at the end of the route. For example, given the following tree:
--
-- > src
-- > └── System
-- >     └── Directory
-- >             └── Contents.hs
--
-- @walkDirTree "src/System"@ should produce
--
-- > Directory
-- > |
-- > `- Contents.hs
--
-- This function does not dereference symlinks, nor does it handle the special
-- paths @.@ and @..@. For more advanced navigation, including handling of special
-- paths, see 'System.Directory.Contents.Zipper.DirZipper'.
walkDirTree :: FilePath -> DirTree a -> Maybe (DirTree a)
walkDirTree target p =
  let pathSegments = splitDirectories target
      walk :: [FilePath] -> DirTree a -> Maybe (DirTree a)
      walk [] path = Just path
      walk (c : gc) path = case path of
        DirTree_Dir a xs
          | takeFileName a == c -> alternative $ walk gc <$> Map.elems xs
        DirTree_File a f
          | takeFileName a == c && null gc -> Just $ DirTree_File a f
        DirTree_Symlink a (Symlink_Internal s t)
          | takeFileName a == c && null gc -> Just $ DirTree_Symlink a
            (Symlink_Internal s t)
        DirTree_Symlink a (Symlink_External _ xs)
          | takeFileName a == c -> alternative $ walk gc <$> Map.elems xs
        _ -> Nothing
  in walk pathSegments p

-- | Like 'walkDirTree' but skips the outermost containing directory. Useful for
-- walking paths relative from the root directory passed to 'buildDirTree'.
--
-- Given the following 'DirTree':
--
-- > src
-- > └── System
-- >     └── Directory
-- >             └── Contents.hs
--
-- @walkContents "System"@ should produce
--
-- > Directory
-- > |
-- > `- Contents.hs
--
--For more advanced navigation, see
--'System.Directory.Contents.Zipper.DirZipper'.
walkContents :: FilePath -> DirTree a -> Maybe (DirTree a)
walkContents p = fmap focused . followRelative p . zipped

-- * Filter
-- | This wrapper really just represents the no-path/empty case so that
-- filtering works
newtype DirTreeMaybe a = DirTreeMaybe { unDirTreeMaybe :: Maybe (DirTree a) }
  deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

instance Filterable DirTreeMaybe where
  catMaybes (DirTreeMaybe Nothing) = DirTreeMaybe Nothing
  catMaybes (DirTreeMaybe (Just x)) = DirTreeMaybe $ do
    let go :: DirTree (Maybe a) -> Writer (Set FilePath) (Maybe (DirTree a))
        go = \case
          DirTree_Dir p xs -> do
            out <- mapM go xs
            pure $ Just $ DirTree_Dir p $ catMaybes out
          DirTree_File p f -> case f of
            Nothing -> tell (Set.singleton p) >> pure Nothing
            Just f' -> pure $ Just $ DirTree_File p f'
          DirTree_Symlink p (Symlink_External s xs) -> do
            out <- mapM go xs
            pure $ Just $ DirTree_Symlink p (Symlink_External s $ catMaybes out)
          DirTree_Symlink p (Symlink_Internal s r) -> pure $
             Just $ DirTree_Symlink p (Symlink_Internal s r)
        removeStaleSymlinks :: Set FilePath -> DirTree a -> Maybe (DirTree a)
        removeStaleSymlinks xs d = case d of
          DirTree_Symlink p (Symlink_Internal s r) ->
            let startingPoint = case takeDirectory $ filePath x of
                  "." -> ""
                  a -> a
            in
              if (startingPoint </> r) `Set.member` xs
              then Nothing
              else Just $ DirTree_Symlink p (Symlink_Internal s r)
          DirTree_Symlink p (Symlink_External s cs) ->
            if Map.null cs && Set.member (filePath x </> s) xs
            then Nothing
            else Just $ DirTree_Symlink p (Symlink_External s cs)
          DirTree_File p f -> Just $ DirTree_File p f
          DirTree_Dir p fs -> Just $ DirTree_Dir p $
            catMaybes $ removeStaleSymlinks xs <$> fs
    let (out, removals) = runWriter $ go x
    removeStaleSymlinks removals =<< out

instance Witherable DirTreeMaybe

-- | Map a function that could produce an empty result over a 'DirTree'
withDirTreeMaybe
  :: (DirTreeMaybe a -> DirTreeMaybe b)
  -> DirTree a
  -> Maybe (DirTree b)
withDirTreeMaybe f = unDirTreeMaybe . f . DirTreeMaybe . Just

-- | Map a function that could produce an empty result in the given functor
withDirTreeMaybeF
  :: Functor f
  => (DirTreeMaybe a -> f (DirTreeMaybe b))
  -> DirTree a
  -> f (Maybe (DirTree b))
withDirTreeMaybeF f = fmap unDirTreeMaybe . f . DirTreeMaybe . Just

-- | 'wither' for 'DirTree'. This represents the case of no paths left after
-- filtering with 'Nothing' (something that the 'DirTree' type can't represent on
-- its own).  NB: Filtering does not remove directories, only files. The
-- directory structure remains intact. To remove empty directories, see
-- 'pruneDirTree'.
witherDirTree
  :: Applicative f
  => (a -> f (Maybe b))
  -> DirTree a
  -> f (Maybe (DirTree b))
witherDirTree = withDirTreeMaybeF . wither

-- | 'filterA' for 'DirTree'. See 'witherDirTree'.
filterADirTree
  :: Applicative f
  => (a -> f Bool)
  -> DirTree a
  -> f (Maybe (DirTree a))
filterADirTree = withDirTreeMaybeF . filterA

-- | 'mapMaybe' for 'DirTree'. See 'witherDirTree'.
mapMaybeDirTree :: (a -> Maybe b) -> DirTree a -> Maybe (DirTree b)
mapMaybeDirTree = withDirTreeMaybe . mapMaybe

-- | 'catMaybes' for 'DirTree'. See 'witherDirTree'.
catMaybesDirTree :: DirTree (Maybe a) -> Maybe (DirTree a)
catMaybesDirTree = withDirTreeMaybe catMaybes

-- | 'Data.Witherable.filter' for 'DirTree'. See 'witherDirTree'.
filterDirTree :: (a -> Bool) -> DirTree a -> Maybe (DirTree a)
filterDirTree = withDirTreeMaybe . Data.Witherable.filter

-- | Remove empty directories from the 'DirTree'
pruneDirTree :: DirTree a -> Maybe (DirTree a)
pruneDirTree = \case
  DirTree_Dir a xs ->
    sub (DirTree_Dir a) xs
  DirTree_File a f ->
    Just $ DirTree_File a f
  DirTree_Symlink a (Symlink_External s xs) ->
    sub (DirTree_Symlink a . Symlink_External s) xs
  DirTree_Symlink a (Symlink_Internal s t) ->
    Just $ DirTree_Symlink a (Symlink_Internal s t)
  where
    sub c xs =
      let ys = mapMaybe pruneDirTree xs
      in if Map.null ys then Nothing else Just $ c ys

-- * Display
-- | Produces a tree drawing (using only text) of a 'DirTree' hierarchy.
drawDirTree :: DirTree a -> Text
drawDirTree = T.pack . drawDirTreeWith const

-- | Apply a rendering function to each file when drawing the directory hierarchy
drawDirTreeWith :: (String -> a -> String) -> DirTree a -> String
drawDirTreeWith f = DataTree.drawTree . pathToTree
  where
    pathToTree = \case
      DirTree_File p a ->
        DataTree.Node (f (takeFileName p) a) []
      DirTree_Dir p ps ->
        DataTree.Node (takeFileName p) $ pathToTree <$> Map.elems ps
      DirTree_Symlink p (Symlink_Internal s _) ->
        DataTree.Node (showSym p s) []
      DirTree_Symlink p (Symlink_External s xs) ->
        DataTree.Node (showSym p s) $ pathToTree <$> Map.elems xs
    showSym p s = takeFileName p <> " -> " <> s

-- | Print the 'DirTree' as a tree. For example:
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
printDirTree :: DirTree a -> IO ()
printDirTree = putStrLn . T.unpack . drawDirTree

-- * Utilities

-- | Make one filepath relative to another
mkRelative :: FilePath -> FilePath -> FilePath
mkRelative root fp = case stripPrefix (dropTrailingPathSeparator root) fp of
  Nothing -> []
  Just r ->
    -- Remove the leading slash - we know it'll be there because
    -- we removed the trailing slash (if it was there) from the root
    drop 1 r

-- | Get the first 'Alternative'
alternative :: Alternative f => [f a] -> f a
alternative = getAlt . mconcat . fmap Alt
