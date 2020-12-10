import Data.Maybe
import System.Directory.Contents
import System.Exit
import System.FilePath

main :: IO ()
main = do
  p <- test
    "Build DirTree in the presence of recursive symlinks"
    (buildDirTree "test/fixtures/test-1")
  test
    "Navigate to a directory that should exist"
    (pure $ walkContents "C" p)
  filtered <- test "Filter contents based on a predicate" $
    pure (filterDirTree ((/="README.md") . takeFileName) p)
  test "Check that filtering didn't prune directories" $
    pure $ walkContents "C" p
  test "Check that pruneDirTree does prune empty directories" $
    pure $ expectNothing $ walkContents "C" =<< pruneDirTree filtered
  rocket <- test "Check that symlink can be traversed" $
    pure (walkContents "D/rocket" filtered)
  let filterRockets = filterDirTree ((/="rocket") . takeFileName)
  test "Check that filtering removes symlinks to filtered files" $
    pure $ expectNothing $
      walkContents "d_rocket" =<< filterRockets p
  test "Check that dereferenced symlinks to filtered files are not removed" $ do
    deref <- dereferenceSymlinks p
    pure $ walkContents "d_rocket" =<< filterRockets deref
  readFile (filePath rocket) >>=
    putStr >> putStrLn " All systems go!"

test :: String -> IO (Maybe a) -> IO a
test describe run = do
  putStr describe
  putStr " ... "
  result <- run
  case result of
    Nothing -> putStrLn "FAILED" >> exitWith (ExitFailure 1)
    Just r -> putStrLn "passed" >> return r

expectNothing :: Maybe a -> Maybe ()
expectNothing x = case x of
  Nothing -> Just ()
  Just _ -> Nothing
