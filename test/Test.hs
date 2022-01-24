{-# Language QuasiQuotes #-}
import Control.Monad.IO.Class
import Coquina
import Data.Maybe
import Data.String.Here
import Data.Text (Text)
import System.Directory
import System.Directory.Contents
import System.Directory.Contents.Zipper
import System.Exit
import System.FilePath
import System.Process

main :: IO ()
main = do
  _ <- setup test1
  return ()

readme_md = [here|
This test directory tree has the following structure:
```
.
├── a -> A/a
├── A
│   ├── a
│   ├── A -> ../A  [recursive, not followed]
│   └── B -> ../B
│       ├── A -> ../A  [recursive, not followed]
│       └── b
├── B
│   ├── A -> ../A  [recursive, not followed]
│   └── b
└── README.md

6 directories, 5 files
```
|]

-- | Annoying that we have to set it up this way, but there doesn't seem
-- to be any good way to include the symlinks we want as test fixtures
-- in cabal.
--
-- This function will manually recreate the contents of test/fixtures
setup :: (FilePath -> IO ()) -> IO (Text, Text, Either Int ())
setup testRunner = runShell $ do
  inTempDirectory "directory-contents-test" $ \fp -> do
    let fixtures = fp </> "test" </> "fixtures"
        test1 = fixtures </> "test-1"
        test2 = fixtures </> "test-2"
    run $ proc "mkdir" ["-p", fixtures]
    run $ proc "mkdir" ["-p", test1]
    run $ proc "mkdir" ["-p", test2]
    liftIO $ do
      writeFile (fixtures </> "test-1" </> "README.md") readme_md
      writeFile (fixtures </> "test-2" </> "rocket") "🚀"
    run $ proc "mkdir" ["-p", test1 </> "A"]
    run $ proc "touch" [test1 </> "A" </> "a"]
    run $ proc "mkdir" ["-p", test1 </> "B"]
    run $ proc "touch" [test1 </> "B" </> "b"]
    run $ proc "mkdir" ["-p", test1 </> "C"]
    run $ proc "ln" ["-s", "../A", test1 </> "A" </> "A"]
    run $ proc "ln" ["-s", "../B", test1 </> "A" </> "B"]
    run $ proc "ln" ["-s", "../A", test1 </> "B" </> "A"]
    run $ proc "ln" ["-s", "../README.md", test1 </> "C" </> "info.md"]
    run $ proc "ln" ["-s", "../test-2", test1 </> "D"]
    run $ proc "ln" ["-s", "D/rocket", test1 </> "d_rocket"]
    run $ proc "ln" ["-s", "../test-2/rocket", test1 </> "rocket"]
    liftIO $ testRunner test1

test1 :: FilePath -> IO ()
test1 fp = do
  p <- test
    "Build DirTree in the presence of recursive symlinks"
    (buildDirTree fp)
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
  test "Check that filtering removes internal symlinks to filtered files" $
    pure $ expectNothing $
      walkContents "C/info.md" filtered
  let filterRockets = filterDirTree ((/="rocket") . takeFileName)
  test "Check that filtering removes external symlinks to filtered files" $
    pure $ expectNothing $
      walkContents "d_rocket" =<< filterRockets p
  test "Check that dereferenced symlinks to filtered files are not removed" $ do
    deref <- dereferenceSymlinks p
    pure $ walkContents "d_rocket" =<< filterRockets deref
  test "Zipper down then up from root == id" $
    pure $ expectTrue $ fmap focused (up =<< down (zipped p)) == Just p
  test "Use zipper to remove a node" $ do
    let a = up =<< remove =<< downTo "info.md" =<< downTo "C" (zipped p)
    pure $ expectNothing $ walkContents "C/info.md" . focused =<< a
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

expectTrue :: Bool -> Maybe ()
expectTrue x = if x then Just () else Nothing
