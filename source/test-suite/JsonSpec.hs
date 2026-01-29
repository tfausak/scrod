{-# LANGUAGE OverloadedStrings #-}

module JsonSpec
  ( -- * Test building
    buildJsonSpec,

    -- * Test discovery
    discoverTests,
    TestCase (..),
    TestGroup (..),
  )
where

import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified GHC.Stack as Stack
import Heck (Test, describe, it)
import qualified HeckHelpers
import qualified Scrod.Unstable.Main as Main
import qualified Scrod.Unstable.Type.Interface as Interface
import qualified Scrod.Unstable.Type.Json as Json
import qualified Scrod.Unstable.Type.Pointer as Pointer
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

-- | A single test case loaded from a JSON file.
data TestCase = MkTestCase
  { input :: Text.Text,
    assertions :: Map.Map Pointer.Pointer Json.Json,
    expectError :: Bool
  }
  deriving (Eq, Ord, Show)

-- | A group of tests, possibly with nested subgroups.
-- Maps to a describe block in the test output.
data TestGroup = MkTestGroup
  { name :: FilePath,
    tests :: [(FilePath, TestCase)],
    subgroups :: [TestGroup]
  }
  deriving (Eq, Ord, Show)

-- | Parse a test case from JSON.
-- Input is expected to be an array of strings (joined with newlines).
parseTestCase :: Json.Json -> Either String TestCase
parseTestCase json = case json of
  Json.Object m -> do
    inp <- case Map.lookup (Text.pack "input") m of
      Just (Json.Array xs) -> do
        strs <- traverse expectString xs
        Right $ Text.intercalate (Text.pack "\n") strs
      Just _ -> Left "input must be an array of strings"
      Nothing -> Left "missing input field"
    let err = case Map.lookup (Text.pack "error") m of
          Just (Json.Boolean b) -> b
          _ -> False
    asserts <- case Map.lookup (Text.pack "assertions") m of
      Just (Json.Object a) -> parseAssertions a
      Just _ -> Left "assertions must be an object"
      Nothing -> Right Map.empty
    Right MkTestCase {input = inp, assertions = asserts, expectError = err}
  _ -> Left "test case must be an object"

-- | Extract a string from a JSON value.
expectString :: Json.Json -> Either String Text.Text
expectString (Json.String s) = Right s
expectString _ = Left "expected string"

-- | Parse the assertions object.
parseAssertions :: Map.Map Text.Text Json.Json -> Either String (Map.Map Pointer.Pointer Json.Json)
parseAssertions m = do
  pairs <- traverse parseAssertion $ Map.toList m
  Right $ Map.fromList pairs

-- | Parse a single assertion (pointer -> expected value).
parseAssertion :: (Text.Text, Json.Json) -> Either String (Pointer.Pointer, Json.Json)
parseAssertion (key, value) = case Pointer.parse $ Text.unpack key of
  Just ptr -> Right (ptr, value)
  Nothing -> Left $ "invalid JSON pointer: " <> Text.unpack key

-- | Build the JSON test spec from discovered test groups.
buildJsonSpec ::
  (Stack.HasCallStack, Monad n) =>
  Test IO n ->
  [TestGroup] ->
  n ()
buildJsonSpec t = mapM_ (runTestGroup t)

-- | Run a test group, recursively handling subgroups.
runTestGroup ::
  (Stack.HasCallStack, Monad n) =>
  Test IO n ->
  TestGroup ->
  n ()
runTestGroup t group =
  describe t (name group) $ do
    mapM_ (runTestGroup t) (subgroups group)
    mapM_ (runTestCase t) (tests group)

-- | Run a single test case.
runTestCase ::
  (Stack.HasCallStack) =>
  Test IO n ->
  (FilePath, TestCase) ->
  n ()
runTestCase t (filePath, tc) =
  it t (filePathToTestName filePath) $ do
    let inputStr = Text.unpack $ input tc
    if expectError tc
      then HeckHelpers.assertSatisfies t Either.isLeft $ Main.extract inputStr
      else do
        interface <- HeckHelpers.expectRight t $ Main.extract inputStr
        let actualJson = Interface.toJson interface
        mapM_ (checkAssertion t actualJson) $ Map.toList (assertions tc)

-- | Check a single assertion.
checkAssertion ::
  (Stack.HasCallStack, Applicative m) =>
  Test m n ->
  Json.Json ->
  (Pointer.Pointer, Json.Json) ->
  m ()
checkAssertion t actualJson (ptr, expected) = do
  let actual = Pointer.evaluate ptr actualJson
  HeckHelpers.assertSatisfies t (\a -> a == Just expected) actual

-- | Convert a file path to a test name.
-- "has-no-language-by-default.json" -> "has no language by default"
filePathToTestName :: FilePath -> String
filePathToTestName =
  fmap dashToSpace . FilePath.takeBaseName
  where
    dashToSpace '-' = ' '
    dashToSpace c = c

-- | Discover test groups from a directory.
-- Each subdirectory becomes a test group, which can contain
-- both JSON test files and nested subdirectories.
discoverTests :: FilePath -> IO [TestGroup]
discoverTests baseDir = do
  exists <- Directory.doesDirectoryExist baseDir
  if not exists
    then pure []
    else do
      entries <- Directory.listDirectory baseDir
      let dirs = List.sort $ filter (not . List.isPrefixOf ".") entries
      groups <- mapM (loadTestGroup baseDir) dirs
      pure $ filter (not . isEmpty) groups

-- | Check if a test group has no tests and no subgroups.
isEmpty :: TestGroup -> Bool
isEmpty group = null (tests group) && null (subgroups group)

-- | Load a test group from a directory.
-- Recursively loads subdirectories as subgroups.
loadTestGroup :: FilePath -> FilePath -> IO TestGroup
loadTestGroup baseDir dirName = do
  let dirPath = baseDir FilePath.</> dirName
  isDir <- Directory.doesDirectoryExist dirPath
  if not isDir
    then pure $ MkTestGroup dirName [] []
    else do
      entries <- Directory.listDirectory dirPath
      let jsonFiles = List.sort $ filter (\f -> FilePath.takeExtension f == ".json") entries
      let subdirs = List.sort $ filter (not . List.isPrefixOf ".") entries
      testList <- mapM (loadTestFile dirPath) jsonFiles
      subgroupList <- mapM (loadTestGroup dirPath) subdirs
      pure $
        MkTestGroup
          { name = dirName,
            tests = [(f, tc) | (f, Just tc) <- testList],
            subgroups = filter (not . isEmpty) subgroupList
          }

-- | Load a single test file.
loadTestFile :: FilePath -> FilePath -> IO (FilePath, Maybe TestCase)
loadTestFile dirPath fileName = do
  let filePath = dirPath FilePath.</> fileName
  contents <- LazyByteString.readFile filePath
  let textContents = Encoding.decodeUtf8 $ LazyByteString.toStrict contents
  case Json.parse textContents of
    Left err -> do
      putStrLn $ "Warning: failed to parse " <> filePath <> ": " <> show err
      pure (fileName, Nothing)
    Right json -> case parseTestCase json of
      Left err -> do
        putStrLn $ "Warning: invalid test case in " <> filePath <> ": " <> err
        pure (fileName, Nothing)
      Right tc -> pure (fileName, Just tc)
