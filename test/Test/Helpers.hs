{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Helpers (
    runTest,
    shouldBeSuccess,
    shouldBeSuccessAnd,
    shouldBeErrorAnd,
    testAuthor,
    testConfig,
    withTempDir,
    withTempDir2,
    runSimpleApp,
) where

import Test.Hspec (Expectation, expectationFailure)

import Control.Exception (bracket_, catch)
import Data.Text (Text)
import Knode.App (AppM, defaultEnv, runApp)
import Knode.Data (AppError (..), WikiConfig (..), WikiSource (..))
import Knode.Fake.Data (Author (..))
import Knode.Fake.Monad (FakeM, FakeState, emptyState, runFake)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)

runTest :: FakeM a -> IO (Either AppError a, FakeState)
runTest action = runFake testConfig (emptyState testAuthor) action

shouldBeSuccess :: FakeM a -> Expectation
shouldBeSuccess action = shouldBeSuccessAnd action $ const $ pure ()

shouldBeSuccessAnd :: FakeM a -> (a -> Expectation) -> Expectation
shouldBeSuccessAnd action check = do
    (result, _) <- runTest action
    case result of
        Left err -> expectationFailure $ "Unexpected error: " ++ show err
        Right value -> check value

shouldBeErrorAnd :: FakeM a -> ((AppError, FakeState) -> Expectation) -> Expectation
shouldBeErrorAnd action predicate = do
    (result, finalState) <- runTest action
    case result of
        Left err -> predicate (err, finalState)
        Right _ -> expectationFailure "Expected error but succeeded"

testAuthor :: Author
testAuthor = Author "test@example.com"

testConfig :: WikiConfig
testConfig =
    WikiConfig
        { wikiPath = "/wiki"
        , wikiSource = WikiSource "git@example.com:test/wiki.git"
        , wikiMaxRetries = 3
        }

withTempDir :: FilePath -> (FilePath -> IO ()) -> IO ()
withTempDir dir action = bracket_ (setup dir) (cleanup dir) (action dir)
  where
    setup d = ignoreIOError (removeDirectoryRecursive d) >> createDirectoryIfMissing True d
    cleanup d = ignoreIOError $ removeDirectoryRecursive d
    ignoreIOError :: IO () -> IO ()
    ignoreIOError m = m `catch` (\(_ :: IOError) -> pure ())

withTempDir2 :: FilePath -> FilePath -> ((FilePath, FilePath) -> IO ()) -> IO ()
withTempDir2 dir1 dir2 action =
    withTempDir dir1 $ \d1 ->
        withTempDir dir2 $ \d2 ->
            action (d1, d2)

runSimpleApp :: Text -> FilePath -> AppM a -> IO (Either AppError a)
runSimpleApp source localDir action = runApp env action
  where
    env =
        defaultEnv
            WikiConfig
                { wikiPath = localDir
                , wikiSource = WikiSource source
                , wikiMaxRetries = 3
                }
