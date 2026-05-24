{-# LANGUAGE OverloadedStrings #-}

module Knode.WorkspaceSpec (spec) where

import Test.Hspec

import Control.Monad (void)
import qualified Data.Text.IO as TIO
import Knode.App (AppM)
import Knode.Capabilities (Querying (..), Workspace (..))
import Knode.Data (AppError (..), Change (..), Page (..), PageOp (..), Query (..), QueryResult (..))
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Test.Helpers (runSimpleApp, withTempDir)

spec :: Spec
spec = describe "Workspace.apply" $ around (withTempDir "/tmp/knode-test-workspace") $ do
    -- WHEN asked to write a page under a non-existent directory,
    -- the system SHALL create all necessary parent directories and write the page content.
    it "creates parent directories and writes page" $ \tmpDir -> do
        let page = "subdir/nested/page.md"
            content = "Hello, world!"
        result <- runAppInDir tmpDir $ do
            apply [PageChange (Page page) (Overwrite content)]
        result `shouldBe` Right []
        doesFileExist (tmpDir </> page)
            `shouldReturn` True
        TIO.readFile (tmpDir </> page)
            `shouldReturn` content

    -- WHEN asked to write a page that already exists,
    -- the system SHALL overwrite the existing content with the new content.
    it "overwrites existing page" $ \tmpDir -> do
        let page = "page.md"
            oldContent = "Old content"
            newContent = "New content"
        TIO.writeFile (tmpDir </> page) oldContent
        result <- runAppInDir tmpDir $ do
            apply [PageChange (Page page) (Overwrite newContent)]
        result `shouldBe` Right []
        TIO.readFile (tmpDir </> page)
            `shouldReturn` newContent

    -- WHEN asked to replace text in a page that does not exist,
    -- the system SHALL report an error indicating that the page was not found.
    it "reports error when replacing text in non-existent page" $ \tmpDir -> do
        let page = "missing.md"
            oldText = "not found"
            newText = "new content"
        result <- runAppInDir tmpDir $ do
            apply [PageChange (Page page) (ReplaceAll oldText newText)]
        case result of
            Left (IOError "missing.md" _) -> pure ()
            got -> expectationFailure $ "Expected IOError got " <> (show got)

    -- WHEN asked to replace text in a page,
    -- the system SHALL modify it on disk.
    it "replaces text on disk" $ \tmpDir -> do
        let page = "subdir/nested/page.md"
            content = "Hello, world!"
        void $ runAppInDir tmpDir $ do
            void $ apply [PageChange (Page page) (Overwrite content)]
            void $ apply [PageChange (Page page) (ReplaceAll "world" "WORLD")]
        TIO.readFile (tmpDir </> page)
            `shouldReturn` "Hello, WORLD!"

    -- WHEN asked to read a page that exists,
    -- the system SHALL return the page content.
    it "reads page content" $ \tmpDir -> do
        let page = "page.md"
            content = "Hello, world!"
        TIO.writeFile (tmpDir </> page) content
        result <- runAppInDir tmpDir $ execQuery (ReadPage (Page page))
        result `shouldBe` Right (PageContent content)

    -- WHEN asked to read a page that does not exist,
    -- the system SHALL return an error.
    it "returns error for missing page" $ \tmpDir -> do
        result <- runAppInDir tmpDir $ execQuery (ReadPage (Page "missing.md"))
        case result of
            Left (IOError "missing.md" _) -> pure ()
            got -> expectationFailure $ "Expected IOError, got " <> show got

    -- WHEN asked to grep a page,
    -- the system SHALL return matching lines.
    it "greps page content" $ \tmpDir -> do
        let page = "page.md"
            content = "line one\nfoo bar\nline three\nfoo baz"
        TIO.writeFile (tmpDir </> page) content
        result <- runAppInDir tmpDir $ execQuery (GrepPage (Page page) "foo")
        result `shouldBe` Right (GrepMatches ["foo bar", "foo baz"])

runAppInDir :: FilePath -> AppM a -> IO (Either AppError a)
runAppInDir dir =
    runSimpleApp "BAD URL" dir
