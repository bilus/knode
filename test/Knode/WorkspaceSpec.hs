{-# LANGUAGE OverloadedStrings #-}

module Knode.WorkspaceSpec (spec) where

import Test.Hspec

import qualified Data.Text.IO as TIO
import Knode.App (AppM)
import Knode.Capabilities (Workspace (..))
import Knode.Data (AppError, Change (..), ChangeError (..), Page (..))
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Test.Helpers (runWiki, withTempDir)

spec :: Spec
spec = describe "Workspace.apply" $ around (withTempDir "/tmp/knode-test-workspace") $ do
    -- WHEN asked to write a page under a non-existent directory,
    -- the system SHALL create all necessary parent directories and write the page content.
    it "creates parent directories and writes page" $ \tmpDir -> do
        let page = "subdir/nested/page.md"
            content = "Hello, world!"
        result <- runWikiInDir tmpDir $ do
            apply [Write (Page page) content]
        result `shouldBe` Right []
        exists <- doesFileExist (tmpDir </> page)
        exists `shouldBe` True
        actual <- TIO.readFile (tmpDir </> page)
        actual `shouldBe` content

    -- WHEN asked to write a page that already exists,
    -- the system SHALL overwrite the existing content with the new content.
    it "overwrites existing page" $ \tmpDir -> do
        let page = "page.md"
            oldContent = "Old content"
            newContent = "New content"
        TIO.writeFile (tmpDir </> page) oldContent
        result <- runWikiInDir tmpDir $ do
            apply [Write (Page page) newContent]
        result `shouldBe` Right []
        actual <- TIO.readFile (tmpDir </> page)
        actual `shouldBe` newContent

    -- WHEN asked to write a page outside the root directory via relative path,
    -- the system SHALL reject the request.
    it "rejects relative path escape" $ \tmpDir -> do
        let page = "../outside.md"
        result <- runWikiInDir tmpDir $ apply [Write (Page page) "content"]
        result `shouldBe` Right [WriteError (Page page) "path escapes root directory"]

    -- WHEN asked to write a page outside the root directory via absolute path,
    -- the system SHALL reject the request.
    it "rejects absolute path" $ \tmpDir -> do
        let page = "/tmp/absolute.md"
        result <- runWikiInDir tmpDir $ apply [Write (Page page) "content"]
        result `shouldBe` Right [WriteError (Page page) "path escapes root directory"]

runWikiInDir :: FilePath -> AppM a -> IO (Either AppError a)
runWikiInDir dir =
    runWiki "BAD URL" dir
