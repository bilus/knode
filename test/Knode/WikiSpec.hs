{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Knode.WikiSpec (spec) where

import Test.Hspec

import Control.Monad (void)
import Data.Either (isRight)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Knode.Capabilities (Wiki (..), Workspace (..))
import Knode.Data (Change (..), Page (..), PageOp (..))
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))
import Test.Helpers (runSimpleApp, withTempDir2)
import Test.Helpers.Git (gitClone, initBareRepo, initRepoWithFile)

spec :: Spec
spec = describe "Wiki.sync" $ around (withTempDir2 "/tmp/knode-test-remote" "/tmp/knode-test-local") $ do
    -- WHEN the local wiki directory does not exist,
    -- the system SHALL clone the wiki from the configured source.
    it "clones when local directory does not exist" $ \(remoteDir, localDir) -> do
        let wikiDir = localDir </> "wiki"
        initBareRepo remoteDir
        runSimpleApp ("file://" <> T.pack remoteDir) wikiDir sync
            `shouldReturn` Right ()
        doesDirectoryExist (wikiDir </> ".git")
            `shouldReturn` True

    -- WHEN the local wiki already exists,
    -- the system SHALL reset it to match the remote.
    it "resets existing wiki to match remote" $ \(remoteDir, localDir) -> do
        let wikiDir = localDir </> "wiki"
            remoteUrl = "file://" <> T.pack remoteDir
        initRepoWithFile remoteDir "page.md" "remote content"
        gitClone remoteUrl wikiDir
        TIO.writeFile (wikiDir </> "local.md") "local only"
        runSimpleApp remoteUrl wikiDir sync
            `shouldReturn` Right ()
        doesFileExist (wikiDir </> "local.md")
            `shouldReturn` False
        TIO.readFile (wikiDir </> "page.md")
            `shouldReturn` "remote content"

    -- WHEN the local wiki has a different remote origin,
    -- the system SHALL reject the sync.
    it "reject different remote origin" $ \(remoteRoot, localDir) -> do
        let wikiDir = localDir </> "wiki"
            thisRemoteDir = remoteRoot </> "this"
            otherRemoteDir = remoteRoot </> "other"
            thisRemote = "file://" <> T.pack thisRemoteDir
            otherRemote = "file://" <> T.pack otherRemoteDir
        initBareRepo thisRemoteDir
        initBareRepo otherRemoteDir
        gitClone otherRemote wikiDir
        isRight <$> runSimpleApp thisRemote wikiDir sync
            `shouldReturn` False

    -- WHEN sync is called after staging but before publishing,
    -- the system SHALL discard the staged changes.
    it "discards staged unpublished changes on sync" $ \(remoteDir, localDir) -> do
        let wikiDir = localDir </> "wiki"
            remoteUrl = "file://" <> T.pack remoteDir
            change = PageChange (Page "new.md") (Overwrite "new content")
        initRepoWithFile remoteDir "page.md" "original"
        gitClone remoteUrl wikiDir
        result <- runSimpleApp remoteUrl wikiDir $ do
            sync
            void $ apply [change]
            stage [change] "Add new page"
            sync
            pure ()
        result `shouldSatisfy` isRight
        doesFileExist (wikiDir </> "new.md")
            `shouldReturn` False

    -- WHEN publish is called,
    -- the system SHALL only push staged changes.
    it "publishes only staged changes" $ \(remoteDir, localDir) -> do
        let wikiDir = localDir </> "wiki"
            verifyDir = localDir </> "verify"
            remoteUrl = "file://" <> T.pack remoteDir
            lostChange = PageChange (Page "unstaged.md") (Overwrite "staged content")
            survivingChange = PageChange (Page "staged.md") (Overwrite "staged content")
        initBareRepo remoteDir
        result <- runSimpleApp remoteUrl wikiDir $
            do
                sync
                void $ apply [survivingChange, lostChange]
                stage [survivingChange] "Add staged page"
                publish
        result `shouldSatisfy` isRight
        gitClone remoteUrl verifyDir
        doesFileExist (verifyDir </> "staged.md")
            `shouldReturn` True
        doesFileExist (verifyDir </> "unstaged.md")
            `shouldReturn` False
