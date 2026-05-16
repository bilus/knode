{-# LANGUAGE OverloadedStrings #-}

module Knode.Fake.WorkspaceSpec (spec) where

import Test.Hspec

import Control.Monad (void)
import qualified Data.Set as Set
import Knode.Capabilities (Wiki (..), Workspace (..))
import Knode.Data (AppError (..), Change (..), Page (..))
import Knode.Fake.Workspace (Author (..), FakeFS (..), getLocalFS, getRemoteFS, getStagedFiles, simulateRemoteChange)
import Test.Helpers (shouldBeErrorAnd, shouldBeSuccessAnd)

spec :: SpecWith ()
spec = describe "Wiki" $ do
    it "after publish, sync does not change local files" $
        do
            sync
            void $ apply [Write (Page "page.md") "content"]
            stage [Write (Page "page.md") "content"] "Add page"
            publish
            filesBefore <- fsFiles <$> getLocalFS
            sync
            filesAfter <- fsFiles <$> getLocalFS
            pure (filesBefore, filesAfter)
            --
            `shouldBeSuccessAnd` \(filesBefore, filesAfter) ->
                filesBefore `shouldBe` filesAfter

    it "publish succeeds when no conflict and clears stage" $
        do
            sync
            void $ apply [Write (Page "page.md") "content"]
            stage [Write (Page "page.md") "content"] "Add page"
            publish
            getStagedFiles
            --
            `shouldBeSuccessAnd` \stagedFiles ->
                stagedFiles `shouldBe` Set.empty

    it "publish fails when remote changed" $
        do
            sync
            void $ apply [Write (Page "page.md") "local content"]
            stage [Write (Page "page.md") "local content"] "Local change"
            simulateRemoteChange (Author "other@example.com") (Write (Page "other.md") "other")
            publish
            --
            `shouldBeErrorAnd` \(err, _) ->
                err `shouldBe` PublishConflict []

    it "local chages dropped on sync, making conflict go away" $
        do
            sync
            void $ apply [Write (Page "page.md") "local content"]
            stage [Write (Page "page.md") "local content"] "Local change"
            simulateRemoteChange (Author "other@example.com") (Write (Page "other.md") "other")
            -- Retry (that's what application logic does)
            sync
            void $ apply [Write (Page "page.md") "local content"]
            stage [Write (Page "page.md") "local content"] "Local change"
            publish
            (,) <$> getRemoteFS <*> getLocalFS
            --
            `shouldBeSuccessAnd` \(remoteFS, localFS) ->
                remoteFS `shouldBe` localFS
