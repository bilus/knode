{-# LANGUAGE OverloadedStrings #-}

module Knode.FakeSpec (spec) where

import Test.Hspec

import Control.Monad (void)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Knode.Capabilities (Wiki (..), Workspace (..))
import Knode.Data (AppError (..), Change (..), Page (..), PageOp (..))
import Knode.Fake.Data (Author (..), FakeFS (..))
import Knode.Fake.Monad (getLocalFS, getRemoteFS, getStagedFiles, simulateRemoteChange)
import Test.Helpers (shouldBeErrorAnd, shouldBeSuccessAnd)

spec :: SpecWith ()
spec = describe "FakeM" $ do
    describe "apply" $ do
        it "overwrites page" $
            do
                void $ apply [PageChange (Page "page.md") (Overwrite "hello world")]
                fsFiles <$> getLocalFS
                --
                `shouldBeSuccessAnd` \files -> do
                    files `shouldSatisfy` Map.member "page.md"
                    Map.lookup "page.md" files `shouldBe` Just "hello world"

        it "replaces text in page" $
            do
                void $ apply [PageChange (Page "page.md") (Overwrite "hello world")]
                void $ apply [PageChange (Page "page.md") (ReplaceAll "world" "WORLD")]
                fsFiles <$> getLocalFS
                --
                `shouldBeSuccessAnd` \files -> do
                    files `shouldSatisfy` Map.member "page.md"
                    Map.lookup "page.md" files `shouldBe` Just "hello WORLD"

    describe "sync" $ do
        it "does not change local files after publish" $
            do
                sync
                void $ apply [PageChange (Page "page.md") (Overwrite "hello world")]
                stage [PageChange (Page "page.md") (Overwrite "hello world")] "Add page"
                publish
                filesBefore <- fsFiles <$> getLocalFS
                sync
                filesAfter <- fsFiles <$> getLocalFS
                pure (filesBefore, filesAfter)
                --
                `shouldBeSuccessAnd` \(filesBefore, filesAfter) ->
                    filesBefore `shouldBe` filesAfter

        it "drops local chages, making conflict go away" $
            do
                sync
                void $ apply [PageChange (Page "page.md") (Overwrite "local content")]
                stage [PageChange (Page "page.md") (Overwrite "local content")] "Local change"
                simulateRemoteChange (Author "other@example.com") (PageChange (Page "other.md") (Overwrite "other"))
                -- Retry (that's what application logic does)
                sync
                void $ apply [PageChange (Page "page.md") (Overwrite "local content")]
                stage [PageChange (Page "page.md") (Overwrite "local content")] "Local change"
                publish
                (,) <$> getRemoteFS <*> getLocalFS
                --
                `shouldBeSuccessAnd` \(remoteFS, localFS) ->
                    remoteFS `shouldBe` localFS

    describe "publish" $ do
        it "succeeds when no conflict and clears stage" $
            do
                sync
                void $ apply [PageChange (Page "page.md") (Overwrite "hello world")]
                stage [PageChange (Page "page.md") (Overwrite "hello world")] "Add page"
                publish
                getStagedFiles
                --
                `shouldBeSuccessAnd` \stagedFiles ->
                    stagedFiles `shouldBe` Set.empty
        it "fails when remote changed" $
            do
                sync
                void $ apply [PageChange (Page "page.md") (Overwrite "local content")]
                stage [PageChange (Page "page.md") (Overwrite "local content")] "Local change"
                simulateRemoteChange (Author "other@example.com") (PageChange (Page "other.md") (Overwrite "other"))
                publish
                --
                `shouldBeErrorAnd` \(err, _) ->
                    err `shouldBe` PublishConflict []
