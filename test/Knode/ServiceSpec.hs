{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Knode.ServiceSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Control.Monad (forM_)
import qualified Data.Map as Map
import Knode.Data (AppError (..), Change (..), Page (..))
import Knode.Fake.Workspace (Author (..), FakeFS (..), FakeM, FakeState (..), beforePublishHook, clearHooks, getLocalFS, getRemoteFS, simulateRemoteChange)
import Knode.Service (makeChanges)
import Test.Helpers (shouldBeErrorAnd, shouldBeSuccessAnd)

spec :: Spec
spec = describe "makeChanges" $ do
    it "succeeds with empty changeset" $
        do
            makeChanges "No changes" []
            remoteFiles <- fsFiles <$> getRemoteFS
            pure remoteFiles
            --
            `shouldBeSuccessAnd` \remoteFiles ->
                remoteFiles `shouldBe` Map.empty

    it "retries on conflict and succeeds" $
        do
            beforePublishHook $ do
                causeConflict
                clearHooks
            makeChanges "Add page" [Write (Page "page.md") "content"]
            (localFS, remoteFS) <- (,) <$> getLocalFS <*> getRemoteFS
            pure (localFS, remoteFS)
            --
            `shouldBeSuccessAnd` \(localFS, remoteFS) -> do
                fsHistory localFS `shouldBe` fsHistory remoteFS
                fsFiles remoteFS `shouldSatisfy` Map.member "page.md"

    it "gives up after max retries" $
        do
            beforePublishHook causeConflict
            makeChanges "Add page" [Write (Page "page.md") "content"]
            --
            `shouldBeErrorAnd` \(err, _) ->
                err `shouldBe` PublishConflict []

    it "syncs and reports all errors when changes fail" $
        do
            makeChanges
                "Edit pages"
                [ Edit (Page "a.md") "not found 1" "new1"
                , Edit (Page "b.md") "not found 2" "new2"
                ]
            --
            `shouldBeErrorAnd` \(err, FakeState{stateReportedErrors, stateLocal, stateRemote}) -> do
                err `shouldBe` ChangesNotApplied
                length stateReportedErrors `shouldBe` 2
                stateLocal `shouldBe` stateRemote

    -- WHEN applying changes with paths outside the root directory,
    -- the system SHALL reject the request.
    it "rejects paths outside root directory" $
        forM_ ["../outside.md", "/etc/passwd"] $ \wrongPath ->
            do
                makeChanges
                    "Escape attempt"
                    [ Write (Page wrongPath) "malicious"
                    ]
                --
                `shouldBeErrorAnd` \(err, FakeState{stateLocal}) -> do
                    err `shouldBe` WrongPath wrongPath
                    fsHistory stateLocal `shouldSatisfy` null

causeConflict :: FakeM ()
causeConflict =
    simulateRemoteChange
        (Author "other@example.com")
        (Write (Page "other.md") "other")
