{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Knode.ServiceSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Control.Monad (forM_, void)
import qualified Data.Map as Map
import Knode.Capabilities (Workspace (..))
import Knode.Data (AppError (..), Change (..), Page (..), PageOp (..), Query (..), QueryResult (..))
import Knode.Fake.Data (Author (..), FakeFS (..))
import Knode.Fake.Monad (FakeM, FakeState (..), beforePublishHook, clearHooks, getLocalFS, getRemoteFS, getReportedQueryResults, simulateRemoteChange)
import Knode.Service (makeChanges, query)
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
            makeChanges "Add page" [PageChange (Page "page.md") (Overwrite "content")]
            (localFS, remoteFS) <- (,) <$> getLocalFS <*> getRemoteFS
            pure (localFS, remoteFS)
            --
            `shouldBeSuccessAnd` \(localFS, remoteFS) -> do
                fsHistory localFS `shouldBe` fsHistory remoteFS
                fsFiles remoteFS `shouldSatisfy` Map.member "page.md"

    it "gives up after max retries" $
        do
            beforePublishHook causeConflict
            makeChanges "Add page" [PageChange (Page "page.md") (Overwrite "content")]
            --
            `shouldBeErrorAnd` \(err, _) ->
                err `shouldBe` PublishConflict []

    it "syncs and reports all errors when changes fail" $
        do
            makeChanges
                "Edit pages"
                [ PageChange (Page "a.md") (ReplaceAll "not found 1" "new1")
                , PageChange (Page "b.md") (ReplaceAll "not found 2" "new2")
                ]
            --
            `shouldBeErrorAnd` \(err, FakeState{stateReportedErrors, stateLocal, stateRemote}) -> do
                err `shouldBe` ChangesNotApplied
                length stateReportedErrors `shouldBe` 2
                stateLocal `shouldBe` stateRemote

    -- TODO: WHEN replacing text in a non-existent page,
    -- the system SHALL report an error for that page.

    -- WHEN applying changes with paths outside the root directory,
    -- the system SHALL reject the request.
    it "rejects paths outside root directory" $
        forM_ ["../outside.md", "/etc/passwd"] $ \wrongPath ->
            do
                makeChanges
                    "Escape attempt"
                    [ PageChange (Page wrongPath) (Overwrite "malicious")
                    ]
                --
                `shouldBeErrorAnd` \(err, FakeState{stateLocal}) -> do
                    err `shouldBe` WrongPath wrongPath
                    fsHistory stateLocal `shouldSatisfy` null

    -- WHEN executing a query,
    -- the system SHALL report the query result.
    it "reports query result" $
        do
            void $ apply [PageChange (Page "page.md") (Overwrite "Hello, world!")]
            query (ReadPage (Page "page.md"))
            getReportedQueryResults
            --
            `shouldBeSuccessAnd` \results ->
                results `shouldBe` [PageContent "Hello, world!"]

    it "reports grep matches" $
        do
            void $ apply [PageChange (Page "page.md") (Overwrite "line one\nfoo bar\nline three\nfoo baz")]
            query (GrepPage (Page "page.md") "foo")
            getReportedQueryResults
            --
            `shouldBeSuccessAnd` \results ->
                results `shouldBe` [GrepMatches ["foo bar", "foo baz"]]

causeConflict :: FakeM ()
causeConflict =
    simulateRemoteChange
        (Author "other@example.com")
        (PageChange (Page "other.md") (Overwrite "other"))
