{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Knode.Service (
    makeChanges,
) where

import Control.Monad (forM_, unless)
import Control.Monad.Except (MonadError, catchError, throwError)
import Data.Text (Text)
import Knode.Capabilities (Config (..), Reporting (..), Wiki (..), Workspace (..))
import Knode.Data (AppError (..), Change, WikiConfig (..))

{- | Apply changes to the wiki and publish them. Syncs with the source first,
reports any change errors to the agent, and retries on publish conflicts.
-}
makeChanges ::
    (Workspace m, Wiki m, Reporting m, MonadError AppError m) =>
    Text -> [Change] -> m ()
makeChanges description changes = do
    cfg <- config
    go (wikiMaxRetries cfg)
  where
    go remainingRetries = do
        sync
        errors <- apply changes
        unless (null errors) $ do
            forM_ errors reportChangeError
            sync
            throwError ChangesNotApplied
        stage changes description
        published <- tryPublish
        unless published $
            if remainingRetries <= 0
                then throwError $ PublishConflict []
                else go (remainingRetries - 1)

    tryPublish :: (Wiki m, MonadError AppError m) => m Bool
    tryPublish = catchError (True <$ publish) (const $ pure False)
