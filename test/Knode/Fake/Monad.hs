{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Knode.Fake.Monad (
    FakeState (..),
    FakeM (..),
    emptyState,
    runFake,
    execFake,
    withAuthor,
    simulateRemoteChange,
    beforePublishHook,
    clearHooks,
    getLocalFS,
    getRemoteFS,
    getStagedFiles,
    getReportedErrors,
    traceLog,
) where

import Control.Monad (when)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.State.Strict (MonadState, StateT, gets, modify', runStateT)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Knode.Capabilities (Config (..), Reporting (..), Wiki (..), Workspace (..))
import Knode.Data (AppError (..), Change (PageChange), ChangeError (..), Page (Page), PageOp (Overwrite, ReplaceAll), WikiConfig)
import Knode.Fake.Data

data FakeState = FakeState
    { stateLocal :: !FakeFS
    , stateRemote :: !FakeFS
    , stateStagedFiles :: !(Set FilePath)
    , stateAuthor :: !Author
    , stateReportedErrors :: ![ChangeError]
    , stateBeforePublishHooks :: ![FakeM ()]
    }

emptyState :: Author -> FakeState
emptyState author = FakeState emptyFS emptyFS Set.empty author [] []

newtype FakeM a = FakeM {unFakeM :: ExceptT AppError (StateT FakeState (ReaderT WikiConfig IO)) a}
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadState FakeState
        , MonadError AppError
        , MonadReader WikiConfig
        )

runFake :: WikiConfig -> FakeState -> FakeM a -> IO (Either AppError a, FakeState)
runFake cfg st action =
    flip runReaderT cfg $ flip runStateT st $ runExceptT $ unFakeM action

execFake :: WikiConfig -> FakeState -> FakeM a -> IO (Either AppError (), FakeState)
execFake cfg st action = do
    (result, finalState) <- runFake cfg st action
    pure (() <$ result, finalState)

instance Config FakeM where
    config :: FakeM WikiConfig
    config = asks id

instance Wiki FakeM where
    sync :: FakeM ()
    sync = do
        remote <- gets stateRemote
        modify' $ \st ->
            st
                { stateLocal = remote
                , stateStagedFiles = Set.empty
                }

    stage :: [Change] -> Text -> FakeM ()
    stage changes _description = do
        let paths = map changePath changes
        author <- gets stateAuthor
        modify' $ \st ->
            st
                { stateStagedFiles = Set.union (stateStagedFiles st) (Set.fromList paths)
                , stateLocal = addToHistory author changes (stateLocal st)
                }
      where
        changePath (PageChange (Page p) _) = p

    publish :: FakeM ()
    publish = do
        runBeforePublishHooks
        checkConflict
        newRemoteState <- getStagedFS
        modify' $ \st -> st{stateRemote = newRemoteState, stateStagedFiles = Set.empty}

instance Reporting FakeM where
    reportChangeError :: ChangeError -> FakeM ()
    reportChangeError err = modify' $ \st ->
        st{stateReportedErrors = stateReportedErrors st ++ [err]}

instance Workspace FakeM where
    apply :: [Change] -> FakeM [ChangeError]
    apply changes = catMaybes <$> mapM applyChange changes
      where
        applyChange (PageChange (Page path) (Overwrite content)) = do
            modify' $ \st ->
                st{stateLocal = (stateLocal st){fsFiles = Map.insert path content (fsFiles $ stateLocal st)}}
            pure Nothing
        applyChange (PageChange page (ReplaceAll old new)) = do
            localFS <- gets stateLocal
            if (not $ contains page old localFS)
                then pure $ Just $ EditNotFound page old
                else do
                    modify' $ \st ->
                        st{stateLocal = replaceAll page old new localFS}
                    pure Nothing

contains :: Page -> Text -> FakeFS -> Bool
contains (Page path) old FakeFS{fsFiles} =
    case Map.lookup path fsFiles of
        Just content -> old `T.isInfixOf` content
        Nothing -> False

replaceAll :: Page -> Text -> Text -> FakeFS -> FakeFS
replaceAll (Page path) old new fs@FakeFS{fsFiles} =
    fs{fsFiles = Map.update replace path fsFiles}
  where
    replace content =
        Just $ T.replace old new content

runBeforePublishHooks :: FakeM ()
runBeforePublishHooks = do
    hooks <- gets stateBeforePublishHooks
    sequence_ (reverse hooks)

checkConflict :: FakeM ()
checkConflict = do
    localHistory <- gets $ fsHistory . stateLocal
    remoteHistory <- gets $ fsHistory . stateRemote
    when (isConflict localHistory remoteHistory) $
        throwError $
            PublishConflict []

getStagedFS :: FakeM FakeFS
getStagedFS = do
    staged <- gets stateStagedFiles
    local <- gets stateLocal
    let localFiles = fsFiles local
        stagedFiles = Map.filterWithKey (\k _ -> Set.member k staged) localFiles
        newHistory = fsHistory local
    pure
        local
            { fsFiles = stagedFiles
            , fsHistory = newHistory
            }

getLocalFS :: FakeM FakeFS
getLocalFS = gets stateLocal

getRemoteFS :: FakeM FakeFS
getRemoteFS = gets stateRemote

getStagedFiles :: FakeM (Set FilePath)
getStagedFiles = gets stateStagedFiles

getReportedErrors :: FakeM [ChangeError]
getReportedErrors = gets stateReportedErrors

withAuthor :: Author -> FakeM a -> FakeM a
withAuthor author action = do
    oldAuthor <- gets stateAuthor
    modify' $ \st -> st{stateAuthor = author}
    result <- action
    modify' $ \st -> st{stateAuthor = oldAuthor}
    pure result

simulateRemoteChange :: Author -> Change -> FakeM ()
simulateRemoteChange author change = modify' $ \st ->
    st{stateRemote = addToHistory author [change] (stateRemote st)}

beforePublishHook :: FakeM () -> FakeM ()
beforePublishHook hook = modify' $ \st ->
    st{stateBeforePublishHooks = hook : stateBeforePublishHooks st}

clearHooks :: FakeM ()
clearHooks = modify' $ \st -> st{stateBeforePublishHooks = []}

traceLog :: String -> FakeM ()
traceLog msg = liftIO $ putStrLn $ "=== " ++ msg ++ " ==="
