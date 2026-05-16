{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Knode.Fake.Workspace (
    Author (..),
    FakeFS (..),
    FakeState (..),
    FakeM,
    emptyFS,
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
import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Knode.Capabilities (Config (..), Reporting (..), Wiki (..), Workspace (..))
import Knode.Data (AppError (..), Change (..), ChangeError (..), Page (..), WikiConfig (..))

newtype Author = Author String
    deriving (Show, Eq)

-- | Commit history, used to detect conflicts. Oldest first.
type History = [(Author, Change)]

data FakeFS = FakeFS
    { fsFiles :: !(Map FilePath Text)
    , fsHistory :: !History
    }
    deriving (Show, Eq)

emptyFS :: FakeFS
emptyFS = FakeFS Map.empty []

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

instance Config FakeM where
    config :: FakeM WikiConfig
    config = asks id

instance Workspace FakeM where
    apply :: [Change] -> FakeM [ChangeError]
    apply changes = catMaybes <$> mapM applyChange changes
      where
        applyChange (Write (Page path) content) = do
            modify' $ \st ->
                st{stateLocal = (stateLocal st){fsFiles = Map.insert path content (fsFiles $ stateLocal st)}}
            pure Nothing
        applyChange (Edit page old _new) =
            pure $ Just $ EditNotFound page old

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
        changePath (Write (Page p) _) = p
        changePath (Edit (Page p) _ _) = p

    publish :: FakeM ()
    publish = do
        runBeforePublishHooks
        checkConflict
        newRemoteState <- getStagedFS
        modify' $ \st -> st{stateRemote = newRemoteState, stateStagedFiles = Set.empty}

runBeforePublishHooks :: FakeM ()
runBeforePublishHooks = do
    hooks <- gets stateBeforePublishHooks
    sequence_ (reverse hooks)

addToHistory :: Author -> [Change] -> FakeFS -> FakeFS
addToHistory author changes fs =
    fs{fsHistory = fsHistory fs ++ map (author,) changes}

isConflict :: History -> History -> Bool
isConflict sourceHistory targetHistory =
    not (targetHistory `isPrefixOf` sourceHistory)

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

instance Reporting FakeM where
    reportChangeError :: ChangeError -> FakeM ()
    reportChangeError err = modify' $ \st ->
        st{stateReportedErrors = stateReportedErrors st ++ [err]}
