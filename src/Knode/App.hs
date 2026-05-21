{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Knode.App (
    AppM,
    Env (..),
    WorkspaceHandle (..),
    WikiHandle (..),
    ReportingHandle (..),
    defaultWorkspaceHandle,
    defaultWikiHandle,
    defaultReportingHandle,
    defaultEnv,
    runApp,
) where

import Control.Exception (displayException)
import Control.Monad (forM_)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.Trans (lift)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Knode.Capabilities (Config (..), Reporting (..), Wiki (..), Workspace (..))
import Knode.Combinators (andM, ifM, whenM)
import Knode.Data (AppError (..), Change (..), ChangeError (..), Page (..), WikiConfig (..), WikiSource (..))
import Shelly (Sh)
import qualified Shelly as Sh
import System.FilePath (takeDirectory, (</>))

data WorkspaceHandle = WorkspaceHandle
    { _apply :: [Change] -> AppM [ChangeError]
    }

data WikiHandle = WikiHandle
    { _sync :: !(AppM ())
    , _publish :: !(AppM ())
    }

data ReportingHandle = ReportingHandle
    { _reportChangeError :: ChangeError -> AppM ()
    }

data Env = Env
    { envWiki :: !WikiConfig
    , envWorkspaceHandle :: !WorkspaceHandle
    , envWikiHandle :: !WikiHandle
    , envReportingHandle :: !ReportingHandle
    }

newtype AppM a = AppM {unAppM :: ReaderT Env (ExceptT AppError Sh) a}
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadReader Env
        , MonadError AppError
        , MonadIO
        )

runApp :: Env -> AppM a -> IO (Either AppError a)
runApp env = Sh.shelly . Sh.errExit False . runExceptT . flip runReaderT env . unAppM

defaultEnv :: WikiConfig -> Env
defaultEnv cfg =
    Env
        { envWiki = cfg
        , envWorkspaceHandle = defaultWorkspaceHandle
        , envWikiHandle = defaultWikiHandle
        , envReportingHandle = defaultReportingHandle
        }

liftSh :: (Text -> AppError) -> Sh a -> AppM a
liftSh mkErr action = do
    result <- AppM $ lift $ lift $ Sh.catchany_sh (Right <$> action) onError
    either throwError pure result
  where
    onError = pure . Left . mkErr . T.pack . displayException

logCmd :: Text -> AppM ()
logCmd msg = liftIO $ TIO.putStrLn $ "$ " <> msg

shell :: Maybe FilePath -> String -> [String] -> AppM Text
shell cwd cmd args = do
    logCmd $ T.pack $ cmd <> " " <> unwords args
    result <- AppM $ lift $ lift $ inDir $ Sh.catchany_sh runCmd onError
    either throwError pure result
  where
    inDir action = case cwd of
        Just path -> Sh.chdir path action
        Nothing -> action
    runCmd = do
        output <- Sh.run cmd $ map T.pack args
        exitCode <- Sh.lastExitCode
        pure $
            if exitCode /= 0
                then Left $ CommandFailed cwd cmd args exitCode
                else Right output
    onError = pure . Left . InternalError . T.pack . displayException

shell_ :: Maybe FilePath -> String -> [String] -> AppM ()
shell_ cwd cmd args = () <$ shell cwd cmd args

zeroExit :: AppM a -> AppM Bool
zeroExit m = catchError (True <$ m) (const $ pure False)

git :: [String] -> AppM Text
git args = do
    WikiConfig{wikiPath} <- asks envWiki
    shell (Just wikiPath) "git" args

git_ :: [String] -> AppM ()
git_ args = () <$ git args

hasStaged :: AppM Bool
hasStaged = not <$> zeroExit (git_ ["diff", "--cached", "--quiet"])

instance Config AppM where
    config = asks envWiki

instance Workspace AppM where
    apply changes = asks envWorkspaceHandle >>= \h -> _apply h changes

instance Wiki AppM where
    sync = asks envWikiHandle >>= _sync

    stage changes description = do
        forM_ changes $ \change -> do
            let (Page pagePath) = changePage change
            git_ ["add", pagePath]
        whenM hasStaged $
            git_ ["commit", "-m", T.unpack description]

    publish = asks envWikiHandle >>= _publish

instance Reporting AppM where
    reportChangeError err = asks envReportingHandle >>= \h -> _reportChangeError h err

formatChangeError :: ChangeError -> Text
formatChangeError (EditNotFound (Page p) old) =
    "Edit failed: text not found in " <> T.pack p <> ": " <> old
formatChangeError (WriteError (Page p) msg) =
    "Write failed: " <> T.pack p <> ": " <> msg

changePage :: Change -> Page
changePage (Write page _) = page
changePage (Edit page _ _) = page

defaultWorkspaceHandle :: WorkspaceHandle
defaultWorkspaceHandle =
    WorkspaceHandle
        { _apply = \changes -> do
            WikiConfig{wikiPath} <- asks envWiki
            catMaybes <$> mapM (applyChange wikiPath) changes
        }
  where
    applyChange wikiPath (Write (Page path) content) = do
        let fullPath = wikiPath </> path
            dir = takeDirectory fullPath
        logCmd $ T.pack $ "mkdir -p " <> dir
        liftSh (IOError dir) $ Sh.mkdir_p (Sh.fromText $ T.pack dir)
        logCmd $ T.pack $ "cat > " <> fullPath
        liftSh (IOError fullPath) $ Sh.writefile (Sh.fromText $ T.pack fullPath) content
        pure Nothing
    applyChange _ (Edit page old _new) =
        pure $ Just $ EditNotFound page old

defaultWikiHandle :: WikiHandle
defaultWikiHandle =
    WikiHandle
        { _sync = ifM isWikiPresent resetWiki cloneWiki
        , _publish = git_ ["push"]
        }

defaultReportingHandle :: ReportingHandle
defaultReportingHandle =
    ReportingHandle
        { _reportChangeError = \err -> liftIO $ TIO.putStrLn $ formatChangeError err
        }

isWikiPresent :: AppM Bool
isWikiPresent = do
    WikiConfig{wikiPath} <- asks envWiki
    testDir wikiPath `andM` zeroExit (git ["rev-parse", "--git-dir"])
  where
    testDir path = zeroExit $ shell_ Nothing "test" ["-d", path]

cloneWiki :: AppM ()
cloneWiki = do
    WikiConfig{wikiSource, wikiPath} <- asks envWiki
    let (WikiSource url) = wikiSource
    shell_ Nothing "git" ["clone", T.unpack url, wikiPath]

resetWiki :: AppM ()
resetWiki = do
    assertCorrectSource
    git_ ["fetch", "origin"]
    git_ ["reset", "--hard", "origin/HEAD"]
    git_ ["clean", "-fd"]

assertCorrectSource :: AppM ()
assertCorrectSource = do
    WikiConfig{wikiSource, wikiPath} <- asks envWiki
    whenM (not <$> isCorrectSource wikiSource) $
        throwError $
            WrongSource wikiSource wikiPath
  where
    isCorrectSource expectedSource = do
        output <- git ["remote", "get-url", "origin"]
        pure $ (WikiSource $ T.strip output) == expectedSource
