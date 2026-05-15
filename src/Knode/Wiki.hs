{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Knode.Wiki (
    -- * Wiki monad
    Wiki,
    runWiki,

    -- * Types
    Page (..),
    Change (..),
    RepoUrl (..),
    Repo (..),

    -- * Errors
    WikiError,
    verboseError,

    -- * Operations
    parseChanges,
    fetch,
    applyChange,
    push,
)
where

import Control.Exception (displayException)
import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT (..), MonadError (catchError), runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Trans (lift)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict)
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import Shelly (Sh, whenM)
import qualified Shelly as Sh
import System.FilePath (takeDirectory, (</>))

-- | The wiki monad, combining error handling with shell operations.
type Wiki a = ReaderT Config (ExceptT WikiError Sh) a

-- | Wiki configuration passed via ReaderT.
type Config = Repo

-- | Run a Wiki action, returning either an error or the result.
runWiki :: Repo -> Wiki a -> IO (Either WikiError a)
runWiki r action =
    runShelly . handleException . runExceptT . runReaderT action $ r
  where
    runShelly = Sh.shelly . Sh.errExit False
    handleException = Sh.handleany_sh (return . Left . InternalError . displayException)

-- | A wiki page identified by its path.
data Page = Page FilePath
    deriving (Generic, Show)

instance FromJSON Page
instance ToJSON Page

-- | A change to a wiki page.
data Change
    = -- | Create or overwrite a page
      Write !Page !Text
    | -- | Replace old text with new text
      Edit !Page !Text !Text
    deriving (Generic, Show)

instance FromJSON Change
instance ToJSON Change

-- | Remote git repository URL.
data RepoUrl = RepoUrl Text
    deriving (Show, Eq)

-- | A locally cloned git repository.
data Repo = Repo
    { repoPath :: !FilePath
    , repoUrl :: !RepoUrl
    }
    deriving (Show)

-- | Wiki errors.
data WikiError
    = ShellError !(Maybe FilePath) !String ![Text] !Int
    | WrongRepo !RepoUrl !FilePath
    | NoSuchDir !FilePath !String
    | PushConflict ![FilePath]
    | ParseError !String !Text
    | WriteError !Page !String
    | InternalError !String
    deriving (Show)

-- | Format error message for AI agent consumption.
verboseError :: WikiError -> Text
verboseError (ShellError mPath cmd args code) =
    "Command failed: "
        <> T.pack cmd
        <> " "
        <> T.unwords args
        <> " (exit code "
        <> T.pack (show code)
        <> ")"
        <> maybe "" (\p -> " in directory " <> T.pack p) mPath
verboseError (WrongRepo (RepoUrl expected) path) =
    "Repository at " <> T.pack path <> " has wrong origin. Expected: " <> expected
verboseError (NoSuchDir path msg) =
    "Directory does not exist: " <> T.pack path <> " (" <> T.pack msg <> ")"
verboseError (PushConflict conflicts) =
    "Push failed due to conflicts in: "
        <> T.intercalate ", " (map T.pack conflicts)
        <> ". Fetch latest changes and retry."
verboseError (ParseError msg _) =
    "Error parsing JSON input: " <> T.pack msg
verboseError (WriteError (Page path) err) =
    "Error writing to " <> T.pack path <> ": " <> T.pack err
verboseError (InternalError msg) =
    "Internal error occurred (" <> T.pack msg <> "). This may be a bug."

-- | Parse newline-delimited JSON changes from text input.
parseChanges :: Text -> Either WikiError [Change]
parseChanges = traverse parseChange . T.lines
  where
    parseChange line = first (flip ParseError line) $ eitherDecodeStrict $ TE.encodeUtf8 line

-- | Clone a repository if it doesn't exist, or update it if it does.
fetch :: Wiki ()
fetch = do
    ifM
        isGitRepo
        resetRepo
        cloneRepo

-- | Apply a change to a page and commit it.
applyChange :: Change -> Wiki ()
applyChange change = do
    case change of
        Write page content -> applyWriteFile page content
        Edit page old new -> applyEditFile page old new
    commit change

status :: Wiki Text
status = git ["status", "--porcelain"]

-- | Push changes to remote, handling conflicts via rebase.
push :: Wiki ()
push = do
    pushed <- zeroExit $ git_ ["push"]
    unless pushed $ do
        git_ ["fetch", "origin"]
        rebased <- zeroExit $ git_ ["rebase", "origin/HEAD"]
        if rebased
            then git_ ["push"]
            else do
                conflicts <- parseConflicts <$> status
                git_ ["rebase", "--abort"]
                throwError $ PushConflict conflicts
  where
    parseConflicts output =
        [ T.unpack (T.drop 3 line)
        | line <- T.lines output
        , "UU " `T.isPrefixOf` line
        ]

-- Internal functions

repo :: Wiki Repo
repo = asks id

git :: [Text] -> Wiki Text
git args = do
    Repo{repoPath} <- repo
    shell (Just repoPath) "git" args

git_ :: [Text] -> Wiki ()
git_ args = () <$ git args

hasStaged :: Wiki Bool
hasStaged = not <$> zeroExit (git_ ["diff", "--cached", "--quiet"])

applyWriteFile :: Page -> Text -> Wiki ()
applyWriteFile page content = do
    r <- repo
    let outputPath = pageFullPath r page
        outputDir = takeDirectory outputPath
    lsh ("mkdir -p " <> outputDir) $ Sh.mkdir_p outputDir
    lsh ("cat > " <> outputPath) $ Sh.writefile outputPath content
  where
    lsh cmdLog = liftSh (T.pack cmdLog) (WriteError page)

applyEditFile :: Page -> Text -> Text -> Wiki ()
applyEditFile = undefined

commit :: Change -> Wiki ()
commit change = do
    git_ ["add", pagePath]
    whenM hasStaged $
        git_ ["commit", "-m", commitMsg]
  where
    (pagePath, commitMsg) = case change of
        Write (Page (T.pack -> p)) _ -> (p, "Added " <> p)
        Edit (Page (T.pack -> p)) _ _ -> (p, "Edited " <> p)

liftSh :: Text -> (String -> WikiError) -> Sh a -> Wiki a
liftSh cmdLog err action = do
    logCmd cmdLog
    lift $ ExceptT $ Sh.catchany_sh (Right <$> action) onError
  where
    onError = return . Left . err . displayException

logCmd :: Text -> Wiki ()
logCmd msg = liftIO $ TIO.putStrLn $ "$ " <> msg

cd :: FilePath -> Wiki ()
cd path = liftSh (T.pack $ "cd " <> path) (NoSuchDir path) $ Sh.cd path

shell :: Maybe FilePath -> String -> [Text] -> Wiki Text
shell cwd cmd args = do
    case cwd of
        Just path -> cd path
        Nothing -> return ()
    logCmd $ T.pack cmd <> " " <> T.unwords args
    output <- lift . lift $ Sh.run cmd args
    exitCode <- lift . lift $ Sh.lastExitCode
    when (exitCode /= 0) $ throwError $ ShellError cwd cmd args exitCode
    return output

shell_ :: Maybe FilePath -> String -> [Text] -> Wiki ()
shell_ cwd cmd args = () <$ shell cwd cmd args

cloneRepo :: Wiki ()
cloneRepo = do
    Repo{repoUrl, repoPath} <- repo
    let (RepoUrl url) = repoUrl
    -- Cannot use `git` function because dir isn't there yet.
    gitNoDir ["clone", url, T.pack repoPath]
  where
    gitNoDir = shell_ Nothing "git"

zeroExit :: Wiki a -> Wiki Bool
zeroExit m = catchError (True <$ m) (const $ return False)

isGitRepo :: Wiki Bool
isGitRepo = do
    Repo{repoPath} <- repo
    (test_d repoPath) `andM` (zeroExit $ git ["rev-parse", "--git-dir"])
  where
    test_d path =
        liftSh (T.pack $ "test " <> path) InternalError $ Sh.test_d path

assertCorrectRepo :: Wiki ()
assertCorrectRepo = do
    Repo{repoUrl, repoPath} <- repo
    whenM (not <$> isCorrectRepo repoUrl) $
        throwError $
            WrongRepo repoUrl repoPath
  where
    isCorrectRepo repoUrl = do
        output <- git ["remote", "get-url", "origin"]
        return $ (RepoUrl $ T.strip output) == repoUrl

resetRepo :: Wiki ()
resetRepo = do
    assertCorrectRepo
    git_ ["fetch", "origin"]
    git_ ["reset", "--hard", "origin/HEAD"]

pageFullPath :: Repo -> Page -> FilePath
pageFullPath Repo{repoPath} (Page pagePath) = repoPath </> pagePath

ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM cond t f = cond >>= \b -> if b then t else f

andM :: (Monad m) => m Bool -> m Bool -> m Bool
andM ma mb = ma >>= \a -> if a then mb else return False
