{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

-- import Lib

import Control.Exception (SomeException, displayException)
import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT (..), MonadError (catchError), liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
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

type App a = ExceptT AppError Sh a

runApp :: App a -> IO (Either AppError a)
runApp =
    runShelly . handleException . runExceptT
  where
    runShelly = Sh.shelly . (Sh.errExit False)
    handleException action =
        Sh.handleany_sh (return . Left . InternalError) action

data Page
    = Page FilePath
    deriving (Generic, Show)

instance FromJSON Page
instance ToJSON Page

-- | Change to a wiki page.
data Change
    = Write !Page !Text
    | Edit !Page !Text !Text
    deriving (Generic, Show)

instance FromJSON Change
instance ToJSON Change

-- | Remote git repository url.
data RepoUrl = RepoUrl Text
    deriving (Show)

-- | Information about a locally cloned git repository.
data ClonedRepo = ClonedRepo
    { repoPath :: !FilePath
    , repoUrl :: !RepoUrl
    }
    deriving (Show)

-- | Apply a change to a page in the repository.
applyChange :: ClonedRepo -> Change -> App ()
applyChange repo (Write page content) = applyWrite repo page content
applyChange repo (Edit page oldContent newContent) = applyEdit repo page oldContent newContent

-- | Stage and commit a change to the repository.
commit :: ClonedRepo -> Change -> App ()
commit (ClonedRepo{..}) change = lift $ do
    Sh.cd repoPath
    Sh.run_ "git" ["add", pagePath]
    Sh.run_ "git" ["commit", "-m", commitMsg]
  where
    (pagePath, commitMsg) = case change of
        Write (Page (T.pack -> p)) _ -> (p, "Added " <> p)
        Edit (Page (T.pack -> p)) _ _ -> (p, "Edited " <> p)

-- | Write content to a page, overwriting any existing content.
applyWrite :: ClonedRepo -> Page -> Text -> App ()
applyWrite repo page content = do
    lsh ("mkdir -p" <> outputDir) $ Sh.mkdir_p outputDir
    lsh ("cat > " <> outputPath) $ Sh.writefile outputPath content
  where
    lsh cmdLog =
        liftSh (T.pack cmdLog) (WriteError page)
    outputPath = pageFullPath repo page
    outputDir = takeDirectory outputPath

-- | Replace old content with new content in a page.
applyEdit :: ClonedRepo -> Page -> Text -> Text -> App ()
applyEdit = undefined

data AppError
    = ShellError !(Maybe FilePath) !String ![Text] !Int
    | WrongRepo !RepoUrl !FilePath
    | NoSuchDir !FilePath !String
    | PushFailed ![FilePath]
    | InputParseError !String !Text
    | ShellyException !String
    | WriteError !Page !String
    | InternalError !SomeException
    deriving (Show)

-- | Format error message for AI agent consumption.
verboseError :: AppError -> Text
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
verboseError (PushFailed conflicts) =
    "Push failed due to conflicts in: "
        <> T.intercalate ", " (map T.pack conflicts)
        <> ". Fetch latest changes and retry."
verboseError (InputParseError msg _) =
    "Error parsing JSON input: "
        <> T.pack msg
verboseError (ShellyException msg) =
    "Shell command resulted in an unexpected error: "
        <> T.pack msg
verboseError (WriteError (Page path) err) =
    "Error writing to " <> T.pack path <> ": " <> T.pack err
verboseError (InternalError ex) =
    "Internal error occurred (" <> (T.pack $ displayException ex) <> "). This may be a bug."

-- | Lift a Sh action into App, converting exceptions to the given error.
liftSh :: Text -> (String -> AppError) -> Sh a -> App a
liftSh cmdLog err action = do
    logCmd cmdLog
    ExceptT $
        Sh.catchany_sh
            (Right <$> action)
            -- TOdo: Do not display stack trace unless --debug
            (\e -> return $ Left $ err $ displayException e)

logCmd :: Text -> App ()
logCmd msg =
    liftIO $ TIO.putStrLn $ "$ " <> msg

cd :: FilePath -> App ()
cd path = liftSh (T.pack $ "cd " <> path) (NoSuchDir path) $ Sh.cd path

-- | Run a shell command, returning stdout or an error with exit code.
shell :: (Maybe FilePath) -> String -> [Text] -> App Text
shell cwd cmd args = do
    case cwd of
        Just path -> cd path
        Nothing -> return ()
    logCmd $ (T.pack cmd) <> " " <> T.unwords args
    output <- lift $ Sh.run cmd args
    exitCode <- lift $ Sh.lastExitCode
    when (exitCode /= 0) $ throwError $ ShellError cwd cmd args exitCode
    return output

-- | Run a shell command, discarding stdout.
shell_ :: (Maybe FilePath) -> String -> [Text] -> App ()
shell_ cwd cmd args = do
    _ <- shell cwd cmd args
    return ()

-- | Clone a git repository to the specified path.
cloneRepo :: RepoUrl -> FilePath -> App ClonedRepo
cloneRepo repoUrl@(RepoUrl repo) repoPath = do
    git ["clone", repo, T.pack repoPath]
    return ClonedRepo{..}
  where
    git = shell_ Nothing "git"

-- | True if action successful.
success :: App a -> App Bool
success m = catchError (True <$ m) (const $ return False)

-- | Is the directory a git repo.
isGitRepo :: FilePath -> App Bool
isGitRepo repoPath = do
    exists <- lift $ Sh.test_d repoPath
    if (not exists)
        then
            return False
        else
            success $ git ["rev-parse", "--git-dir"]
  where
    git = shell (Just repoPath) "git"

-- | Is the directory this git repo. Error if not git repo or dir doesn't exist.
isCorrectRepo :: RepoUrl -> FilePath -> App Bool
isCorrectRepo (RepoUrl repoUrl) repoPath = do
    url <- git ["remote", "get-url", "origin"]
    return $ T.strip url == repoUrl
  where
    git = shell (Just repoPath) "git"

-- | Fetch and reset an existing repository to match origin/HEAD.
resetRepo :: RepoUrl -> FilePath -> App ClonedRepo
resetRepo repoUrl repoPath = do
    whenM
        (not <$> isCorrectRepo repoUrl repoPath)
        (throwError $ WrongRepo repoUrl repoPath)

    git ["fetch", "origin"]
    git ["reset", "--hard", "origin/HEAD"]
    return ClonedRepo{..}
  where
    git = shell_ (Just repoPath) "git"

-- | Clone a repository if it doesn't exist, or update it if it does.
prepareRepo :: RepoUrl -> FilePath -> App ClonedRepo
prepareRepo repoUrl repoPath =
    ifM
        (isGitRepo repoPath)
        (resetRepo repoUrl repoPath)
        (cloneRepo repoUrl repoPath)

push :: ClonedRepo -> App ()
push ClonedRepo{..} = do
    pushed <- success $ git_ ["push"]
    when pushed $ return ()
    git_ ["fetch", "origin"]
    rebased <- success $ git ["rebase", "origin/HEAD"]
    when rebased $ git_ ["push"]
    conflicts <- parseConflicts <$> git ["status", "--porcelain"]
    unless (null conflicts) $ throwError $ PushFailed conflicts
  where
    git_ = shell_ (Just repoPath) "git"
    git = shell (Just repoPath) "git"
    parseConflicts :: Text -> [FilePath]
    parseConflicts output =
        -- Lines starting with "UU " are unmerged (conflict)
        [ T.unpack (T.drop 3 line)
        | line <- T.lines output
        , "UU " `T.isPrefixOf` line
        ]

-- | Get the full filesystem path for a page within a repository.
pageFullPath :: ClonedRepo -> Page -> FilePath
pageFullPath (ClonedRepo{repoPath}) (Page pagePath) =
    repoPath </> pagePath

ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM cond t f = cond >>= \b -> if b then t else f

parseChanges :: Text -> (Either AppError [Change])
parseChanges =
    (traverse parseChange) . T.lines
  where
    parseChange :: Text -> Either AppError Change
    parseChange line =
        first (flip InputParseError line) $ eitherDecodeStrict $ TE.encodeUtf8 line

-- | Entry point.
main :: IO ()
main = do
    result <- runApp $ do
        input <- liftIO TIO.getContents
        changes <- liftEither $ parseChanges input
        repo <- prepareRepo repoUrl repoPath
        mapM_ (applyChange repo) changes
        mapM_ (commit repo) changes
        push repo
    case result of
        Right _ -> putStrLn "Success!"
        Left e -> TIO.putStrLn $ verboseError e
  where
    repoPath = "/tmp/repo"
    repoUrl = sampleRepoUrl

---------
-- Sample data for testing

sampleRepoUrl :: RepoUrl
sampleRepoUrl = RepoUrl "git@github.com:bilus/knode-test.git"

_sampleRepo :: ClonedRepo
_sampleRepo =
    ClonedRepo
        { repoPath = "/tmp/repo"
        , repoUrl = sampleRepoUrl
        }

_sampleChange :: Change
_sampleChange = Write (Page "test.txt") "Hello, World!"
