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
    Repo,

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

import Control.Exception (SomeException, displayException)
import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT (..), MonadError (catchError), runExceptT, throwError)
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

-- | The wiki monad, combining error handling with shell operations.
type Wiki a = ExceptT WikiError Sh a

-- | Run a Wiki action, returning either an error or the result.
runWiki :: Wiki a -> IO (Either WikiError a)
runWiki =
    runShelly . handleException . runExceptT
  where
    runShelly = Sh.shelly . Sh.errExit False
    handleException = Sh.handleany_sh (return . Left . InternalError)

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
    deriving (Show)

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
    | InternalError !SomeException
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
verboseError (InternalError ex) =
    "Internal error occurred (" <> T.pack (displayException ex) <> "). This may be a bug."

-- | Parse newline-delimited JSON changes from text input.
parseChanges :: Text -> Either WikiError [Change]
parseChanges = traverse parseChange . T.lines
  where
    parseChange line = first (flip ParseError line) $ eitherDecodeStrict $ TE.encodeUtf8 line

-- | Clone a repository if it doesn't exist, or update it if it does.
fetch :: RepoUrl -> FilePath -> Wiki Repo
fetch repoUrl repoPath =
    ifM
        (isGitRepo repoPath)
        (resetRepo repoUrl repoPath)
        (cloneRepo repoUrl repoPath)

-- | Apply a change to a page and commit it.
applyChange :: Repo -> Change -> Wiki ()
applyChange repo change = do
    case change of
        Write page content -> applyWriteFile repo page content
        Edit page old new -> applyEditFile repo page old new
    commit repo change

status :: FilePath -> Wiki Text
status repoPath = git ["status", "--porcelain"]
  where
    git = shell (Just repoPath) "git"

-- | Push changes to remote, handling conflicts via rebase.
push :: Repo -> Wiki ()
push Repo{..} = do
    pushed <- success $ git_ ["push"]
    unless pushed $ do
        git_ ["fetch", "origin"]
        rebased <- success $ git_ ["rebase", "origin/HEAD"]
        if rebased
            then git_ ["push"]
            else do
                conflicts <- parseConflicts <$> status repoPath
                git_ ["rebase", "--abort"]
                throwError $ PushConflict conflicts
  where
    git_ = shell_ (Just repoPath) "git"
    parseConflicts output =
        [ T.unpack (T.drop 3 line)
        | line <- T.lines output
        , "UU " `T.isPrefixOf` line
        ]

-- Internal functions

hasStaged :: FilePath -> Wiki Bool
hasStaged repoPath = not <$> success (shell_ (Just repoPath) "git" ["diff", "--cached", "--quiet"])

applyWriteFile :: Repo -> Page -> Text -> Wiki ()
applyWriteFile repo page content = do
    lsh ("mkdir -p " <> outputDir) $ Sh.mkdir_p outputDir
    lsh ("cat > " <> outputPath) $ Sh.writefile outputPath content
  where
    lsh cmdLog = liftSh (T.pack cmdLog) (WriteError page)
    outputPath = pageFullPath repo page
    outputDir = takeDirectory outputPath

applyEditFile :: Repo -> Page -> Text -> Text -> Wiki ()
applyEditFile = undefined

commit :: Repo -> Change -> Wiki ()
commit Repo{..} change = do
    git_ ["add", pagePath]
    whenM (hasStaged repoPath) $
        git_ ["commit", "-m", commitMsg]
  where
    git_ = shell_ (Just repoPath) "git"
    (pagePath, commitMsg) = case change of
        Write (Page (T.pack -> p)) _ -> (p, "Added " <> p)
        Edit (Page (T.pack -> p)) _ _ -> (p, "Edited " <> p)

liftSh :: Text -> (String -> WikiError) -> Sh a -> Wiki a
liftSh cmdLog err action = do
    logCmd cmdLog
    ExceptT $ Sh.catchany_sh (Right <$> action) (return . Left . err . displayException)

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
    output <- lift $ Sh.run cmd args
    exitCode <- lift $ Sh.lastExitCode
    when (exitCode /= 0) $ throwError $ ShellError cwd cmd args exitCode
    return output

shell_ :: Maybe FilePath -> String -> [Text] -> Wiki ()
shell_ cwd cmd args = () <$ shell cwd cmd args

cloneRepo :: RepoUrl -> FilePath -> Wiki Repo
cloneRepo repoUrl@(RepoUrl repo) repoPath = do
    shell_ Nothing "git" ["clone", repo, T.pack repoPath]
    return Repo{..}

success :: Wiki a -> Wiki Bool
success m = catchError (True <$ m) (const $ return False)

isGitRepo :: FilePath -> Wiki Bool
isGitRepo repoPath = do
    exists <- lift $ Sh.test_d repoPath
    if not exists
        then return False
        else success $ shell (Just repoPath) "git" ["rev-parse", "--git-dir"]

isCorrectRepo :: RepoUrl -> FilePath -> Wiki Bool
isCorrectRepo (RepoUrl repoUrl) repoPath = do
    url <- shell (Just repoPath) "git" ["remote", "get-url", "origin"]
    return $ T.strip url == repoUrl

resetRepo :: RepoUrl -> FilePath -> Wiki Repo
resetRepo repoUrl repoPath = do
    whenM (not <$> isCorrectRepo repoUrl repoPath) $
        throwError $
            WrongRepo repoUrl repoPath
    git ["fetch", "origin"]
    git ["reset", "--hard", "origin/HEAD"]
    return Repo{..}
  where
    git = shell_ (Just repoPath) "git"

pageFullPath :: Repo -> Page -> FilePath
pageFullPath Repo{repoPath} (Page pagePath) = repoPath </> pagePath

ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM cond t f = cond >>= \b -> if b then t else f
