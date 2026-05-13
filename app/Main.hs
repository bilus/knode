{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

-- import Lib

import Control.Monad (when)
import Control.Monad.Except (ExceptT (..), MonadError (catchError), runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Shelly (Sh, whenM)
import qualified Shelly as Sh
import System.FilePath ((</>))

type App a = ExceptT AppError Sh a

runApp :: App a -> IO (Either AppError a)
runApp = Sh.shelly . (Sh.errExit False) . runExceptT

data Page = Page FilePath

-- | Change to a wiki page.
data Change
    = Write !Page !Text
    | Edit !Page !Text !Text

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
applyChange repo (Write page content) = write repo page content
applyChange repo (Edit page oldContent newContent) = edit repo page oldContent newContent

-- | Stage and commit a change to the repository.
commit :: ClonedRepo -> Change -> App ()
commit repo@(ClonedRepo{..}) change = lift $ do
    Sh.cd repoPath
    Sh.run_ "git" ["add", T.pack pagePath]
    Sh.run_ "git" ["commit", "-m", commitMsg]
  where
    ((Page pagePath), commitMsg) = case change of
        Write page _ -> (page, "Added " <> T.pack (pageFullPath repo page))
        Edit page _ _ -> (page, "Edited " <> T.pack (pageFullPath repo page))

-- | Write content to a page, overwriting any existing content.
write :: ClonedRepo -> Page -> Text -> App ()
write repo page content = do
    liftIO $ BS.writeFile (pageFullPath repo page) (TE.encodeUtf8 content)

-- | Replace old content with new content in a page.
edit :: ClonedRepo -> Page -> Text -> Text -> App ()
edit = undefined

data AppError
    = ShellError {exitCode :: Int}
    | WrongRepo RepoUrl FilePath
    | NoSuchDir FilePath
    deriving (Show)

-- | Lift a Sh action into App, converting exceptions to the given error.
liftSh :: AppError -> Sh a -> App a
liftSh err action =
    ExceptT $
        Sh.catchany_sh
            (Right <$> action)
            (const $ return $ Left err)

-- | Run a shell command, returning stdout or an error with exit code.
shell :: (Maybe FilePath) -> String -> [Text] -> App Text
shell cwd cmd args = do
    case cwd of
        Just path -> liftSh (NoSuchDir path) $ Sh.cd path
        Nothing -> return ()
    output <- lift $ Sh.run cmd args
    exitCode <- lift $ Sh.lastExitCode
    when (exitCode /= 0) $ throwError $ ShellError exitCode
    return output

-- | Run a shell command, discarding stdout.
shell_ :: (Maybe FilePath) -> String -> [Text] -> App ()
shell_ cwd cmd args = do
    _ <- shell cwd cmd args
    return ()

-- | Clone a git repository to the specified path.
cloneRepo :: RepoUrl -> FilePath -> App ClonedRepo
cloneRepo repoUrl@(RepoUrl repo) repoPath = do
    shell_ Nothing "git" ["clone", repo, T.pack repoPath]
    return ClonedRepo{..}

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
            success $ shell (Just repoPath) "git" ["rev-parse", "--git-dir"]

-- | Is the directory this git repo. Error if not git repo or dir doesn't exist.
isThisRepo :: RepoUrl -> FilePath -> App Bool
isThisRepo (RepoUrl repoUrl) repoPath = do
    url <- shell (Just repoPath) "git" ["remote", "get-url", "origin"]
    return $ T.strip url == repoUrl

-- | Fetch and reset an existing repository to match origin/HEAD.
resetRepo :: RepoUrl -> FilePath -> App ClonedRepo
resetRepo repoUrl repoPath = do
    whenM
        (not <$> (isThisRepo repoUrl repoPath))
        (throwError $ WrongRepo repoUrl repoPath)

    shell_ (Just repoPath) "git" ["fetch", "origin"]
    shell_ (Just repoPath) "git" ["reset", "--hard", "origin/HEAD"]
    return ClonedRepo{..}

-- | Clone a repository if it doesn't exist, or update it if it does.
prepareRepo :: RepoUrl -> FilePath -> App ClonedRepo
prepareRepo repoUrl repoPath =
    ifM
        (isGitRepo repoPath)
        (resetRepo repoUrl repoPath)
        (cloneRepo repoUrl repoPath)

-- | Get the full filesystem path for a page within a repository.
pageFullPath :: ClonedRepo -> Page -> FilePath
pageFullPath (ClonedRepo{repoPath}) (Page pagePath) =
    repoPath </> pagePath

ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM cond t f = cond >>= \b -> if b then t else f

-- | Entry point.
main :: IO ()
main = do
    result <- runApp $ do
        repo <- prepareRepo repoUrl repoPath
        liftIO $ putStrLn (show repo)

    case result of
        Right _ -> putStrLn "Success!"
        Left e -> putStrLn $ show e
  where
    repoPath = "/tmp/repo"
    repoUrl = sampleRepoUrl

---------
-- Sample data for testing

sampleRepoUrl = RepoUrl "git@github.com:bilus/fencer.git"

sampleRepo =
    ClonedRepo
        { repoPath = "/tmp/repo"
        , repoUrl = sampleRepoUrl
        }
sampleChange = Write (Page "test.txt") "Hello, World!"
