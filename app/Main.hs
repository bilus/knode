{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

-- import Lib

import Control.Monad (when)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Control.Monad.Trans (lift)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Shelly (Sh)
import qualified Shelly as Sh
import System.FilePath ((</>))

data Page = Page FilePath

-- | Change to a wiki page.
data Change
    = Write !Page !Text
    | Edit !Page !Text !Text

-- | Remote git repository url.
data RepoURL = RepoURL Text
    deriving (Show)

-- | Information about a locally cloned git repository.
data ClonedRepo = ClonedRepo
    { repoPath :: !FilePath
    , repoUrl :: !RepoURL
    }
    deriving (Show)

-- | Apply a change to a page in the repository.
applyChange :: ClonedRepo -> Change -> Sh ()
applyChange repo (Write page content) = write repo page content
applyChange repo (Edit page oldContent newContent) = edit repo page oldContent newContent

-- | Stage and commit a change to the repository.
commit :: ClonedRepo -> Change -> Sh ()
commit repo@(ClonedRepo{..}) change = do
    Sh.cd repoPath
    Sh.run_ "git" ["add", T.pack pagePath]
    Sh.run_ "git" ["commit", "-m", commitMsg]
  where
    ((Page pagePath), commitMsg) = case change of
        Write page _ -> (page, "Added " <> T.pack (pageFullPath repo page))
        Edit page _ _ -> (page, "Edited " <> T.pack (pageFullPath repo page))

-- | Write content to a page, overwriting any existing content.
write :: ClonedRepo -> Page -> Text -> Sh ()
write repo page content = do
    Sh.liftIO $ BS.writeFile (pageFullPath repo page) (TE.encodeUtf8 content)

-- | Replace old content with new content in a page.
edit :: ClonedRepo -> Page -> Text -> Text -> Sh ()
edit = undefined

data ShellError = ShellError {exitCode :: Int}
    deriving (Show)

-- | Run a shell command, returning stdout or an error with exit code.
shell :: (Maybe FilePath) -> String -> [Text] -> Sh (Either ShellError Text)
shell cwd cmd args = Sh.errExit False $ do
    case cwd of
        Just path -> Sh.cd path -- TODO: Catch exception.
        Nothing -> return ()
    output <- Sh.run cmd args
    exitCode <- Sh.lastExitCode
    return $
        if exitCode /= 0
            then Left (ShellError exitCode)
            else
                Right output

-- | Run a shell command, discarding stdout.
shell_ :: (Maybe FilePath) -> String -> [Text] -> Sh (Either ShellError ())
shell_ cwd cmd args = fmap (() <$) $ shell cwd cmd args

-- | Clone a git repository to the specified path.
cloneRepo :: RepoURL -> FilePath -> Sh (Either ShellError ClonedRepo)
cloneRepo repoUrl@(RepoURL repo) repoPath = do
    result <- shell_ Nothing "git" ["clone", repo, T.pack repoPath]
    return $ (const ClonedRepo{..}) <$> result

-- | Fetch and reset an existing repository to match origin/HEAD.
rebaseRepo :: RepoURL -> FilePath -> Sh (Either ShellError ClonedRepo)
rebaseRepo repoUrl repoPath = runExceptT $ do
    exists <- lift $ Sh.test_d repoPath
    when (not exists) $ throwError $ ShellError{exitCode = 1} -- TODO: Sum type to give an error message
    _ <- ExceptT $ shell (Just repoPath) "git" ["fetch", "origina"]
    _ <- ExceptT $ shell (Just repoPath) "git" ["reset", "--hard", "origin/HEAD"]
    return ClonedRepo{..}

-- | Clone a repository if it doesn't exist, or update it if it does.
ensureRepoCloned :: RepoURL -> FilePath -> IO (Either Text ClonedRepo)
ensureRepoCloned repoUrl@(RepoURL repo) repoPath = Sh.shelly $ do
    -- TODO: Check if the repo already exists and is fresh.
    Sh.run_ "git" ["clone", repo, T.pack repoPath]
    return $ Right ClonedRepo{..}

-- | Get the full filesystem path for a page within a repository.
pageFullPath :: ClonedRepo -> Page -> FilePath
pageFullPath (ClonedRepo{repoPath}) (Page pagePath) =
    repoPath </> pagePath

-- | Entry point.
main :: IO ()
main = Sh.shelly $ do
    output <- shell (Just "/tmp/repo") "git" ["status"]
    write (ClonedRepo{repoPath = ".", repoUrl = RepoURL ""}) (Page "test.txt") "Hello, World!"
    Sh.liftIO $ print output

---------
-- Sample data for testing

sampleRepoUrl = RepoURL "git@github.com:bilus/fencer.git"

sampleRepo =
    ClonedRepo
        { repoPath = "/tmp/repo"
        , repoUrl = sampleRepoUrl
        }
sampleChange = Write (Page "test.txt") "Hello, World!"
