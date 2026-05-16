{-# LANGUAGE OverloadedStrings #-}

module Test.Helpers.Git (
    initBareRepo,
    initRepoWithFile,
    gitClone,
    gitCommitFile,
) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Shelly as Sh
import System.FilePath ((</>))

initBareRepo :: FilePath -> IO ()
initBareRepo dir = Sh.shelly $ Sh.run_ "git" ["init", "--bare", T.pack dir]

initRepoWithFile :: FilePath -> FilePath -> Text -> IO ()
initRepoWithFile repo path content = Sh.shelly $ do
    Sh.run_ "git" ["init", T.pack repo]
    Sh.writefile (Sh.fromText $ T.pack $ repo </> path) content
    Sh.chdir (Sh.fromText $ T.pack repo) $ do
        Sh.run_ "git" ["add", T.pack path]
        Sh.run_ "git" ["commit", "-m", "Initial commit"]

gitClone :: Text -> FilePath -> IO ()
gitClone source dest = Sh.shelly $ Sh.run_ "git" ["clone", source, T.pack dest]

gitCommitFile :: FilePath -> FilePath -> Text -> Text -> IO ()
gitCommitFile repo path content message = Sh.shelly $ do
    Sh.writefile (Sh.fromText $ T.pack $ repo </> path) content
    Sh.chdir (Sh.fromText $ T.pack repo) $ do
        Sh.run_ "git" ["add", T.pack path]
        Sh.run_ "git" ["commit", "-m", message]
