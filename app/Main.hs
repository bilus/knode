{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.IO as TIO
import Knode.Wiki

main :: IO ()
main = do
    result <- runWiki repo $ do
        fetch
        readFromStdio >>= mapM_ applyChange
        push
    case result of
        Right _ -> putStrLn "Success!"
        Left e -> TIO.putStrLn $ verboseError e
  where
    readFromStdio = liftIO TIO.getContents >>= liftEither . parseChanges
    repoPath = "/tmp/repo"
    repoUrl = RepoUrl "git@github.com:bilus/knode-test.git"
    repo = Repo{..}
