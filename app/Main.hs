{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.IO as TIO
import Knode.Wiki

main :: IO ()
main = do
    result <- runWiki $ do
        input <- liftIO TIO.getContents
        changes <- liftEither $ parseChanges input
        repo <- fetch repoUrl repoPath
        mapM_ (applyChange repo) changes
        push repo
    case result of
        Right _ -> putStrLn "Success!"
        Left e -> TIO.putStrLn $ verboseError e
  where
    repoPath = "/tmp/repo"
    repoUrl = RepoUrl "git@github.com:bilus/knode-test.git"
