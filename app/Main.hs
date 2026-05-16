{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.IO as TIO
import Knode.App (AppM, Env (..), defaultReportingHandle, defaultWikiHandle, defaultWorkspaceHandle, runApp)
import Knode.Data (Change, WikiConfig (..), WikiSource (..), parseChanges)
import Knode.Service (makeChanges)

main :: IO ()
main = do
    result <- runApp env $ do
        changes <- readFromStdio
        makeChanges "Update wiki" changes
    case result of
        Left err -> print err
        Right () -> putStrLn "Success!"
  where
    readFromStdio :: AppM [Change]
    readFromStdio = liftIO TIO.getContents >>= liftEither . parseChanges
    wikiPath = "/tmp/wiki"
    wikiSource = WikiSource "git@github.com:bilus/knode-test.git"
    wikiMaxRetries = 3
    env =
            Env
                { envWiki = WikiConfig{..}
                , envWorkspaceHandle = defaultWorkspaceHandle
                , envWikiHandle = defaultWikiHandle
                , envReportingHandle = defaultReportingHandle
                }
