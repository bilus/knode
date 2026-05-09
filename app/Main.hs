{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- import Lib
import Shelly

main :: IO ()
main = shelly $ do
  output <- run "git" ["status"]
  liftIO $ print output
