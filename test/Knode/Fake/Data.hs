{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Knode.Fake.Data (
    Author (..),
    History,
    FakeFS (..),
    emptyFS,
    addToHistory,
    isConflict,
) where

import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Knode.Data (Change)

newtype Author = Author String
    deriving (Show, Eq)

type History = [(Author, Change)]

data FakeFS = FakeFS
    { fsFiles :: !(Map FilePath Text)
    , fsHistory :: !History
    }
    deriving (Show, Eq)

emptyFS :: FakeFS
emptyFS = FakeFS Map.empty []

addToHistory :: Author -> [Change] -> FakeFS -> FakeFS
addToHistory author changes fs =
    fs{fsHistory = fsHistory fs ++ map (author,) changes}

isConflict :: History -> History -> Bool
isConflict sourceHistory targetHistory =
    not (targetHistory `isPrefixOf` sourceHistory)
