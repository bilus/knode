module Knode.Capabilities (Config (..), Workspace (..), Wiki (..), Reporting (..)) where

import Data.Text (Text)
import Knode.Data (Change (..), ChangeError, WikiConfig)

class (Monad m) => Config m where
    config :: m WikiConfig

class (Monad m, Config m) => Workspace m where
    apply :: [Change] -> m [ChangeError]

class (Monad m, Config m) => Wiki m where
    sync :: m ()
    stage :: [Change] -> Text -> m ()
    publish :: m ()

class (Monad m) => Reporting m where
    reportChangeError :: ChangeError -> m ()
