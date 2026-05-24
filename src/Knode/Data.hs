{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Knode.Data (Page (..), Change (..), PageOp (..), Query (..), QueryResult (..), WikiSource (..), WikiConfig (..), AppError (..), ChangeError (..), pageFullPath, parseChanges) where

import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict)
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import System.FilePath ((</>))

-- | A wiki page identified by its path.
data Page = Page FilePath
    deriving (Generic, Show, Eq)

instance FromJSON Page
instance ToJSON Page

data Change = PageChange !Page !PageOp
    deriving (Generic, Show, Eq)

instance FromJSON Change
instance ToJSON Change

-- | A change to a wiki page.
data PageOp
    = -- | Create or overwrite a page
      Overwrite !Text
    | -- | Replace old text with new text
      ReplaceAll !Text !Text
    deriving (Generic, Show, Eq)

instance FromJSON PageOp
instance ToJSON PageOp

-- | A query to read wiki content.
data Query
    = -- | Read a page's content
      ReadPage !Page
    | -- | Search for text in a page
      GrepPage !Page !Text
    deriving (Generic, Show, Eq)

instance FromJSON Query
instance ToJSON Query

-- | Result of a query.
data QueryResult
    = -- | Content of a page
      PageContent !Text
    | -- | Lines matching the grep pattern
      GrepMatches ![Text]
    | -- | Page not found
      PageNotFound
    deriving (Generic, Show, Eq)

instance FromJSON QueryResult
instance ToJSON QueryResult

-- | Where the wiki content lives (e.g., a git remote URL).
data WikiSource = WikiSource Text
    deriving (Show, Eq)

-- | Local wiki workspace configuration.
data WikiConfig = WikiConfig
    { wikiPath :: !FilePath
    , wikiSource :: !WikiSource
    , wikiMaxRetries :: !Int
    }
    deriving (Show)

-- | Application errors.
data AppError
    = CommandFailed !(Maybe FilePath) !String ![String] !Int
    | IOError !FilePath !Text
    | InternalError !Text
    | WrongSource !WikiSource !FilePath
    | PublishConflict ![FilePath]
    | ParseError !String !Text
    | WrongPath !FilePath
    | ChangesNotApplied
    deriving (Show, Eq)

-- | Errors when applying changes.
data ChangeError
    = EditNotFound !Page !Text
    | WriteError !Page !Text
    deriving (Show, Eq)

-- | Full path to a page in the workspace.
pageFullPath :: WikiConfig -> Page -> FilePath
pageFullPath WikiConfig{wikiPath} (Page pagePath) = wikiPath </> pagePath

-- | Parse newline-delimited JSON changes from text input.
parseChanges :: Text -> Either AppError [Change]
parseChanges = traverse parseChange . T.lines
  where
    parseChange line = first (flip ParseError line) $ eitherDecodeStrict $ TE.encodeUtf8 line
