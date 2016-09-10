{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module YX.Type.CommandType
  where

import Prelude (Bounded, Enum)

import Control.Applicative (empty, pure)
import Data.Eq (Eq)
import Data.Function (($), (.))
import Data.Ord (Ord)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.String (IsString)
import GHC.Generics (Generic)
import Text.Read (Read)
import Text.Show (Show)

import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON))
import qualified Data.Aeson as Aeson (withText)
import qualified Data.CaseInsensitive as CI (FoldCase, mk)
import Data.Default (Default(def))
import Data.Text (Text)


data CommandType = Command | Symlink | Alias
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

-- | By default we assume that user entered @type: Command@ in a configuration
-- file.
--
-- @'def' = 'Command'@
instance Default CommandType where
    def = Command

fromString :: (Eq s, IsString s, CI.FoldCase s) => s -> Maybe CommandType
fromString s = case CI.mk s of
    "command" -> Just Command
    "symlink" -> Just Symlink
    "alias" -> Just Alias
    _ -> Nothing

toString :: IsString s => CommandType -> s
toString = \case
    Command -> "Command"
    Symlink -> "Symlink"
    Alias -> "Alias"

fromText :: Text -> Maybe CommandType
fromText = fromString

toText :: CommandType -> Text
toText = toString

instance FromJSON CommandType where
    parseJSON = Aeson.withText "CommandType" $ maybe empty pure . fromText

instance ToJSON CommandType where
    toJSON = toJSON . toText
