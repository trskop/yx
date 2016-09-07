{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module YX.Type.BuildTool
  where

import Prelude (Bounded, Enum)

import Control.Applicative (empty, pure)
import Data.Eq (Eq)
import Data.Function (($), (.))
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Ord (Ord)
import Data.String (IsString)
import GHC.Generics (Generic)
import Text.Read (Read)
import Text.Show (Show)

import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON))
import qualified Data.Aeson as Aeson (withText)
import qualified Data.CaseInsensitive as CI (FoldCase, mk)
import Data.Specifiable (Specifiable)
import Data.Text (Text)


type SomeBuildTool = Specifiable Text BuildTool

data BuildTool
    = Cabal
    | Stack
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

-- | Convert 'BuildTool' in to a generic string.
toString :: IsString s => BuildTool -> s
toString = \case
    Cabal -> "Cabal"
    Stack -> "Stack"
{-# INLINEABLE toString #-}

-- | Convert a generic string in to a 'BuildTool' in a case insensitive maner.
-- 'Nothing' is returned when string value is not recognized.
fromString :: (Eq s, IsString s, CI.FoldCase s) => s -> Maybe BuildTool
fromString s = case CI.mk s of
    "cabal" -> Just Cabal
    "stack" -> Just Stack
    _ -> Nothing
{-# INLINEABLE fromString #-}

toText :: BuildTool -> Text
toText = toString
{-# INLINE toText #-}

fromText :: Text -> Maybe BuildTool
fromText = fromString
{-# INLINE fromText #-}

instance FromJSON BuildTool where
    parseJSON = Aeson.withText "BuildTool" $ maybe empty pure . fromText

instance ToJSON BuildTool where
    toJSON = toJSON . toText
