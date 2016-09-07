{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:       $HEADER$
-- Description:  Enumeration of shells that YX supports natively.
-- Copyright:    (c) 2016 Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Enumeration of shells that YX supports natively.
module YX.Type.Shell
    ( Shell(..)
    , SomeShell
    , toString
    , fromString
    , toText
    , fromText
    )
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


-- | Enumeration of shells that YX supports natively.
data Shell = Bash
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

-- | Convert 'Shell' in to a generic string.
toString :: IsString s => Shell -> s
toString = \case
    Bash -> "Bash"
{-# INLINEABLE toString #-}

-- | Convert a generic string in to a 'Shell' in a case insensitive maner.
-- 'Nothing' is returned when string value is not recognized.
fromString :: (Eq s, IsString s, CI.FoldCase s) => s -> Maybe Shell
fromString s = case CI.mk s of
    "bash" -> Just Bash
    _ -> Nothing
{-# INLINEABLE fromString #-}

-- | Specialization of 'toString' for 'Text'.
toText :: Shell -> Text
toText = toString
{-# INLINE toText #-}

-- | Specialization of 'fromString' for 'Text'.
fromText :: Text -> Maybe Shell
fromText = fromString
{-# INLINE fromText #-}

instance FromJSON Shell where
    parseJSON = Aeson.withText "Shell" $ maybe empty pure . fromText

instance ToJSON Shell where
    toJSON = toJSON . toText

type SomeShell = Specifiable Text Shell
