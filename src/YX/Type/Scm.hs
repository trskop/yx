{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module:       $HEADER$
-- Description:  Data type that represents name of a SCM (Source Code
--               Management) tool.
-- Copyright:    (c) 2016 Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Data type that represents name of a SCM (Source Code Management) tool.
module YX.Type.Scm
    ( Scm(..)
    , SomeScm
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


-- | Enumeration of SCMs (Source Code Managemetn tools) that YX supports
-- natively.
data Scm = Git
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

-- | Convert 'Scm' in to a generic string.
toString :: IsString s => Scm -> s
toString = \case
    Git -> "Git"
{-# INLINEABLE toString #-}

-- | Convert a generic string in to a 'Scm' in a case insensitive maner.
-- 'Nothing' is returned when string value is not recognized.
fromString :: (Eq s, IsString s, CI.FoldCase s) => s -> Maybe Scm
fromString s = case CI.mk s of
    "git" -> Just Git
    _ -> Nothing
{-# INLINEABLE fromString #-}

toText :: Scm -> Text
toText = toString
{-# INLINE toText #-}

fromText :: Text -> Maybe Scm
fromText = fromString
{-# INLINE fromText #-}

instance FromJSON Scm where
    parseJSON = Aeson.withText "Scm" $ maybe empty pure . fromText

instance ToJSON Scm where
    toJSON = toJSON . toText

type SomeScm = Specifiable Text Scm
