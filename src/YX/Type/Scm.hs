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
  where

import Control.Applicative ((<|>))
import Control.Exception (bracketOnError)
import Control.Monad (when)
import Data.Word (Word64)
import Data.IORef
import GHC.Generics (Generic)

import Data.Aeson
    ( FromJSON(parseJSON)
    , ToJSON(toJSON)
    , (.!=)
    , (.:)
    , (.:?)
    , (.=)
    )
import qualified Data.Aeson as Aeson
    ( Value(Null)
    , object
    , withObject
    , withText
    )
import qualified Data.Aeson.Types as Aeson (Parser, typeMismatch)
import qualified Data.CaseInsensitive as CI (mk)
import Data.Default (Default(def))
import Data.OverloadedLabels.TH (label)
import Data.OverloadedRecords ((:::), R, get)
import Data.OverloadedRecords.TH (overloadedRecord)
import Data.Text (Text)
import qualified Database.SQLite.Simple.FromField as SQLite
    ( FromField(fromField)
    )
import qualified Database.SQLite.Simple.ToField as SQLite (ToField(toField))


-- {{{ Utilities --------------------------------------------------------------

withNull :: String -> Aeson.Parser a -> Aeson.Value -> Aeson.Parser a
withNull _   p Aeson.Null = p
withNull e   _ v          = Aeson.typeMismatch e v

-- }}} Utilities --------------------------------------------------------------

-- {{{ Scm --------------------------------------------------------------------

data Scm
    = UnspecifiedScm
    | Git
    | OtherScm !Text
  deriving (Eq, Generic, Show)

instance Default Scm where
    def = UnspecifiedScm

toText :: Scm -> Maybe Text
toText = \case
    UnspecifiedScm -> Nothing
    Git -> Just "Git"
    OtherScm t -> Just t

fromText :: Text -> Scm
fromText t = case CI.mk t of
    "git" -> Git
    _ -> OtherScm t

instance FromJSON Scm where
    parseJSON v = unspecifiedScm v <|> specifiedScm v
      where
        unspecifiedScm = withNull expectedType (pure UnspecifiedScm)
        specifiedScm = Aeson.withText expectedType (pure . fromText)
        expectedType = "Scm"

instance ToJSON Scm where
    toJSON = toJSON . toText

instance SQLite.FromField Scm where
    fromField = fmap (maybe UnspecifiedScm fromText) <$> SQLite.fromField

instance SQLite.ToField Scm where
    toField = SQLite.toField . toText
