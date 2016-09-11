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
-- Description:  TODO
-- Copyright:    (c) 2016 Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- TODO
module YX.Type.EnvVarTemplate
    ( EnvVarTemplate(..)
    , toString
    , fromString
    , toText
    , fromText

    -- * Rendering
    , ContextA
    , renderA
    )
  where

import Control.Applicative (empty, pure)
import Data.Either (Either(Left, Right))
import Data.Eq (Eq)
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.String (IsString, String)
import qualified Data.String as IsString (fromString)
import GHC.Generics (Generic)
import Text.Show (Show(show))

import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON))
import qualified Data.Aeson as Aeson (withText)
import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy.Text (toStrict)
import Data.Text.Template (ContextA, Template, showTemplate, templateSafe)
import qualified Data.Text.Template as Template (renderA)


newtype EnvVarTemplate = EnvVarTemplate Template
  deriving (Eq, Generic)

instance Show EnvVarTemplate where
    show (EnvVarTemplate t) = show t

toString :: IsString s => EnvVarTemplate -> s
toString = IsString.fromString . show
{-# INLINE toString #-}

fromString :: String -> Maybe EnvVarTemplate
fromString = fromText . IsString.fromString
{-# INLINEABLE fromString #-}

toText :: EnvVarTemplate -> Text
toText (EnvVarTemplate t) = showTemplate t
{-# INLINE toText #-}

fromText :: Text -> Maybe EnvVarTemplate
fromText t = case templateSafe t of
    Left _ -> Nothing
    Right r -> Just $ EnvVarTemplate r
{-# INLINE fromText #-}

instance FromJSON EnvVarTemplate where
    parseJSON = Aeson.withText "EnvVarTemplate" $ maybe empty pure . fromText

instance ToJSON EnvVarTemplate where
    toJSON = toJSON . toText

renderA :: Applicative f => EnvVarTemplate -> ContextA f -> f Text
renderA (EnvVarTemplate t) = fmap Lazy.Text.toStrict . Template.renderA t
