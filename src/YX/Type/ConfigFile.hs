{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module YX.Type.ConfigFile
  where

import Control.Applicative ((<*>), pure)
import Control.Monad (join)
import Control.Monad.Fail (fail)
import Data.Bool (Bool(False))
import Data.Either (Either)
import Data.Eq (Eq)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (uncurry)
import GHC.Generics (Generic)
import Text.Read (Read)
import Text.Show (Show)
import System.IO (FilePath, IO)

import Data.Aeson
    ( FromJSON(parseJSON)
    , ToJSON(toJSON)
    , (.!=)
    , (.:)
    , (.:?)
    , (.=)
    )
import qualified Data.Aeson as Aeson (object, withObject)
import Data.Default (def)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (empty)
import Data.Text (Text)
import qualified Data.Yaml as Yaml (ParseException, decodeFileEither)

import YX.Type.BuildTool (SomeBuildTool)
import YX.Type.CommandType (CommandType(Command))
import YX.Type.Scm (SomeScm)


data Executable = Executable
    { _type :: CommandType
    , _command :: Text
    , _environment :: Maybe Environment
    }
  deriving (Eq, Generic, Read, Show)

instance FromJSON Executable where
    parseJSON = Aeson.withObject "Executable" $ \o -> join $ mk
        <$> o .:? "type" .!= def
        <*> o .: "command"
        <*> o .:? "env"
      where
        mk t c e = uncurry (\t' -> Executable t' c) <$> case (t, e) of
            (Command, _) -> pure (t, e)
            (_, Nothing) -> pure (t, e)
            (_, Just  _) -> fail "\"env\" can only be used wiht type=command"

instance ToJSON Executable where
    toJSON Executable{..} = Aeson.object
        [ "type" .= _type
        , "command" .= _command
        , "env" .= _environment
        ]

data Environment = Environment
    { _env :: HashMap Text Text
    , _bin :: HashMap Text Executable
    , _isDefault :: Bool
    }
  deriving (Eq, Generic, Read, Show)

instance FromJSON Environment where
    parseJSON = Aeson.withObject "Environment" $ \o -> Environment
        <$> o .:? "env" .!= HashMap.empty
        <*> o .:? "bin" .!= HashMap.empty
        <*> o .:? "is-default" .!= False

instance ToJSON Environment where
    toJSON Environment{..} = Aeson.object
        [ "env" .= _env
        , "bin" .= _bin
        , "is-default" .= _isDefault
        ]

data ProjectConfig = ProjectConfig
    { _scm :: SomeScm
    , _buildTool :: SomeBuildTool
    , _environments :: HashMap Text Environment
    }
  deriving (Eq, Generic, Read, Show)

instance FromJSON ProjectConfig where
    parseJSON = Aeson.withObject "ProjectConfig" $ \o -> ProjectConfig
        <$> o .:? "scm" .!= def
        <*> o .:? "build-tool" .!= def
        <*> o .:? "environment" .!= HashMap.empty

instance ToJSON ProjectConfig where
    toJSON ProjectConfig{..} = Aeson.object
        [ "scm" .= _scm
        , "build-tool" .= _buildTool
        , "environment" .= _environments
        ]

parseProjectConfig :: FilePath -> IO (Either Yaml.ParseException ProjectConfig)
parseProjectConfig = Yaml.decodeFileEither
