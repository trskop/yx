{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Data.Aeson (FromJSON(parseJSON), {-ToJSON(toJSON),-} (.!=), (.:), (.:?))
import qualified Data.Aeson as Aeson (withObject)
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
    parseJSON = Aeson.withObject "Executable" $ \o -> join $ mkExecutable
        <$> o .:? "type" .!= def
        <*> o .: "command"
        <*> o .:? "env"
      where
        mkExecutable t c e = uncurry (\t' -> Executable t' c) <$> case (t, e) of
            (Command, _) -> pure (t, e)
            (_, Nothing) -> pure (t, e)
            (_, Just  _) -> fail "\"env\" can only be used wiht type=command"

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

parseProjectConfig :: FilePath -> IO (Either Yaml.ParseException ProjectConfig)
parseProjectConfig = Yaml.decodeFileEither
