{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module YX.Type.ConfigFile
    (
    -- * Executable
      ExecutableName
    , Executable(..)

    -- * Environment
    , EnvironmentName
    , Environment(..)

    -- * ProjectConfig
    , ProjectConfig(..)
    , parseProjectConfig
    , defaultEnvironment
    , getEnvironment
    )
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
import Data.Monoid ((<>))
import Data.Tuple (uncurry)
import GHC.Generics (Generic)
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
import qualified Data.HashMap.Strict as HashMap (empty, filter, lookup, toList)
import Data.Text (Text)
import qualified Data.Yaml as Yaml (ParseException, decodeFileEither)

import YX.Type.BuildTool (SomeBuildTool)
import YX.Type.CommandType (CommandType(Command))
import YX.Type.EnvVarTemplate (EnvVarTemplate)
import YX.Type.Scm (SomeScm)


-- {{{ Executable -------------------------------------------------------------

-- TODO: Move to a separate file.

type ExecutableName = Text

data Executable = Executable
    { _type :: CommandType
    , _command :: Text
    , _environment :: Maybe Environment

    -- TODO:
    --, _preHook :: Text
    --, _postHook :: Text
    }
  deriving (Eq, Generic, Show)

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

-- }}} Executable -------------------------------------------------------------

-- {{{ Environment ------------------------------------------------------------

-- TODO: Move to a separate file.

type EnvironmentName = Text

data Environment = Environment
    { _env :: [(EnvironmentName, Maybe EnvVarTemplate)]
    , _bin :: HashMap ExecutableName Executable
    , _isDefault :: Bool
    }
  deriving (Eq, Generic, Show)

instance FromJSON Environment where
    parseJSON = Aeson.withObject "Environment" $ \o -> Environment
        <$> o .:? "env" .!= []
        <*> o .:? "bin" .!= HashMap.empty
        <*> o .:? "is-default" .!= False

instance ToJSON Environment where
    toJSON Environment{..} = Aeson.object
        [ "env" .= _env
        , "bin" .= _bin
        , "is-default" .= _isDefault
        ]

-- }}} Environment ------------------------------------------------------------

-- {{{ ProjectConfig ----------------------------------------------------------

data ProjectConfig = ProjectConfig
    { _scm :: SomeScm
    , _buildTool :: SomeBuildTool   -- TODO: consider multiple build tools.
    , _environments :: HashMap Text Environment
    -- TODO:
    --, _global :: GlobalEnvironment
    }
  deriving (Eq, Generic, Show)

instance FromJSON ProjectConfig where
    parseJSON = Aeson.withObject "ProjectConfig" $ \o -> join $ mk
        <$> o .:? "scm" .!= def
        <*> o .:? "build-tool" .!= def
        <*> o .:? "environment" .!= HashMap.empty
      where
        mk s b e = case HashMap.toList $ HashMap.filter _isDefault e of
            [] -> err "no such environment was found"
            [_] -> pure $ ProjectConfig s b e
            _ -> err "multiple such environments were found"

        err rest = fail
            $ "Exactly one environment has to have 'is-default: true', but "
            <> rest <> "."

instance ToJSON ProjectConfig where
    toJSON ProjectConfig{..} = Aeson.object
        [ "scm" .= _scm
        , "build-tool" .= _buildTool
        , "environment" .= _environments
        ]

parseProjectConfig :: FilePath -> IO (Either Yaml.ParseException ProjectConfig)
parseProjectConfig = Yaml.decodeFileEither

defaultEnvironment :: ProjectConfig -> Maybe (Text, Environment)
defaultEnvironment p = case HashMap.toList defaultEnvs of
    [] -> Nothing
    [x] -> Just x
    _ -> Nothing
  where
    defaultEnvs = HashMap.filter _isDefault $ _environments p

getEnvironment :: ProjectConfig -> Text -> Maybe Environment
getEnvironment ProjectConfig{_environments = envs} name =
    HashMap.lookup name envs

-- }}} ProjectConfig ----------------------------------------------------------
