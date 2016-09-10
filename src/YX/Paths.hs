{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
module YX.Paths
  where

import Data.Char (toLower)
import Data.Function ((.))
import Data.List (map)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.String (String)
import System.IO (FilePath)
import Text.Show (Show)

import System.FilePath ((<.>), (</>))

import YX.Type.Shell (Shell)
import qualified YX.Type.Shell as Shell (toString)


-- {{{ ${projectRoot}/.yx-stuff -----------------------------------------------

type EnvironmentName = String
type ProjectRoot = FilePath

data TypeOfStuff
    = YxStuff
    | CachedStuff
    | EnvironmentStuff (Maybe EnvironmentName)
    | ExecutableStuff EnvironmentName
    | ShellStuff EnvironmentName Shell
  deriving Show

yxStuffPath
    :: Maybe ProjectRoot
    -- ^ 'Nothing' will give a relative paths and @'Just' root@ will give
    -- absolute paths with @root@ as a path prefix.
    -> TypeOfStuff
    -> FilePath
yxStuffPath possiblyRoot = (yxStuffRoot </>) . \case
    YxStuff -> ""
    CachedStuff -> "cache"
    EnvironmentStuff possiblyEnvName -> envDir (fromMaybe "" possiblyEnvName)
    ExecutableStuff envName -> envDir envName </> "bin"
    ShellStuff envName shell -> envDir envName </> shellToString shell
  where
    yxStuffRoot = fromMaybe "" possiblyRoot </> ".yx-stuff"
    envDir = ("env" </>)
    shellToString = map toLower . Shell.toString

yxStuffFile
    :: ProjectRoot
    -> TypeOfStuff
    -> FilePath
    -> FilePath
yxStuffFile r t = (yxStuffPath (Just r) t </>)

yxStuffFile'
    :: TypeOfStuff
    -> FilePath
    -> FilePath
yxStuffFile' t = (yxStuffPath Nothing t </>)

yxExeStuff :: FilePath -> EnvironmentName -> FilePath
yxExeStuff root = yxStuffPath (Just root) . ExecutableStuff

yxShellStuff :: FilePath -> EnvironmentName -> Shell -> FilePath
yxShellStuff root = (yxStuffPath (Just root) .) . ShellStuff

-- }}} ${projectRoot}/.yx-stuff -----------------------------------------------

-- {{{ Project Configuration File ---------------------------------------------

-- | Non-empty list of possible YX project configuration files:
--
-- * @yx.yaml@ (considered as a default when automatically creating new file)
--
-- * @yx.yml@
yxConfigs :: NonEmpty FilePath
yxConfigs = yxYaml :| [yxYml]
  where
    yxBase = "yx"
    yxYaml = yxBase <.> "yaml"
    yxYml = yxBase <.> "yml"

-- }}} Project Configuration File ---------------------------------------------
