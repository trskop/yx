{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module YX.Initialize
  where

import Control.Monad (foldM, return)
import Data.Bool (Bool, not)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import qualified Data.List as List (map, null)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty (toList)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid ((<>))
import Data.String (String)
import System.IO (FilePath, IO)

import Data.Text (Text)
import qualified Data.Text as Text (unlines)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((</>), (<.>))
import System.FilePath.Glob (globDir1)
import qualified System.FilePath.Glob as Glob (compile)

import YX.Type.Scm (Scm(Git))
import qualified YX.Type.Scm as Scm (toText)
import YX.Type.BuildTool (BuildTool(Cabal, Stack))
import qualified YX.Type.BuildTool as BuildTool (toText)


type ProjectRoot = FilePath
type GlobPattern = String

createProjectConfig :: ProjectRoot -> IO Text
createProjectConfig root = do
    scm <- detectVersionControl root
    buildTool <- detectBuildTool root
    return $ Text.unlines
        [ "# Source Code Management (SCM) tool used by the project."
        , "# Currently only 'Git' is recognized automatically."
        , field' "scm" $ Scm.toText <$> scm
        , ""
        , "# Build tool used by the project."
        , "# Currently only 'Cabal' and 'Stack' are recognized automatically."
        , field' "build-tool" $ BuildTool.toText <$> buildTool
        , ""
        , "# Environments for this project."
        , "environment:"
        , "  # Execution environment named \"default\". It is used when there\
            \ is no"
        , "  # environment specified on the command line, due to \"is-default:\
            \ true\"."
        , "  default:"
        , "    is-default: true"
        , ""
        , "    # Add or modify environment variables of the isolated execution\
            \ environment."
        , "    #env:"
        , "    #  PATH: \"/some/path/bin:${PATH}\""
        , ""
        , "    # Add following commands/executables in to the isolated execution\
            \ environment."
        , "    #bin:"
        , "    #  stack:"
        , "    #    type: command"
        , "    #    command: stack build"
        , "    #    env:"
        , "    #      STACK_YAML: ${YX_PROJECT_ROOT}/stack-production.yaml"
        , "    #"
        , "    #  build:"
        , "    #    type: alias"
        , "    #    command: ${YX_ENVIRONMENT_DIR}/bin/stack build"
        , "    #"
        , "    #  lint:"
        , "    #    type: symlink"
        , "    #    command: /opt/hlint/bin/hlint"
        , "    #"
        , "    #  hoogle:"
        , "    #    type: alias"
        , "    #    command: \"stack exec hoogle --\""
        ]
  where
    field :: Text -> Text -> Text
    field name value = name <> ": \"" <> value <> "\""

    field' :: Text -> Maybe Text -> Text
    field' name = \case
        Nothing -> "#" <> name <> ":"
        Just value -> field name value

detectVersionControl :: ProjectRoot -> IO (Maybe Scm)
detectVersionControl root = detect
    [ doesDirectoryExist (root </> ".git") ~> Git
    ]

detectBuildTool :: ProjectRoot -> IO (Maybe BuildTool)
detectBuildTool root = detect
    [ glob root "stack*.yaml" ~> Stack
    , glob root "stack*.cabal" ~> Cabal
    ]

yxConfigs :: NonEmpty FilePath
yxConfigs = yxYaml :| [yxYml]
  where
    yxBase = "yx"
    yxYaml = yxBase <.> "yaml"
    yxYml = yxBase <.> "yml"

detectYxConfig :: ProjectRoot -> IO (Maybe FilePath)
detectYxConfig root =
    detect . List.map doesConfigExist $ NonEmpty.toList yxConfigs
  where
    doesConfigExist cfg = doesFileExist (root </> cfg) ~> cfg

-- {{{ Utility functions ------------------------------------------------------

glob :: ProjectRoot -> GlobPattern -> IO Bool
glob root pattern = not . List.null <$> globDir1 (Glob.compile pattern) root

(~>) :: IO Bool -> a -> (IO Bool, a)
(~>) = (,)

detect :: [(IO Bool, a)] -> IO (Maybe a)
detect = foldM go Nothing
  where
    go r@(Just _) _              = return r
    go Nothing    (predicate, a) = do
        isThisOne <- predicate
        return $ if isThisOne
            then Just a
            else Nothing

-- }}} Utility functions ------------------------------------------------------
