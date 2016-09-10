{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module YX.Shell.Bash
  where

import Data.Text (Text)
import qualified Data.Text as Text (unlines)

import YX.Type.ConfigFile (ProjectConfig)


mkBashrc :: ProjectConfig -> Text -> Text
mkBashrc _ _ = Text.unlines
    [ "if [[ -e '/etc/profile' ]]; then"
    , "    source '/etc/profile'"
    , "elif [[ -e '/etc/bash.bashrc' ]]; then"
    , "    source '/etc/bash.bashrc'"
    , "fi"
    , ""
    , "function __yx_ps1()"
    , "{"
    , "    if [[ ! -v 'YX_VERSION' ]]; then"
    , "        return"
    , "    fi"
    , ""
    , "    if [[ -v 'YX_ENVIRONMENT' && \"${YX_ENVIRONMENT}\" ]]"
    , "    then"
    , "        local yxEnv=\":${YX_ENVIRONMENT}\""
    , "    fi"
    , ""
    , "    printf \"yx:%s%s\\n\" \"${YX_PROJECT}\" \"${yxEnv}\""
    , "}"
    , ""
    , "function __yx_ps1_pretty()"
    , "{"
    , "    if [[ ! -v 'YX_VERSION' ]]; then"
    , "        return"
    , "    fi"
    , ""
    , "    printf \" (%s)\\n\" \"$(__yx_ps1)\""
    , "}"
    , ""
    , "PS1='\\u@\\h:\\w$(__yx_ps1_pretty)\\$ '"
    , ""
    , "if [[ -e \"${HOME}/.bash_yx\" ]]; then"
    , "    source \"${HOME}/.bash_yx\""
    , "fi"
    , ""
    , "if [[ -e \"${YX_ENVIRONMENT_DIR}/bash/completion\" ]]; then"
    , "    source \"${YX_ENVIRONMENT_DIR}/bash/completion\""
    , "fi"
    , ""
    , "if [[ -e \"${YX_ENVIRONMENT_DIR}/bash/environment\" ]]; then"
    , "    source \"${YX_ENVIRONMENT_DIR}/bash/environment\""
    , "fi"
    ]

mkCompletion :: ProjectConfig -> Text -> Text
mkCompletion _ _ = ""

mkEnvironment :: ProjectConfig -> Text -> Text
mkEnvironment _ _ = ""
