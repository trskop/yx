{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module YX.ProcessEnvironmentVariables
  where

import Control.Arrow ((***))
import Data.Function (($), (.))
import Data.Functor ((<$>), fmap)
import qualified Data.List as List (map)
import Data.Maybe (Maybe(Just, Nothing), catMaybes)
import Data.String (fromString)
import Data.Version (Version, showVersion)
import qualified System.Environment (getEnvironment)
import System.IO (FilePath, IO)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (filterWithKey, fromList)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet (fromList, member)
import Data.OverloadedRecords ((:::), R, get)
import Data.Text (Text)

import YX.Type.ConfigFile (EnvironmentName, ProjectConfig)


type EnvVarName = Text
type EnvVarNames = HashSet EnvVarName
type EnvVarValue = Text
type EnvVar = (EnvVarName, EnvVarValue)
type EnvVars = HashMap EnvVarName EnvVarValue


-- | Environment variables that are safe or somewhat necessary to be present in
-- the sanitized environment.
allowList :: EnvVarNames
allowList = HashSet.fromList
    -- User profile
    [ "HOME"
    , "LOGNAME"
    , "USER"

    -- Locale
    , "LANG"
    , "LANGUAGE"
    , "LC_CTYPE"
    , "LC_NUMERIC"
    , "LC_TIME"
    , "LC_COLLATE"
    , "LC_MONETARY"
    , "LC_MESSAGES"
    , "LC_PAPER"
    , "LC_NAME"
    , "LC_ADDRESS"
    , "LC_TELEPHONE"
    , "LC_MEASUREMENT"
    , "LC_IDENTIFICATION"
    , "LC_ALL"

    -- Editor
    , "EDITOR"
    , "VISUAL"

    -- Terminal
    , "COLORTERM"
    , "TERM"

    -- Dbus
    , "DBUS_SESSION_BUS_ADDRESS"

    -- X server and desktop environment
    --
    -- TODO: XDG_*
    , "DISPLAY"

    -- Gnome Keyring
    , "GNOME_KEYRING_CONTROL"
    , "GNOME_KEYRING_PID"

    -- GPG
    --
    -- https://www.gnupg.org/documentation/manuals/gnupg/GPG-Configuration.html
    , "GNUPGHOME"
    , "GPG_AGENT_INFO"  -- Deprecated since version 2.1.

    -- SSH Agent (see ssh-agent(1) man-page)
    , "SSH_AGENT_PID"
    , "SSH_AUTH_SOCK"

    -- Proxy environment variables. Note that some tools, unfortunately, use
    -- lower-case and some upper-case names.
    --
    -- See also curl(1) and wget(1).
    , "ALL_PROXY_PROXY"
    , "FTPS_PROXY"
    , "FTP_PROXY"
    , "HTTPS_PROXY"
    , "HTTP_PROXY"
    , "IMAP_PROXY"
    , "LDAP_PROXY"
    , "NO_PROXY_PROXY"
    , "POP3_PROXY"
    , "SMTP_PROXY"
    , "all_proxy_proxy"
    , "ftp_proxy"
    , "ftps_proxy"
    , "http_proxy"
    , "https_proxy"
    , "imap_proxy"
    , "ldap_proxy"
    , "no_proxy_proxy"
    , "pop3_proxy"
    , "smtp_proxy"

    -- Others
    , "LESSCLOSE"
    , "LESSOPEN"
    , "LS_COLORS"
    , "SELINUX_INIT"
    ]

safePath :: EnvVar
safePath = ("PATH", "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin")

{-
introspectStackVariables :: EnvVarNames
introspectStackVariables = HashSet.fromList
    [ "STACK_ROOT"
    , "STACK_YAML"
    ]
-}

type YxVariableParams =
    '[ "yxVersion" ::: Version
    , "yxInvokedAs" ::: FilePath
    , "yxExe" ::: FilePath
    , "projectConfig" ::: Maybe ProjectConfig
    , "currentEnvironment" ::: Maybe EnvironmentName
    ]

yxVariables :: R YxVariableParams cfg => cfg -> EnvVars
yxVariables cfg = HashMap.fromList $ catMaybes
    -- Variables relevant for YX
    [ add "YX_VERSION"     yxVersion
    , add "YX_INVOKED_AS"  yxInvokedAs
    , add "YX_EXE"         yxExe

    -- Project related variables
    , add' "YX_STUFF"           yxStuff
    , add' "YX_PROJECT"         yxProject
    , add' "YX_PROJECT_ROOT"    yxProjectRoot
    , add' "YX_ENVIRONMENT"     yxCurrentEnvironment
    , add' "YX_ENVIRONMENT_DIR" yxCurrentEnvironmentDir

    -- Variables based solely on project configuration
    , add' "YX_PROJECT_TOOLS"   yxProjectTools
    ]
  where
    add = (Just .) . (,)
    add' = fmap . (,)

    yxVersion = fromString . showVersion $ get #yxVersion cfg
    yxInvokedAs = fromString $ get #yxInvokedAs cfg
    yxExe = fromString $ get #yxExe cfg

    yxStuff = Nothing
    yxProject = Nothing
    yxProjectRoot = Nothing
    yxCurrentEnvironment = get #currentEnvironment cfg
    yxCurrentEnvironmentDir = Nothing

    yxProjectTools = Nothing

-- | Apply allow-list of environment variable names to an environment to
-- produce sanitized environment.
sanitizeEnvironment
    :: EnvVarNames
    -- ^ Allow-list of variable names to preserve.
    -> EnvVars
    -- ^ Unsinitized environment.
    -> EnvVars
    -- ^ Sanitized environment.
sanitizeEnvironment allow =
    HashMap.filterWithKey $ \name _value -> name `HashSet.member` allow

-- | Wrapper for 'System.Environment.getEnvironment' that produces 'EnvVars'
-- instead of @[(Data.String.String, Data.String.String)]@.
getEnvironment :: IO EnvVars
getEnvironment = HashMap.fromList . List.map (fromString *** fromString)
    <$> System.Environment.getEnvironment
