{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module YX.Initialize
  where

import Prelude (error)

import Control.Applicative (Applicative, (*>), pure)
import Control.Exception (Exception, throwIO)
import Control.Monad ((>>=), foldM, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Bool (Bool(False, True), (||), not, otherwise)
import Data.Either (Either(Left, Right))
import Data.Function (($), (.), flip)
import Data.Functor ((<$), (<$>))
import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef)
import qualified Data.List as List (map, null)
import qualified Data.List.NonEmpty as NonEmpty (head, toList)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Monoid ((<>))
import Data.String (String, fromString)
import System.Environment (getExecutablePath)
import System.IO (FilePath, IO)
import qualified System.IO as IO ({-print,-} putStrLn)
import Text.Show (Show)

import qualified Data.Aeson as Aeson (eitherDecode', encode)
import qualified Data.ByteString.Lazy as Lazy.ByteString (readFile, writeFile)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (foldrWithKey, lookup)
import Data.Text (Text)
import qualified Data.Text as Text (unlines, unpack)
import qualified Data.Text.IO as Text (writeFile)
import Development.Shake
    ( ShakeOptions
        ( shakeFiles
--      , shakeLint
        )
    , (%>)
    , (~>)
    , shake
    , shakeOptions
    )
import qualified Development.Shake as Shake
    ( Action
--  , Lint(LintFSATrace)
    , alternatives
    , need
    , newCache
    , want
    )
import System.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , removeFile
    )
import System.FilePath ((</>))
import qualified System.FilePath as FilePath
    ( dropTrailingPathSeparator
    , makeRelative
    , splitFileName
    , takeDirectory
    )
import System.FilePath.Glob (globDir1)
import qualified System.FilePath.Glob as Glob (compile)
import qualified System.Posix.Files as Posix (createSymbolicLink)

import YX.Paths
    ( ProjectRoot
    , TypeOfStuff(CachedStuff, EnvironmentStuff)
    , yxConfigs
    , yxExeStuff
    , yxShellStuff
    , yxStuffFile
    , yxStuffPath
    )
import YX.Type.BuildTool (BuildTool(Cabal, Stack))
import qualified YX.Type.BuildTool as BuildTool (toText)
import YX.Type.CommandType (CommandType(Alias, Command, Symlink))
import YX.Type.ConfigFile
    ( ProjectConfig(ProjectConfig, _environments)
    , Environment(Environment, _bin)
    , Executable(Executable, _command, _type)
    , parseProjectConfig
    )
import YX.Type.Scm (Scm(Git))
import qualified YX.Type.Scm as Scm (toText)
import YX.Type.Shell (Shell(Bash))


type GlobPattern = String

-- | During initialization we only know where the project is, nothing more.
initializeProject :: ProjectRoot -> IO ProjectConfig
initializeProject root = do
    possibleConfig <- detectYxConfig root
    cfgRef <- newIORef (Nothing :: Maybe ProjectConfig)
    doYxStuff cfgRef root (root </> yxConfig) $ (root </>) <$> possibleConfig
    readIORef cfgRef >>= \case
        Nothing -> readCachedYxConfig $ yxStuffFile root CachedStuff "config.bin"
        Just cfg -> pure cfg
  where
    yxConfig = NonEmpty.head yxConfigs
        -- First file is considered to be the default. See 'yxConfigs' for more
        -- details.

doYxStuff
    :: IORef (Maybe ProjectConfig)
    -> ProjectRoot
    -> FilePath
    -> Maybe FilePath
    -> IO ()
doYxStuff cfgRef root defaultYxConfig possibleYxConfig = shake opts $ do
    getProjectCfg' <- Shake.newCache $ \(yxConfigChanged, cacheFile) -> liftIO
        $ let memo r = r <$ atomicWriteIORef cfgRef (Just r)
              getMemo = readIORef cfgRef
          in
            -- When configuration file has been changed then the cache must be
            -- invalidated. To avoid reading cache file (efficiently stored
            -- version of parsed configuration file) multiple times, we memoize
            -- the parsing result.
            if yxConfigChanged
                then parseAndCacheYxConfig yxConfig cacheFile >>= memo
                else getMemo >>= \case
                    Just r -> pure r
                    Nothing -> readCachedYxConfig cacheFile
                        -- We don't need to memoize cache file reads, because,
                        -- it is done for us by Shake. See 'Shake.newCache' for
                        -- more details.

    let compileProjectCfg = getProjectCfg' . (True, )
        getProjectCfg = getProjectCfg' . (False, )

    Shake.want ["yx-initialization"]

    -- We need to create YX project file only if there isn't one. To correctly
    -- track the dependencies we need this rule to be a pattern rule instead of
    -- phony rule.
    yxConfig %> \out -> liftIO $ do
        -- Even though this is not a phony rule, we need to check for existence
        -- of the file, otherwise we would overwrite any user defined
        -- configuration.
        haveConfig <- doesFileExist out
        unless haveConfig $ liftIO $ do
            IO.putStrLn $ out
                <> ": Generating YX configuration file for this project..."
            createProjectConfig root >>= Text.writeFile out

    cfgCacheFile %> \out -> do
        -- We expect yxConfig to be already present. See "yx-initialization" rule
        -- for details.
        Shake.need [yxConfig]

        -- (BIG) TODO: Delete old artifacts in .yx-stuff dir. Currently we
        -- would keep e.g. old links in "bin" dir.
        () <$ compileProjectCfg out

    yxShellStuff root "*" Bash </> "bashrc" %> \out -> do
        Shake.need [yxConfig, cfgCacheFile]
        cfg <- getProjectCfg cfgCacheFile
        compileBashrc cfg (FilePath.makeRelative yxEnvStuff out) out

    -- Combinator 'Shake.alternatives' allows us to use overlapping patterns.
    Shake.alternatives $ do
        -- Symbolic link to "yx" is a special case. We want to handle it
        -- separately to make that fact explicit.
        yxExeStuff root "*" </> "yx" %> \out -> do
            yxExe <- liftIO getExecutablePath
            createExecutableLink yxExe out

        yxExeStuff root "*" </> "*" %> \out -> do
            Shake.need [cfgCacheFile]
            (envName, exeName) <- parseBinFileName yxEnvStuff out
            cfg <- getProjectCfg cfgCacheFile
            lookupExe cfg envName exeName >>= \exe -> case _type exe of
                Alias -> error $ out
                    <> ": Trying to create binary, but shell alias was found."
                Command -> do
                    yxExe <- liftIO getExecutablePath
                    createExecutableLink yxExe out
                Symlink ->
                    createExecutableLink (Text.unpack $ _command exe) out

    -- Top level rule:
    "yx-initialization" ~> do
        -- No matter what, we need to parse YX project configuration file, or
        -- read the cached version if its available.
        Shake.need [cfgCacheFile]

        -- We need to traverse the configuration file to know what tasks need
        -- to be done executed.
        getProjectCfg cfgCacheFile >>= mapHM_ envDependencies . _environments
  where
    yxConfig = fromMaybe defaultYxConfig possibleYxConfig
    cfgCacheFile = yxStuffFile root CachedStuff "config.bin"
    yxEnvStuff = yxStuffPath (Just root) (EnvironmentStuff Nothing)

    opts = shakeOptions
        { shakeFiles = yxStuffPath (Just root) CachedStuff
--      , shakeLint = Just Shake.LintFSATrace
        }

    mapHM_ :: Applicative f => (k -> v -> f ()) -> HashMap k v -> f ()
    mapHM_ f = HashMap.foldrWithKey (\k -> (*>) . f k) $ pure ()

    forHM_ = flip mapHM_

    envDependencies name Environment{_bin = bins} = do
        needBin name "yx"
        forHM_ bins $ \exeName Executable{_type = t} -> case t of
            Alias   -> pure ()  -- Aliases are handled by shell.
            Command -> needBin name exeName
            Symlink -> needBin name exeName

    needBin name bin = do
        Shake.need [yxExeStuff root (Text.unpack name) </> Text.unpack bin]

    parseBinFileName dir file
      | haveParseError = error $ file <> ": Unexpected path when parsing\
        \'yxStuff </> envName </> \"bin\" </> exeName'"
      | otherwise      = pure (envName, exeName)
      where
        haveParseError = List.null envName || List.null exeName

        envName = FilePath.dropTrailingPathSeparator envDir

        (envDir, _) = FilePath.splitFileName
            $ FilePath.dropTrailingPathSeparator binDir

        (binDir, exeName) = FilePath.splitFileName
            $ FilePath.makeRelative dir file

    lookupExe :: ProjectConfig -> String -> String -> Shake.Action Executable
    lookupExe ProjectConfig{_environments = envs} envName exeName =
        case HashMap.lookup envName' envs >>= HashMap.lookup exeName' . _bin of
            Nothing -> error $ exeName <> ": Unable to find configuration for\
                \ executable in environment '" <> envName <> "'"
            Just r -> pure r
      where
        exeName' = fromString exeName
        envName' = fromString envName

createExecutableLink :: FilePath -> FilePath -> Shake.Action ()
createExecutableLink src dst = liftIO $ do
    srcExists <- doesFileExist src
    unless srcExists . error
        $ src <> ": File not found when trying to create symbolic link: "
        <> dst

    -- We need to force the symlink existence, since the configuration may have
    -- changed.
    dstExists <- doesFileExist dst
    when dstExists $ removeFile dst

    IO.putStrLn $ "Creating symbolic link: " <> src <> " --> " <> dst
    Posix.createSymbolicLink src dst

compileBashrc :: ProjectConfig -> FilePath -> FilePath -> Shake.Action ()
compileBashrc cfg relativeOut out = compileBashrc cfg relativeOut out

prepareExecutable :: ProjectConfig -> FilePath -> FilePath -> Shake.Action ()
prepareExecutable cfg relativeOut out = prepareExecutable cfg relativeOut out

-- {{{ Project Configuration File ---------------------------------------------

-- | Failed to parse\/decode YX project configuration cache file.
data CacheDecodingException = CacheDecodingException FilePath String
  deriving Show

instance Exception CacheDecodingException

-- | Read YX project configuration cache file, which is just the same as
-- @yx.yaml@ or @yx.yml@ (see also 'yxConfigs'), but stored more efficiently.
--
-- /Throws: 'CacheDecodingException'/
readCachedYxConfig :: FilePath -> IO ProjectConfig
readCachedYxConfig cfg =
    (Aeson.eitherDecode' <$> Lazy.ByteString.readFile cfg) >>= \case
        Left e -> throwIO $ CacheDecodingException cfg e
            -- TODO: Maybe we could try to parse the YAML file in this case.
        Right r -> pure r

-- | Parse YX project configuration file @yx.yaml@ or @yx.yml@ (see also
-- 'yxConfigs').
--
-- /Throws: 'Data.Yaml.ParseException'/
parseAndCacheYxConfig :: FilePath -> FilePath -> IO ProjectConfig
parseAndCacheYxConfig yxConfig out = parseProjectConfig yxConfig >>= \case
    Left e -> throwIO e
    Right r -> do
        createDirectoryIfMissing True (FilePath.takeDirectory out)
        -- TODO: Use efficient binary serialization. Should we assume
        --       portability? I hope not, but some people may use git clone
        --       inside a Dropbox. Don't ask for details, please, just think
        --       about how to handle such (pathological) cases.
        r <$ Lazy.ByteString.writeFile out (Aeson.encode r)

    -- TODO: We should check that there is at least one environment section in
    --       the configuration file and that there is exactly one with
    --       "is-default: True".

-- | Create initial version of project configuration file.
--
-- *TODO:*
--
-- * When /Stack/ is detected, then we need to add stack wrappers in to the configuration.
--
-- * When non-standard /Stack configuration file/ is found, then @STACK_YAML@
--   should be defined.
createProjectConfig :: ProjectRoot -> IO Text
createProjectConfig root = do
    scm <- detectVersionControl root
    buildTool <- detectBuildTool root
    pure $ Text.unlines
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
        , "    # Add or modify environment variables of the isolated execution"
        , "    # environment."
        , "    env:"
        , "    #  PATH: \"/some/path/bin:${PATH}\""
        , ""
        , "    # Add following commands/executables in to the isolated execution"
        , "    # environment."
        , "    #"
        , "    # When creating symlink, the command has always have to be\
            \ absolute path to"
        , "    # the executable."
        , "    bin:"
        , "      #stack:"
        , "      #  type: command"
        , "      #  command: stack build"
        , "      #  env:"
        , "      #    STACK_YAML: ${YX_PROJECT_ROOT}/stack-production.yaml"
        , "      #"
        , "      #build:"
        , "      #  type: alias"
        , "      #  command: ${YX_ENVIRONMENT_DIR}/bin/stack build"
        , "      #"
        , "      #lint:"
        , "      #  type: symlink"
        , "      #  command: /opt/hlint/bin/hlint"
        , "      #"
        , "      #hoogle:"
        , "      #  type: alias"
        , "      #  command: \"stack exec hoogle --\""
        ]
  where
    field :: Text -> Text -> Text
    field name value = name <> ": \"" <> value <> "\""

    field' :: Text -> Maybe Text -> Text
    field' name = \case
        Nothing -> "#" <> name <> ":"
        Just value -> field name value

-- | Detect if project contains YX project configuration file, and which, if
-- its found.
detectYxConfig :: ProjectRoot -> IO (Maybe FilePath)
detectYxConfig root =
    detect . List.map doesConfigExist $ NonEmpty.toList yxConfigs
  where
    doesConfigExist cfg = doesFileExist (root </> cfg) ~~> cfg

-- }}} Project Configuration File ---------------------------------------------

-- {{{ Tooling Detection ------------------------------------------------------

detectVersionControl :: ProjectRoot -> IO (Maybe Scm)
detectVersionControl root = detect
    [ doesDirectoryExist (root </> ".git") ~~> Git
    ]

detectBuildTool :: ProjectRoot -> IO (Maybe BuildTool)
detectBuildTool root = detect
    [ glob root "stack*.yaml" ~~> Stack
    , glob root "stack*.cabal" ~~> Cabal
    ]

-- }}} Tooling Detection ------------------------------------------------------

-- {{{ Utility functions ------------------------------------------------------

glob :: ProjectRoot -> GlobPattern -> IO Bool
glob root pattern = not . List.null <$> globDir1 (Glob.compile pattern) root

(~~>) :: IO Bool -> a -> (IO Bool, a)
(~~>) = (,)

detect :: [(IO Bool, a)] -> IO (Maybe a)
detect = foldM go Nothing
  where
    go r@(Just _) _              = pure r
    go Nothing    (predicate, a) = do
        isThisOne <- predicate
        pure $ if isThisOne
            then Just a
            else Nothing

-- }}} Utility functions ------------------------------------------------------
