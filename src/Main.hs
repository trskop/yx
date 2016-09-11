{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Main (main)
  where

import Prelude (error)

import Control.Applicative ((<*>), pure)
import Control.Monad ((=<<), (>>=), mapM_, unless)
import Data.Bool (Bool(True), otherwise)
import Data.Eq (Eq((/=)))
import Data.Function (($), (.), id)
import Data.Functor ((<$>), fmap)
import Data.Foldable (foldr)
import Data.List (elem, lookup, map, words)
import Data.Maybe (Maybe(Just, Nothing), catMaybes)
import Data.Monoid ((<>))
import Data.String (IsString(fromString), String)
import Data.Version (showVersion)
import System.Environment
    ( getArgs
    , getEnvironment
    , getExecutablePath
    , getProgName
    , lookupEnv
    )
import System.IO (FilePath, IO)

import qualified Data.HashMap.Strict as HashMap (lookup)
--import Data.List.Split (splitOn)
--import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import qualified Data.Text.IO as Text (putStrLn)
import qualified Database.SQLite.Simple as SQLite (execute_, withConnection)
import System.Directory
    ( XdgDirectory(XdgConfig)
    , canonicalizePath
--  , createDirectory
    , createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
--  , executable
--  , findFileWith
--  , getPermissions
    , getXdgDirectory
    , setCurrentDirectory
    )
import System.FilePath
    ( (</>)
    , dropTrailingPathSeparator
    , takeFileName
    )
import System.Posix.Process (executeFile)

import YX.Initialize (initializeProject, readCachedYxConfig)
import YX.Main
import YX.Paths
    ( EnvironmentName
    , TypeOfStuff(CachedStuff, EnvironmentStuff, YxStuff)
    , yxExeStuff
    , yxShellStuff
    , yxStuffPath
    , yxStuffFile
    )
import YX.Shell (Shell(Bash), findShell, shellExePaths)
import YX.Type.CommandType (CommandType(Command))
import YX.Type.ConfigFile (Executable(Executable){-, ProjectConfig-})
import qualified YX.Type.ConfigFile as Environment (Environment(_bin))
import qualified YX.Type.ConfigFile as ProjectConfig
    ( Executable(_command, {-_environment,-} _type)
    , defaultEnvironment
    , getEnvironment
    )
import YX.Type.DbConnection (DbConnection(DbConnection))
import YX.Type.Shell () -- Force build.

import Paths_yx (version)


main :: IO ()
main = do
    getProgName >>= \case
        "yx" -> yxMain
        other -> getArgs >>= yxExec other
  where
    yxExec alias args = do
        lookupEnv "YX_VERSION" >>= \case
            Nothing -> error "Not in an YX environment."
            Just _ -> pure ()

        envName <- lookupEnv "YX_ENVIRONMENT" >>= \case
            Nothing -> error "YX environment variables are corrupted."
            Just x -> pure x

        root <- lookupEnv "YX_PROJECT_ROOT" >>= \case
            Nothing -> error "YX environment variables are corrupted."
            Just x -> pure x

        projCfg <- readCachedYxConfig
            $ yxStuffFile root CachedStuff "config.bin"

        projEnv <- case ProjectConfig.getEnvironment projCfg $ fromString envName of
            Nothing -> error "YX configuration corrupted."
            Just e -> pure e

        case HashMap.lookup (fromString alias) (Environment._bin projEnv) of
            Nothing -> errInvalidAlias alias
            Just Executable{..}
              | _type /= Command -> errInvalidAlias alias
              | otherwise        -> case words (Text.unpack _command) of
                [] -> error $ alias <> ": Invalid configuration of this alias."
                cmd : cmdArgs -> executeFile cmd True (cmdArgs <> args) Nothing

    errInvalidAlias cmd = error $ cmd <> ": YX called with invalid alias."

yxMain :: IO ()
yxMain = do
    dbFile <- getYxDatabaseFile
    yxExe <- getExecutablePath
    getArgs >>= \case
        "cd" : pattern : _ -> doCd dbFile yxExe pattern
        ["ls"] -> doLs dbFile Nothing
        "ls" : pattern : _ -> doLs dbFile $ Just pattern
        _ -> error "Unknown argument or option."

doCd :: FilePath -> FilePath -> String -> IO ()
doCd dbFile yxExe pattern = SQLite.withConnection dbFile $ \c -> do
    let conn = DbConnection c
    findProjects conn (modifyPattern pattern) >>= \case
        [] -> do
            isADirectory <- doesDirectoryExist pattern
            if isADirectory
                then newProject conn pattern >>= runProjEnv Nothing
                else error "No such project found, try \"yx ls PATTERN\"."
        [p] -> runProjEnv Nothing p
        ps -> printProjects ps
  where
    runProjEnv = runProjectEnvironment yxExe

doLs :: FilePath -> Maybe String -> IO ()
doLs dbFile possiblePattern = SQLite.withConnection dbFile $ \c -> do
    let conn = DbConnection c
    printProjects =<< case possiblePattern of
        Nothing -> listProjects conn
        Just pattern -> findProjects conn (modifyPattern pattern)

modifyPattern :: IsString s => String -> s
modifyPattern pat
  | '*' `elem` pat = fromString pat
  | otherwise      = fromString $ "*" <> pat <> "*"

printProjects :: [Project] -> IO ()
printProjects = mapM_ printProject
  where
    printProject Project{..} = Text.putStrLn $ _name <> ": " <> _path

runProjectEnvironment :: FilePath -> Maybe EnvironmentName -> Project -> IO ()
runProjectEnvironment yxExe possiblyEnvName project@Project{..} = do
    projectCfg <- initializeProject yxExe root
    (projEnvName, _projEnv) <- getProjEnv projectCfg
    setCurrentDirectory root
    env <- modifyEnvVariables project projEnvName <*> getEnvironment
    shell <- findShell
        (lookup "PATH" env)
        (lookup "SHELL" env)
        defaultShells
    executeFile shell True ["--rcfile", bashrc projEnvName] (Just env)
  where
    -- We currently support only bash, so it makes sense to have it as default.
    defaultShells = shellExePaths Bash
    bashrc projEnvName = yxShellStuff root projEnvName Bash </> "bashrc"

    root = Text.unpack _path

    getProjEnv cfg = case possiblyEnvName of
        Just envName -> case lookupWhenProvided envName of
            Just env -> pure env
            Nothing -> error $ envName <> ": No such environment found."
        Nothing -> case ProjectConfig.defaultEnvironment cfg of
            Just (n, e) -> pure (Text.unpack n, e)
            Nothing -> error "Unable to find default environment. Exaclty one\
                \ environment has to have 'is-default: true', but no such\
                \ environment was found."
      where
        lookupWhenProvided n =
            (n, ) <$> ProjectConfig.getEnvironment cfg (fromString n)

newProject :: DbConnection -> FilePath -> IO Project
newProject conn relativeDir = do
    -- Function canonicalizePath preserves trailing path separator.
    dir <- dropTrailingPathSeparator <$> canonicalizePath relativeDir
    let project = Project
            { _id = 0 -- Ignored during INSERT.
            , _name = fromString $ takeFileName dir
            , _path = fromString dir
            }
    addProject conn project
    pure project

modifyEnvVariables
    :: Project
    -> EnvironmentName
    -> IO ([(String, String)] -> [(String, String)])
modifyEnvVariables project projEnvName = (. go) <$> addYxEnvVariables project
  where
    go = (getRidOfStackEnvVariables .) . map $ \case
        (n@"PATH", path) -> (n, updatePathEnvVariable project projEnvName path)
            -- TODO: Correct implementation witouth hardcoded environment.
        ev -> ev

updatePathEnvVariable :: Project -> EnvironmentName -> String -> String
updatePathEnvVariable project environment path = yxBinDir <> ":" <> path
  where
    yxBinDir = yxExeStuff root environment
    root = Text.unpack $ _path project

getRidOfStackEnvVariables :: [(String, String)] -> [(String, String)]
getRidOfStackEnvVariables = (catMaybes .) . map $ \case
    -- These variables are exported by stack and occur when we are already in a
    -- "stack exec" environment. For the purposes of isolation we need to get
    -- rid of them. Keeping them means that e.g. changing GHC version would
    -- fail due to trying to open incorrect package DB.
    ("STACK_EXE", _) -> Nothing
    ("GHC_VERSION", _) -> Nothing
    ("GHC_PACKAGE_PATH", _) -> Nothing
    ("CABAL_INSTALL_VERSION", _) -> Nothing
    ("HASKELL_PACKAGE_SANDBOX", _) -> Nothing
    ("HASKELL_PACKAGE_SANDBOXES", _) -> Nothing
    ("HASKELL_DIST_DIR", _) -> Nothing
    e -> Just e

addYxEnvVariables
    :: Project
    -> IO ([(String, String)] -> [(String, String)])
addYxEnvVariables Project{..} = do
    yxExe <- getExecutablePath
    yxInvokedName <- getProgName
    pure . foldr (.) id $ catMaybes
        [ add "YX_ENVIRONMENT_DIR"  $ Just projectEnvironmentDir
        , add "YX_ENVIRONMENT"      $ Just projectEnvironment
        , add "YX_EXE"              $ Just yxExe
        , add "YX_INVOKED_AS"       $ Just yxInvokedName
        , add "YX_PROJECT"          . Just $ Text.unpack _name
        , add "YX_PROJECT_ROOT"     $ Just projectRoot
        , add "YX_STUFF"            $ Just yxStuffDir
        , add "YX_VERSION"          . Just $ showVersion version
        ]
  where
    projectRoot = Text.unpack _path
    yxStuffDir = yxStuffPath (Just projectRoot) YxStuff
    projectEnvironment = "default"
    projectEnvironmentDir = yxStuffPath (Just projectRoot) . EnvironmentStuff
        $ Just projectEnvironment

    add :: String
        -> Maybe String
        -> Maybe ([(String, String)] -> [(String, String)])
    add n = fmap $ (:) . (n,)

getYxConfigDir :: IO FilePath
getYxConfigDir = do
    dir <- getXdgDirectory XdgConfig "yx"
    createDirectoryIfMissing True dir
    pure dir

getYxDatabaseFile :: IO FilePath
getYxDatabaseFile = do
    dbFile <- (</> "data.db") <$> getYxConfigDir
    haveDbFile <- doesFileExist dbFile
    unless haveDbFile $ SQLite.withConnection dbFile $ \conn ->
        SQLite.execute_ conn
            "CREATE TABLE Project\
                \(id INTEGER PRIMARY KEY AUTOINCREMENT,\
                \ name TEXT UNIQUE NOT NULL,\
                \ path TEXT UNIQUE NOT NULL\
                \);"
    pure dbFile
