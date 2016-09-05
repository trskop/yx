{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Main (main)
  where

import Prelude (error)

import Control.Applicative ((<*>), liftA2, pure)
import Control.Monad ((>>=), forM_, mapM_, unless)
import Data.Bool (Bool(True), (&&), (||), not, otherwise)
import Data.Function (($), (.), const, id)
import Data.Functor ((<$>), fmap)
import Data.Foldable (foldlM, foldr)
import Data.List (elem, init, intercalate, last, lookup, map, unlines)
import Data.Maybe (Maybe(Just, Nothing), catMaybes, fromMaybe, maybe)
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
import System.IO (IO, FilePath, writeFile)

import Data.List.Split (splitOn)
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import qualified Data.Text.IO as Text (putStrLn)
import qualified Database.SQLite.Simple as SQLite (execute_, withConnection)
import System.Directory
    ( XdgDirectory(XdgConfig)
    , canonicalizePath
    , createDirectory
    , createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , executable
    , findFileWith
    , getPermissions
    , getXdgDirectory
    , setCurrentDirectory
    )
import System.FilePath
    ( (</>)
    , dropTrailingPathSeparator
    , splitSearchPath
    , takeFileName
    )
import System.Posix.Files (createSymbolicLink)
import System.Posix.Process (executeFile)

import YX.Main
import YX.Type.DbConnection (DbConnection(DbConnection))

import Paths_yx (version)


main :: IO ()
main = do
    getProgName >>= \case
        "yx" -> yxMain
        "build" -> yxExec "stack" ["build"]
        "bld" -> yxExec "stack" ["build"]
        _ -> yxMain
  where
    yxExec cmd args = lookupEnv "YX_VERSION" >>= \case
        Nothing -> error "Not in an YX environment."
        Just _ -> executeFile cmd True args Nothing

yxMain :: IO ()
yxMain = do
    dbFile <- getYxDatabaseFile
    getArgs >>= \case
        "cd" : pattern : _ -> SQLite.withConnection dbFile $ \c -> do
            let conn = DbConnection c
            findProjects conn (modifyPattern pattern) >>= \case
                [] -> do
                    isADirectory <- doesDirectoryExist pattern
                    if isADirectory
                        then newProeject conn pattern >>= runProjectEnvironment
                        else error "No such project found, try \"yx ls PATTERN\"."
                [p] -> runProjectEnvironment p
                ps -> printProjects ps

        ["ls"] -> SQLite.withConnection dbFile $ \c ->
            listProjects (DbConnection c) >>= printProjects

        "ls" : pattern : _ -> SQLite.withConnection dbFile $ \c ->
            findProjects (DbConnection c) (modifyPattern pattern)
                >>= printProjects

        _ -> error "Unknown argument or option."
  where
    modifyPattern pat
      | '*' `elem` pat = fromString pat
      | otherwise      = fromString $ pat <> "*"

    printProject Project{..} = Text.putStrLn $ _name <> ": " <> _path
    printProjects = mapM_ printProject

runProjectEnvironment :: Project -> IO ()
runProjectEnvironment project@Project{..} = do
    initYxStuff project ProjectConfig
        { _scm = "Git"
        , _environments = [Environment "_default"]
        }
    setCurrentDirectory (Text.unpack _path)
    env <- modifyEnvVariables project <*> getEnvironment
    shell <- findShell'
        (lookup "PATH" env)
        (lookup "SHELL" env)
        defaultShells
    executeFile shell True ["--rcfile", bashrc] (Just env)
  where
    defaultShells =
        [ "/bin/bash"
        , "/usr/bin/bash"
        , "/usr/local/bin/bash"
        , "bash"    -- Try to locate it in "$PATH".
        ]

    bashrc = projectYxStuffDir project "_default" (ShellStuff Bash) </> "bashrc"

newProeject :: DbConnection -> FilePath -> IO Project
newProeject conn relativeDir = do
    -- Function canonicalizePath preserves trailing path separator.
    dir <- dropTrailingPathSeparator <$> canonicalizePath relativeDir
    let project = Project
            { _id = 0 -- Ignored during INSERT.
            , _name = fromString $ takeFileName dir
            , _path = fromString dir
            }
    addProject conn project
    pure project

findShell'
    :: Maybe String
    -- ^ Search path.
    -> Maybe FilePath
    -- ^ Preferred shell executable.
    -> [FilePath] -> IO FilePath
findShell' pathVar shellVar otherShells =
    findShell path . maybe id (:) shellVar $ otherShells
  where
    path = maybe [] splitSearchPath pathVar

findShell :: [FilePath] -> [FilePath] -> IO FilePath
findShell path = go $ \case
    r@(Just _) -> const $ pure r
    Nothing -> \case
        fp@('/' : _) -> do
            isShell <- doesFileExist fp <&&> isExecutable fp
            pure $ if isShell then Just fp else Nothing
        fp -> findFileWith isExecutable path fp -- Windows support? (<.> exe)
  where
    go  :: (Maybe FilePath -> FilePath -> IO (Maybe FilePath))
        -> [FilePath]
        -> IO FilePath
    go f = handleError . foldlM f Nothing

    handleError :: IO (Maybe FilePath) -> IO FilePath
    handleError =
        fmap . fromMaybe $ error "Unable to find shell executable."

    (<&&>) :: IO Bool -> IO Bool -> IO Bool
    (<&&>) = liftA2 (&&)

    isExecutable :: FilePath -> IO Bool
    isExecutable = fmap executable . getPermissions

modifyEnvVariables
    :: Project
    -> IO ([(String, String)] -> [(String, String)])
modifyEnvVariables project = (. go) <$> addYxEnvVariables project
  where
    go = (getRidOfStackEnvVariables .) . map $ \case
        (n@"PATH", path) -> (n, updatePathEnvVariable project "_default" path)
            -- TODO: Correct implementation witouth hardcoded environment.
        ev -> ev

data KnownShell = Bash

data TypeOfStuff
    = YxStuff
    | EnvironmentStuff
    | ExecutableStuff
    | ShellStuff KnownShell
    | CachedStuff

projectYxStuffDir :: Project -> String -> TypeOfStuff -> FilePath
projectYxStuffDir Project{_path = root} environment = (yxStuffRoot </>) . \case
    YxStuff -> yxStuffRoot
    EnvironmentStuff -> envDir
    ExecutableStuff -> envDir </> "bin"
    ShellStuff Bash -> envDir </> "bash"
    CachedStuff -> "cache"
  where
    yxStuffRoot = Text.unpack root </> ".yx-stuff"
    envDir = "env" </> environment

updatePathEnvVariable :: Project -> String -> String -> String
updatePathEnvVariable project environment path = yxBinDir <> ":" <> path
  where
    yxBinDir = projectYxStuffDir project environment ExecutableStuff

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
addYxEnvVariables project@Project{..} = do
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
    projectEnvironment = "_default"
    projectEnvironmentDir =
        projectYxStuffDir project projectEnvironment EnvironmentStuff
    yxStuffDir =
        projectYxStuffDir project projectEnvironment YxStuff

    add :: String
        -> Maybe String
        -> Maybe ([(String, String)] -> [(String, String)])
    add n = fmap $ (:) . (n,)

mkBashrc :: Project -> Environment -> String
mkBashrc _ _ = unlines
    [ "if [[ -e '/etc/bash.bashrc' ]]; then"
    , "    source '/etc/bash.bashrc'"
    , "fi"
    , ""
    , "if [[ -e \"${HOME}/.bashrc\" ]]; then"
    , "    source \"${HOME}/.bashrc\""
    , "fi"
    , ""
    , "if [[ -e \"${YX_ENVIRONMENT_DIR}/bash/completion\" ]]; then"
    , "    source \"${YX_ENVIRONMENT_DIR}/bash/completion\""
    , "fi"
    , ""
    , "function __yx_ps1()"
    , "{"
    , "    if [[ ! -v 'YX_VERSION' ]]; then"
    , "        return"
    , "    fi"
    , ""
    , "    if [[ -v 'YX_ENVIRONMENT' && \"${YX_ENVIRONMENT}\" != '_default' ]]"
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
    ]

mkCompletion :: Project -> Environment -> String
mkCompletion _ _ = ""

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

data Environment = Environment
    { envName :: Text
    }

data ProjectConfig = ProjectConfig
    { _scm :: Text
    , _environments :: [Environment]
    }

initYxStuff :: Project -> ProjectConfig -> IO ()
initYxStuff project ProjectConfig{..} = forM_ _environments $ \env -> do
    let name = Text.unpack $ envName env
        envDir = projectYxStuffDir project name  EnvironmentStuff
        binDir = envDir </> "bin"
        bashDir = envDir </> "bash"
    mapM_ (createDirectoryIfMissing True) [binDir, bashDir]

    let bashrc = bashDir </> "bashrc"
    haveBashrc <- doesFileExist bashrc
    unless haveBashrc
        . writeFile bashrc $ mkBashrc project env

    let buildCmd = binDir </> "build"
    haveBuildCmd <- doesFileExist buildCmd
    unless haveBuildCmd $ do
        yxExe <- getExecutablePath
        createSymbolicLink yxExe buildCmd

    let bldCmd = binDir </> "bld"
    haveBuildCmd <- doesFileExist bldCmd
    unless haveBuildCmd $ do
        yxExe <- getExecutablePath
        createSymbolicLink yxExe bldCmd

{- TODO:
    let root = projectRoot project
        yxConfig = root </> "xy.yaml"
        yxConfig' = root </> "xy.yml"
    haveYxConfig <- doesDirectoryExist yxConfig
        <||> doesDirectoryExist yxConfig'
    unless haveYxConfig $ mkYxConfig >>= writeFile yxConfig

    (<||>) :: IO Bool -> IO Bool -> IO Bool
    (<||>) = liftA2 (||)

projectRoot :: Project -> FilePath
projectRoot = Text.unpack . #path

mkYxConfig :: Project -> ProjectConfig -> IO String
mkYxConfig =
-}
