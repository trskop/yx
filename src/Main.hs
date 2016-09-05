{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main (main)
  where

import Prelude (error)

import Control.Applicative ((<*>), liftA2, pure)
import Control.Monad ((>>=), mapM_, when)
import Data.Bool (Bool(True), (&&), not, otherwise)
import Data.Function (($), (.), const, id)
import Data.Functor ((<$>), fmap)
import Data.Foldable (foldlM, foldr)
import Data.List (elem, init, intercalate, last, lookup, map)
import Data.Maybe (Maybe(Just, Nothing), catMaybes, fromMaybe, maybe)
import Data.Monoid ((<>))
import Data.String (IsString(fromString), String)
import Data.Version (showVersion)
import System.Environment
    ( getArgs
    , getEnvironment
    , getExecutablePath
    , getProgName
    )
import System.IO (IO, FilePath)

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
import System.Posix.Process (executeFile)

import YX.Main
import YX.Type.DbConnection (DbConnection(DbConnection))

import Paths_yx (version)


main :: IO ()
main = do
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

    runProjectEnvironment project@Project{..} = do
        setCurrentDirectory (Text.unpack _path)
        env <- modifyEnvVariables project <*> getEnvironment
        shell <- findShell'
            (lookup "PATH" env)
            (lookup "SHELL" env)
            defaultShells
        executeFile shell True [] (Just env)
      where
        defaultShells =
            [ "/bin/bash"
            , "/usr/bin/bash"
            , "/usr/local/bin/bash"
            , "bash"    -- Try to locate it in "$PATH".
            ]

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
modifyEnvVariables = fmap (. getRidOfStackEnvVariables) . addYxEnvVariables

data KnownShell = Bash

data TypeOfStuff
    = YxStuff
    | EnvironmentStuff
    | ExecutableStuff
    | ShellStuff KnownShell
    | CachedStuff

type Environment = String

projectYxStuffDir :: Project -> Environment -> TypeOfStuff -> FilePath
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

{-
mkBashrc :: Project -> Environment -> String
mkBashrc
-}

getYxConfigDir :: IO FilePath
getYxConfigDir = do
    dir <- getXdgDirectory XdgConfig "yx"
    createDirectoryIfMissing True dir
    pure dir

getYxDatabaseFile :: IO FilePath
getYxDatabaseFile = do
    dbFile <- (</> "data.db") <$> getYxConfigDir
    dbFileIsMissing <- not <$> doesFileExist dbFile
    when dbFileIsMissing $ SQLite.withConnection dbFile $ \conn ->
        SQLite.execute_ conn
            "CREATE TABLE Project\
                \(id INTEGER PRIMARY KEY AUTOINCREMENT,\
                \ name TEXT UNIQUE NOT NULL,\
                \ path TEXT UNIQUE NOT NULL\
                \);"
    pure dbFile

