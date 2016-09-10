{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module YX.Shell
    ( findShell
    , shellExePaths
    , ExecuteShellException(..)
    , module YX.Type.Shell

    -- * Utility functions
    , findExecutables
    )
  where

import Control.Applicative (pure)
import Control.Exception (Exception, throwIO)
import Control.Monad ((>>=))
import Data.Foldable (foldlM)
import Data.Function (($), (.), const, id)
import Data.Functor (fmap)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.String (String)
import System.IO (FilePath, IO)
import Text.Show (Show)

import System.Directory
    ( doesFileExist
    , executable
    , findFileWith
    , getPermissions
    )
import System.FilePath (splitSearchPath)

import Data.Bool.Lifted ((<&&>))

import YX.Type.Shell


data ExecuteShellException = UnableToFindShellExecutable
  deriving Show

instance Exception ExecuteShellException

shellExePaths :: Shell -> [FilePath]
shellExePaths = \case
    Bash ->
        [ "/bin/bash"
        , "/usr/bin/bash"
        , "/usr/local/bin/bash"     -- E.g. FreeBSD
        , "bash"                    -- Try to locate it in "$PATH".
        ]

findShell
    :: Maybe String
    -- ^ Search path.
    -> Maybe FilePath
    -- ^ Preferred shell executable.
    -> [FilePath]
    -- ^ Absolute paths and executable names of shells to try, in order.
    -> IO FilePath
findShell pathVar shellVar otherShells =
    findExecutables path shellPaths >>= reportError
  where
    path = maybe [] splitSearchPath pathVar
    shellPaths = maybe id (:) shellVar $ otherShells

    reportError :: Maybe FilePath -> IO FilePath
    reportError = maybe (throwIO UnableToFindShellExecutable) pure

findExecutables :: [FilePath] -> [FilePath] -> IO (Maybe FilePath)
findExecutables path = go $ \case
    r@(Just _) -> const $ pure r
    Nothing -> \case
        fp@('/' : _) -> do
            isExe <- doesFileExist fp <&&> isExecutable fp
            pure $ if isExe then Just fp else Nothing
        fp -> findFileWith isExecutable path fp
  where
    go  :: (Maybe FilePath -> FilePath -> IO (Maybe FilePath))
        -> [FilePath]
        -> IO (Maybe FilePath)
    go f = foldlM f Nothing

    isExecutable = fmap executable . getPermissions
