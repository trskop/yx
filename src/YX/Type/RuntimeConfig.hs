{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
module YX.Type.RuntimeConfig
  where

import Prelude (($!))

import Control.Applicative (pure)
import Control.Exception (bracketOnError)
import Control.Monad ((>>=), when)
import Data.Bool (Bool(False, True))
import Data.Function (($))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Version (Version)
import GHC.Generics (Generic)
import System.IO (FilePath, IO)

import Data.Default (def)
import Data.OverloadedRecords ((:::), R, get)
import Data.OverloadedRecords.TH (overloadedRecord)
import qualified Database.SQLite.Simple as SQLite (close, open)

import YX.Type.DbConnection (DbConnection(DbConnection))
import YX.Type.ConfigFile (EnvironmentName, ProjectConfig)

import qualified Paths_yx as YX (version)


data RuntimeConfig = RuntimeConfig
    { _yxVersion :: Version
    , _yxInvokedAs :: FilePath
    , _yxExe :: FilePath
    , _projectConfig :: Maybe ProjectConfig
    , _currentEnvironment :: Maybe EnvironmentName

    , _dbConnection :: !(IO DbConnection)
    , _closeDbConnection :: !(IO ())
    }
  deriving (Generic)

overloadedRecord def ''RuntimeConfig

type MkRuntimeConfigParams =
    '[ "database" ::: FilePath
    , "yxInvokedAs" ::: FilePath
    , "yxExe" ::: FilePath
    , "projectConfig" ::: Maybe ProjectConfig
    , "currentEnvironment" ::: Maybe EnvironmentName
    ]

mkRuntimeConfig
    :: R MkRuntimeConfigParams opts
    => opts
    -> IO RuntimeConfig
mkRuntimeConfig opts = do
    dbConnRef <- newIORef Nothing
    pure $! mkRuntimeConfig'
        (mkGetDbConnection (get #database opts) dbConnRef)
        (mkCloseDbConnection dbConnRef)
  where
    mkRuntimeConfig' getConn closeConn = RuntimeConfig
        { _yxVersion = YX.version
        , _yxInvokedAs = get #yxInvokedAs opts
        , _yxExe = get #yxExe opts
        , _projectConfig = get #projectConfig opts
        , _currentEnvironment = get #currentEnvironment opts
        , _dbConnection = getConn
        , _closeDbConnection = closeConn
        }

    mkGetDbConnection
        :: FilePath
        -> IORef (Maybe DbConnection)
        -> IO DbConnection
    mkGetDbConnection !db !dbConnRef = readIORef dbConnRef >>= \case
        Just conn -> pure conn
        Nothing -> SQLite.open db `bracketOnError` SQLite.close $ \conn -> do
            -- We can not use 'bracket', that would close the connection right
            -- away and we need to keep it open as long as its needed.
            (isConflict, conn') <- atomicModifyIORef' dbConnRef $ \case
                existing@(Just conn') -> (existing, (True, conn'))
                    -- Other thread has allocated the connection and updated
                    -- the IORef first.
                Nothing ->
                    let conn' = DbConnection $! conn
                    in (Just $! conn', (False, conn'))

            -- Conflict means that we have opened the database simultaneously
            -- with another thread, and the other thread has won the race for
            -- publishing it via IORef. Hence the cleanup.
            when isConflict $ SQLite.close conn
            pure conn'

    mkCloseDbConnection :: IORef (Maybe DbConnection) -> IO ()
    mkCloseDbConnection !dbConnRef = do
        conn <- atomicModifyIORef' dbConnRef $ \case
            Nothing -> (Nothing, Nothing)
            c@(Just _) -> (Nothing, c)
        case conn of
            Nothing -> pure ()
            Just (DbConnection c) -> SQLite.close c

withDbConnection
    :: R '["dbConnection" ::: IO DbConnection] r
    => r
    -> (DbConnection -> IO a)
    -> IO a
withDbConnection r = (get #dbConnection r >>=)
