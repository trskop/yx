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
import GHC.Generics (Generic)
import System.IO (FilePath, IO)

import Data.Default (def)
import Data.OverloadedLabels.TH (labels)
import Data.OverloadedRecords ((:::), R, get)
import Data.OverloadedRecords.TH (overloadedRecord)
import qualified Database.SQLite.Simple as SQLite (close, open)

import YX.Type.DbConnection (DbConnection(DbConnection))


data RuntimeConfig = RuntimeConfig
    { _dbConnection :: !(IO DbConnection)
    , _closeDbConnection :: !(IO ())
    }
  deriving (Generic)

overloadedRecord def ''RuntimeConfig

labels ["database", "dbConnection"]

mkRuntimeConfig
    :: R '["database" ::: FilePath] opts
    => opts
    -> IO RuntimeConfig
mkRuntimeConfig opts = do
    dbConnRef <- newIORef Nothing
    pure $! RuntimeConfig
        (mkGetDbConnection (get database opts) dbConnRef)
        (mkCloseDbConnection dbConnRef)
  where
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
withDbConnection r = (get dbConnection r >>=)
