{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module YX.Main
  where

import Control.Applicative ((<*>))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Word (Word64)
import GHC.Generics (Generic)
import Text.Show (Show)
import System.IO (IO)

import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON), (.:), (.=))
import qualified Data.Aeson as Aeson (object, withObject)
import Data.Default (def)
import Data.OverloadedRecords.TH (overloadedRecord)
import Data.Text (Text)
import Database.SQLite.Simple (NamedParam((:=)))
import qualified Database.SQLite.Simple as SQLite
    ( FromRow(fromRow)
    , ToRow(toRow)
    , executeNamed
    , field
    , query_
    , queryNamed
    )
import qualified Database.SQLite.Simple.ToField as SQLite (toField)

import YX.Type.DbConnection (DbConnection(DbConnection))
import YX.Type.RuntimeConfig ()


-- {{{ Project ----------------------------------------------------------------

data Project = Project
    { _id :: Word64
    , _name :: Text
    , _path :: Text
    }
  deriving (Generic, Show)

overloadedRecord def ''Project

instance FromJSON Project where
    parseJSON = Aeson.withObject "Project" $ \o -> Project
        <$> o .: "id"
        <*> o .: "name"
        <*> o .: "path"

instance ToJSON Project where
    toJSON Project{..} = Aeson.object
        [ "id" .= _id
        , "name" .= _name
        , "path" .= _path
        ]

instance SQLite.FromRow Project where
    fromRow = Project <$> SQLite.field <*> SQLite.field <*> SQLite.field

instance SQLite.ToRow Project where
    toRow Project{..} =
        [ SQLite.toField _id
        , SQLite.toField _name
        , SQLite.toField _path
        ]

-- {{{ Project - SQL Queries --------------------------------------------------

type Pattern = Text

addProject :: DbConnection -> Project -> IO ()
addProject (DbConnection c) Project{..} =
    SQLite.executeNamed c insert [":name" := _name, ":path" := _path]
  where
    insert = "INSERT INTO `Project` (name, path) VALUES (:name, :path);"

listProjects :: DbConnection -> IO [Project]
listProjects (DbConnection c) =
    SQLite.query_ c "SELECT id,name,path FROM `Project`;"

findProjects :: DbConnection -> Pattern -> IO [Project]
findProjects (DbConnection c) pattern =
    -- TODO: These two patterns should be different. On the first one we want
    -- prefix equivalence and the other one we want suffix-like equivalence.
    -- The later is because the last path element is the most significant part.
    SQLite.queryNamed c select [":pat1" := pattern, ":pat2" := pattern]
  where
    select =
        "SELECT id,name,path\
        \ FROM `Project`\
        \ WHERE\
            \ name GLOB :pat1\
        \ OR\
            \ path GLOB :pat2;"

-- }}} Project - SQL Queries --------------------------------------------------

-- }}} Project ----------------------------------------------------------------
