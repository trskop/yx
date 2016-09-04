{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Data type that represents name of a SCM (Source Code
--               Management) tool.
-- Copyright:    (c) 2016 Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Data type that represents name of a SCM (Source Code Management) tool.
module YX.Type.DbConnection
    ( DbConnection(..)
    , withSQLiteConn
    )
  where

import Data.Function (($))
import GHC.Generics (Generic)

import qualified Database.SQLite.Simple as SQLite (Connection)


-- | YX stores its global data about existing projects in a database. This data
-- type wraps lower level connection for more type safety.
newtype DbConnection = DbConnection SQLite.Connection
  deriving (Generic)

-- | Use 'DbConnection' as a SQLite Simple 'SQLite.Connection'.
--
-- Example:
--
-- @
-- 'withSQLiteConn' conn $ \\c ->
--     SQLite.query_ c \"SELECT * FROM InterestingTable;\"
-- @
withSQLiteConn :: DbConnection -> (SQLite.Connection -> a) -> a
withSQLiteConn (DbConnection c) = ($ c)
