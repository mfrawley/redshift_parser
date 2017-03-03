{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Types
where

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)

type DistStyle = String
type DistKey = String
type ColumnName = String
data ColumnLength = ColumnLength {
      colLen :: Int
    , colPrecision :: Maybe Int
} deriving (Show, Eq, Generic, ToJSON, FromJSON)

data ColumnDefinition = ColumnDefinition {
      colName :: ColumnName
    , colType :: String
    , colDataLen :: Maybe ColumnLength
    , colDefaults :: String
    , colEncoding :: Maybe String
} deriving (Eq, Generic, Show, ToJSON, FromJSON)

data TableDefinition = TableDefinition {
    schemaName :: String
  , tableName :: String
  , columns :: [ColumnDefinition]
  , primaryKey :: Maybe ColumnName
  , uniqueKey :: Maybe ColumnName
  , distStyle :: Maybe DistStyle
  , distKey :: Maybe DistKey
  , sortKeys :: Maybe [ColumnName]
} deriving (Eq, Generic, Show, ToJSON, FromJSON)

