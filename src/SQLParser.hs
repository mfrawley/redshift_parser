module SQLParser

where

import Text.ParserCombinators.Parsec ((<|>), (<?>), string, spaces, parse, ParseError
  , alphaNum, many1, char, try, many1, digit, optionMaybe, option, endBy)
import Data.Char (toLower, toUpper)

import Data.Maybe

data ColumnDefinition = ColumnDefinition {
    colName :: String
    , colType :: String
    , colDataLen :: Maybe String
    , colDefaults :: String
} deriving (Show)

data TableDefinition = TableDefinition {
    schemaName :: String
  , tableName :: String
  , columnDefs :: [ColumnDefinition]
} deriving (Show)

createStm = string "create"
table = string "table"

{-Parses a table or column name-}
sqlName = many1 $ alphaNum <|> char '_'

colDataTypeParser = string "int"
        <|> string "varchar"
        <|> string "float"

leftParen = char '('
rightParen = char ')'

defaultsParser = string "not null" <|> string "default null"

colDataLenParser =
  do spaces
     leftParen
     spaces
     colDataTypeLen <- many1 digit
     spaces
     rightParen
     return colDataTypeLen

lineBeginningWithComma =
  do spaces
     c <- optionMaybe (char ',')
     spaces

colWithNoSize =
  do
    lineBeginningWithComma <|> spaces
    columnName <- sqlName
    spaces
    colDataType <- colDataTypeParser
    spaces
    defaults <- option "" defaultsParser
    spaces

    return (ColumnDefinition {
        colName = columnName
        , colType = colDataType
        , colDataLen = Nothing
        , colDefaults = defaults
        })

colWithSize =
    do
        lineBeginningWithComma <|> spaces
        columnName <- sqlName
        spaces
        colDataType <- colDataTypeParser
        spaces
        colDataTypeLen <- colDataLenParser
        spaces
        defaults <- option "" defaultsParser
        spaces

        return (ColumnDefinition {
            colName = columnName
            , colType = colDataType
            , colDataLen = Just colDataTypeLen
            , colDefaults = defaults
            })

query =
  do  spaces
      createStm
      spaces
      t <- table
      spaces
      schemaName <- sqlName
      char '.'
      tableName <- sqlName
      spaces
      leftParen
      colDefs <- (many1 (colWithNoSize <|> colWithSize))
      -- return [[schemaName, tableName, (colName d), (colType d), (colDataLen d)]]
      return colDefs

parseSQL :: String -> Either ParseError [ColumnDefinition]
parseSQL input = parse query "(unknown)" input
