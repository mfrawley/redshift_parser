{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module SQLParser

where
import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)
import Text.ParserCombinators.Parsec ((<|>), (<?>), string, spaces, parse, ParseError
  , alphaNum, many1, char, try, many1, digit, optionMaybe, option, endBy)
import Data.Char (toLower, toUpper)
import Text.Parsec.Prim (ParsecT)
import Data.Functor.Identity
import Data.Maybe

data ColumnDefinition = ColumnDefinition {
    colName :: String
    , colType :: String
    , colDataLen :: Maybe String
    , colDefaults :: String
} deriving (Eq, Generic, Show, ToJSON, FromJSON)

data TableDefinition = TableDefinition {
    schemaName :: String
  , tableName :: String
  , columns :: [ColumnDefinition]
  , primaryKey :: Maybe String
  , uniqueKey :: Maybe String
  , distStyle :: Maybe String
} deriving (Eq, Generic, Show, ToJSON, FromJSON)

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

primaryKeyLiteral = string "primary key"

uniqueKeyLiteral :: Text.Parsec.Prim.ParsecT [Char] u Data.Functor.Identity.Identity String
uniqueKeyLiteral = string "unique"

keyLineParser literalParser = do
    lineBeginningWithComma
    literalParser
    spaces
    leftParen
    spaces
    keyCol <- sqlName
    spaces
    rightParen
    spaces
    return keyCol

primaryKeyLineParser = keyLineParser primaryKeyLiteral

uniqueKeyLineParser = keyLineParser uniqueKeyLiteral

distStyleParser = do
    string "diststyle"
    spaces
    dStyle <- string "all" <|> string "even" <|> string "key"
    spaces
    return dStyle

colDataLenParser = do
    spaces
    leftParen
    spaces
    colDataTypeLen <- many1 digit
    spaces
    rightParen
    return colDataTypeLen

lineBeginningWithComma = do
    spaces
    c <- optionMaybe (char ',')
    spaces

colWithSize = do
    lineBeginningWithComma
    columnName <- sqlName
    spaces
    colDataType <- colDataTypeParser
    spaces
    colDataTypeLen <- optionMaybe colDataLenParser
    spaces
    defaults <- option "" defaultsParser
    spaces

    return (ColumnDefinition {
        colName = columnName
        , colType = colDataType
        , colDataLen = colDataTypeLen
        , colDefaults = defaults
        })

query = do
    spaces
    createStm
    spaces
    t <- table
    spaces
    schema <- sqlName
    char '.'
    table <- sqlName
    spaces
    leftParen
    colDefs <- (many1 (try colWithSize))
    pKey <- optionMaybe primaryKeyLineParser
    uKey <- optionMaybe uniqueKeyLineParser
    rightParen
    spaces
    dStyle <- optionMaybe distStyleParser

    return (TableDefinition {
        tableName = table
      , schemaName = schema
      , columns = colDefs
      , primaryKey = pKey
      , uniqueKey = uKey
      , distStyle = dStyle
      })


parseSQL :: String -> Either ParseError TableDefinition
parseSQL input = parse query "(unknown)" input
