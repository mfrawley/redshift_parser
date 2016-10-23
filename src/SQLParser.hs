{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module SQLParser

where
import Lib
import Types
import Text.ParserCombinators.Parsec ((<|>), (<?>), string, spaces, parse, ParseError
  , alphaNum, many1, char, try, many1, digit, optionMaybe, option, endBy)
import Data.Char (toLower, toUpper)
import Text.Parsec.Prim (ParsecT)
import Data.Functor.Identity
import Data.Maybe


createStm = do
    string "create table"
    spaces
    ifExistsStm

defaultsParser :: Text.Parsec.Prim.ParsecT [Char] u Data.Functor.Identity.Identity String
defaultsParser = string "not null" <|> string "default null"

primaryKeyLiteral :: Text.Parsec.Prim.ParsecT [Char] u Data.Functor.Identity.Identity String
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

primaryKeyLineParser :: Text.Parsec.Prim.ParsecT
           [Char] u Data.Functor.Identity.Identity [Char]
primaryKeyLineParser = keyLineParser primaryKeyLiteral

uniqueKeyLineParser :: Text.Parsec.Prim.ParsecT
           [Char] u Data.Functor.Identity.Identity [Char]
uniqueKeyLineParser = keyLineParser uniqueKeyLiteral

distStyleParser :: Text.Parsec.Prim.ParsecT
           String u Data.Functor.Identity.Identity DistStyle
distStyleParser = do
    string "diststyle"
    spaces
    dStyle <- string "all" <|> string "even" <|> string "key"
    spaces
    return dStyle

distKeyParser :: Text.Parsec.Prim.ParsecT
           String u Data.Functor.Identity.Identity DistKey
distKeyParser = do
    string "distkey"
    spaces
    dKey <- alphaNumInParens
    spaces
    return dKey

lineBeginningWithComma :: Text.Parsec.Prim.ParsecT
       [Char] u Data.Functor.Identity.Identity ()
lineBeginningWithComma = do
    spaces
    c <- optionMaybe (char ',')
    spaces

colWithSize :: Text.Parsec.Prim.ParsecT
       [Char] u Data.Functor.Identity.Identity ColumnDefinition
colWithSize = do
    lineBeginningWithComma
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
        , colDataLen = colDataTypeLen
        , colDefaults = defaults
        })

dropTableStm = string "drop table"

ifExistsStm = string "if not exists" <|> string "if exists"

dropTableQuery = do
    spaces
    dropTableStm
    spaces
    ifExistsStm
    spaces
    tab <- tableRef
    spaces
    char ';'
    return tab

sortKeyParser = do
    spaces
    string "sortkey"
    spaces
    leftParen
    spaces
    cols <- many1 $ fieldWithOptionalTrailingComma
    spaces
    rightParen
    spaces
    return cols

createQuery :: Text.Parsec.Prim.ParsecT
       String u Data.Functor.Identity.Identity TableDefinition
createQuery = do
    spaces
    droppedTable <- optionMaybe dropTableQuery
    spaces
    createStm
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
    dStyle <- optionMaybe (try distStyleParser)
    dKey <- optionMaybe (try distKeyParser)

    sKeys <- optionMaybe (try sortKeyParser)

    return (TableDefinition {
        tableName = table
      , schemaName = schema
      , columns = colDefs
      , primaryKey = pKey
      , uniqueKey = uKey
      , distKey = dKey
      , distStyle = dStyle
      , sortKeys = sKeys
      })


parseSQL :: String -> Either ParseError TableDefinition
parseSQL input = parse createQuery "(unknown)" input
