{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module SQLParser

where
import Lib
import Types
import Text.ParserCombinators.Parsec ((<|>), (<?>), string, spaces, parse, ParseError
  , alphaNum, many1, char, try, many1, digit, optionMaybe, option, optional, endBy)
import Data.Char (toLower, toUpper)
import Text.Parsec.Prim (ParsecT)
import Data.Functor.Identity (Identity)
import Data.Maybe


createStm = do
    spaces
    string "create table"
    spaces
    optional ifExistsStm
    spaces
    schema <- sqlName
    char '.'
    table <- sqlName
    spaces
    return (schema, table)

defaultsParser :: Text.Parsec.Prim.ParsecT [Char] u Identity String
defaultsParser = string "not null" <|> string "default null"

primaryKeyLiteral :: Text.Parsec.Prim.ParsecT [Char] u Identity String
primaryKeyLiteral = string "primary key"

uniqueKeyLiteral :: Text.Parsec.Prim.ParsecT [Char] u Identity String
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
           [Char] u Identity [Char]
primaryKeyLineParser = keyLineParser primaryKeyLiteral

uniqueKeyLineParser :: Text.Parsec.Prim.ParsecT
           [Char] u Identity [Char]
uniqueKeyLineParser = keyLineParser uniqueKeyLiteral

distStyleParser :: Text.Parsec.Prim.ParsecT
           String u Identity DistStyle
distStyleParser = do
    try $ do
        string "diststyle"
    spaces
    dStyle <- string "all" <|> string "even" <|> string "key"
    spaces
    return dStyle

distKeyParser :: Text.Parsec.Prim.ParsecT
           String u Identity DistKey
distKeyParser = do
    string "distkey"
    spaces
    dKey <- alphaNumInParens
    spaces
    return dKey

lineBeginningWithComma :: Text.Parsec.Prim.ParsecT
       [Char] u Identity ()
lineBeginningWithComma = do
    spaces
    c <- optionMaybe (char ',')
    spaces

encodeParser = do
    string "encode "
    encoding <- string "delta" <|>
        string "bytedict" <|>
        string "lzo" <|>
        string "raw" <|>
        string "mostly32"
        
    return encoding
    
colWithSize :: Text.Parsec.Prim.ParsecT
       [Char] u Identity ColumnDefinition
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
    encoding <- optionMaybe encodeParser
    spaces

    return (ColumnDefinition {
        colName = columnName
        , colType = colDataType
        , colDataLen = colDataTypeLen
        , colDefaults = defaults
        , colEncoding = encoding
        })

dropTableStm = string "drop table"

ifExistsStm = string "if not exists" <|> string "if exists"

dropTableQuery = do
    spaces
    dropTableStm
    spaces
    optional (string "if exists")
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
       String u Identity TableDefinition
createQuery = do
    optional (try spacesOrComment)
    optional (try dropTableQuery)
--     try dropTableQuery
    spaces
    (schema, table) <- createStm
    leftParen
    colDefs <- (many1 (try colWithSize))
    pKey <- optionMaybe primaryKeyLineParser
    uKey <- optionMaybe uniqueKeyLineParser
    rightParen
    spaces

    dStyle <- option "" distStyleParser
    
    dKey <- optionMaybe (try distKeyParser)

    sKeys <- optionMaybe (try sortKeyParser)

    return (TableDefinition {
        tableName = table
      , schemaName = schema
      , columns = colDefs
      , primaryKey = pKey
      , uniqueKey = uKey
      , distKey = dKey
      , distStyle = Just dStyle
      , sortKeys = sKeys
      })


parseSQL :: String -> Either ParseError TableDefinition
parseSQL input = parse createQuery "(unknown)" input
