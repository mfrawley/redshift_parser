module SQLParser
    ( parseSQL
    )
where

import Text.ParserCombinators.Parsec ((<|>), (<?>), string, spaces, parse, ParseError, alphaNum, many1, char, try, many1, digit)
import Data.Char (toLower, toUpper)

data ColumnDefinition = ColumnDefinition {
    colName :: String
    , colType :: String
    , colDataLen :: String
    } deriving (Show)

-- Match the lowercase or uppercase form of 'c'
-- caseInsensitiveChar c = char (toLower c) <|> char (toUpper c)
--
-- -- Match the string 's', accepting either lowercase or uppercase form of each character
-- caseInsensitiveString s = try (mapM caseInsensitiveChar s) <?> "\"" ++ s ++ "\""

-- spaces = many1 (string " ")

createStm = string "create"

table = string "table"

{-Parses a table or column name-}
sqlName = many1 $ alphaNum <|> char '_'

colDataTypeParser = string "int"
        <|> string "varchar"
        <|> string "float"

leftParen = char '('

columnDefinition =
    do  spaces
        leftParen
        spaces
        columnName <- sqlName
        spaces
        colDataType <- colDataTypeParser
        spaces
        leftParen
        colDataTypeLen <- many1 digit
        return (ColumnDefinition {
            colName = columnName
            , colType = colDataType
            , colDataLen = colDataTypeLen
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
      d <- columnDefinition
      return [[schemaName, tableName, (colName d), (colType d), (colDataLen d)]]

parseSQL :: String -> Either ParseError [[String]]
parseSQL input = parse query "(unknown)" input
