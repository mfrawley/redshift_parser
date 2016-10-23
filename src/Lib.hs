module Lib
    ( sqlName
    , leftParen
    , rightParen
    , tableRef
    , fullyQualifiedTable
    , alphaNumInParens
    , numInParens
    , fieldWithOptionalTrailingComma
    , colDataTypeParser
    , colDataLenParser
    )
where
import Text.ParserCombinators.Parsec ((<|>), (<?>), string, spaces, parse, ParseError
  , alphaNum, many1, char, try, many1, digit, optionMaybe, option, endBy)
import Text.Parsec.Prim (ParsecT)
import Data.Functor.Identity
import Types

{-Parses a table or column name-}
sqlName :: Text.Parsec.Prim.ParsecT [Char] u Data.Functor.Identity.Identity String
sqlName = many1 $ alphaNum <|> char '_'

fullyQualifiedTable = do
    table <- sqlName
    char '.'
    col <- sqlName
    return table

wildCard = string "*"

tableRef = wildCard <|> fullyQualifiedTable <|> sqlName

leftParen :: Text.Parsec.Prim.ParsecT [Char] u Data.Functor.Identity.Identity Char
leftParen = char '('

rightParen :: Text.Parsec.Prim.ParsecT [Char] u Data.Functor.Identity.Identity Char
rightParen = char ')'

alphaNumInParens = do
    leftParen
    spaces
    val <- sqlName
    spaces
    rightParen
    return val

numInParens = do
    leftParen
    spaces
    val <- (many1 digit)
    spaces
    rightParen
    let intVal = read val :: Int
    return ColumnLength {colLen = intVal, colPrecision = 0}

-- Used in decimal precision - e.g. (12,2)
intPairInParens = do
    leftParen
    spaces
    val1 <- (many1 digit)
    let val1Int = read val1 :: Int
    char ','
    spaces
    val2 <- (many1 digit)
    let val2Int = read val2 :: Int
    return ColumnLength {colLen = val1Int, colPrecision = val2Int}

fieldWithOptionalTrailingComma = do
    col <- sqlName
    _ <- optionMaybe $ try $ char ','
    spaces
    return col

colDataTypeParser :: Text.Parsec.Prim.ParsecT [Char] u Data.Functor.Identity.Identity String
colDataTypeParser = string "int"
        <|> string "varchar"
        <|> string "float"
        <|> string "timestamp"
        <|> string "bigint"
        <|> string "date"
        <|> string "decimal"

colDataLenParser  :: Text.Parsec.Prim.ParsecT
           [Char] u Data.Functor.Identity.Identity (Maybe ColumnLength)
colDataLenParser = do
    spaces
    l <- try $ optionMaybe $ intPairInParens <|> numInParens
    spaces
    return l
