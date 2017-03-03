module Lib
    ( sqlName
    , leftParen
    , rightParen
    , tableRef
    , fullyQualifiedTable
    , alphaNumInParens
    , numInParens
    , intPairInParens
    , fieldWithOptionalTrailingComma
    , colDataTypeParser
    , colDataLenParser
    , spacesOrComment
    )
where
import Text.ParserCombinators.Parsec ((<|>), (<?>), string, spaces, parse, ParseError
  , alphaNum, many1, char, try, many1, digit, optionMaybe, option, endBy, manyTill)
import Text.Parsec.Prim (ParsecT)
import Text.Parsec.Char (anyChar)
import Data.Functor.Identity (Identity)
import Data.Maybe (fromJust)
import Types

commentStart = string "/*"
commentEnd = string "*/"

multiLineComment = do
    commentStart
    manyTill anyChar (try commentEnd)
    return ()


spacesOrComment :: ParsecT [Char] u Identity ()
spacesOrComment = (try multiLineComment) <|> spaces
    
{-Parses a table or column name-}
sqlName :: ParsecT [Char] u Identity String
sqlName = many1 $ alphaNum <|> char '_'

fullyQualifiedTable = do
    schema <- sqlName
    char '.'
    table <- sqlName
    return table

wildCard = string "*"

tableRef = wildCard <|> fullyQualifiedTable <|> sqlName

leftParen :: ParsecT [Char] u Identity Char
leftParen = char '('

rightParen :: ParsecT [Char] u Identity Char
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
    return ColumnLength {colLen = intVal, colPrecision = Nothing}

{-column_name(max) indicates max int32 size-}
maxInParens = do
    leftParen
    spaces
    string "max"
    spaces
    rightParen
    let intVal = 65535
    return ColumnLength {colLen = intVal, colPrecision = Nothing}

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
    spaces
    rightParen
    return ColumnLength {colLen = val1Int, colPrecision = Just val2Int}

fieldWithOptionalTrailingComma = do
    col <- sqlName
    _ <- optionMaybe $ try $ char ','
    spaces
    return col

colDataTypeParser :: Text.Parsec.Prim.ParsecT [Char] u Identity String
colDataTypeParser = (try $ string "bigint")
        <|> string "boolean"
        <|> string "char"
        <|> (try $ string "date")
        <|> string "decimal"
        <|> string "float"
        <|> (try $ string "integer")
        <|> string "int"
        <|> string "smallint"
        <|> string "timestamp"
        <|> string "varchar"




colDataLenParser  :: Text.Parsec.Prim.ParsecT
           [Char] u Identity (Maybe ColumnLength)
colDataLenParser = do
    spaces
    res <- optionMaybe $ (try intPairInParens) <|> (try numInParens) <|> (try maxInParens)
    spaces
    return res
