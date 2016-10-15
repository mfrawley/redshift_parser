module Lib
    ( sqlName
    , leftParen
    , rightParen
    , tableRef
    , alphaNumInParens
    , numInParens
    )
where
import Text.ParserCombinators.Parsec ((<|>), (<?>), string, spaces, parse, ParseError
  , alphaNum, many1, char, try, many1, digit, optionMaybe, option, endBy)
import Text.Parsec.Prim (ParsecT)
import Data.Functor.Identity

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
    return val
