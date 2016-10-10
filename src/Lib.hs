module Lib
    ( sqlName
    , leftParen
    , rightParen
    )
where
import Text.ParserCombinators.Parsec ((<|>), (<?>), string, spaces, parse, ParseError
  , alphaNum, many1, char, try, many1, digit, optionMaybe, option, endBy)
import Text.Parsec.Prim (ParsecT)
import Data.Functor.Identity

{-Parses a table or column name-}
sqlName :: Text.Parsec.Prim.ParsecT [Char] u Data.Functor.Identity.Identity [Char]
sqlName = many1 $ alphaNum <|> char '_'


leftParen :: Text.Parsec.Prim.ParsecT [Char] u Data.Functor.Identity.Identity Char
leftParen = char '('

rightParen :: Text.Parsec.Prim.ParsecT [Char] u Data.Functor.Identity.Identity Char
rightParen = char ')'
