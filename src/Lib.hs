module Lib
    ( parseCSV
    )
where

-- import Text.ParserCombinators.Parsec
import Text.Megaparsec.Combinator

quotedCell =
    do char '"'
       content <- many quotedChar
       char '"' <?> "quote at end of cell"
       return content

quotedChar =
        noneOf "\""
    <|> try (string "\"\"" >> return '"')

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

tabOrFail = tab <?> "Tab char"
line = sepBy cell tabOrFail
cell = quotedCell <|> many (noneOf ",\n\r")

csvFile = endBy line eol

-- comma = char ','

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input
