{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module SelectQueryParser

where
import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)
import Text.ParserCombinators.Parsec ((<|>), (<?>), string, spaces, parse, ParseError
  , alphaNum, many1, char, try, many1, digit, optionMaybe, option, endBy)
import Data.Char (toLower, toUpper)
import Text.Parsec.Prim (ParsecT)
import Data.Functor.Identity
import Data.Maybe
