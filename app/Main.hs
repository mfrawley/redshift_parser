{-# LANGUAGE DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}
module Main where
import Prelude hiding (interact, concat, unlines, null, putStrLn)
import Lib
import SQLParser
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 (ByteString, interact, unlines, null, putStrLn)

main =
    do c <- getContents
       case parseSQL c of
            Left e -> do putStrLn "Error parsing input:"
                         print e
            Right r -> do
                         putStrLn $ encodePretty r
