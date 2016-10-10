{-# LANGUAGE DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}
module Main where
import Prelude hiding (putStrLn, getContents)
import Lib
import SQLParser
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 (ByteString, putStrLn)
import Data.Text.Encoding (decodeUtf8)
import Data.Text (toLower, pack, unpack)
import Data.Text.IO (getContents)

main = do
    c <- getContents
    let lowercasedContents = unpack $ toLower c

    case parseSQL lowercasedContents of
        Left e -> do putStrLn "Error parsing input:"
                     print e
        Right r -> do
                     putStrLn $ encodePretty r
    return ()
