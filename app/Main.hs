module Main where
import Lib
import SQLParser

main =
    do c <- getContents
       case parseSQL c of
            Left e -> do putStrLn "Error parsing input:"
                         print e
            Right r -> print r
