module Main where

import AltitudePrinter

main :: IO ()
main = putStrLn . prettyPrint =<< getContents
