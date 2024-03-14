module Main where

import System.IO (hFlush, stdout)
import Parser

prompt :: IO ()
prompt = do
    putStr "$ "
    hFlush stdout

main :: IO ()
main = do
    prompt
    line <- getLine
    print line
    case term line of
        Left e -> putStrLn e
        Right t -> print t
    main
