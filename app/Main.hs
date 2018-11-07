module Main where

import Text.ParserCombinators.Parsec
import WfgLang
import Parser

printResult :: Either ParseError Expression -> IO ()
printResult (Left err) = putStrLn $ show err
printResult (Right v) = putStrLn $ show v

main :: IO ()
main = do
    printResult $ parseWfg "(1)" "'hello, wfg' + '!'"
    printResult $ parseWfg "(2)" "-1.3 * 0"
    printResult $ parseWfg "(3)" "true and false"
    printResult $ parseWfg "(4)" "x / 3.14"
