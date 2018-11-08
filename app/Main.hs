module Main where

import Text.ParserCombinators.Parsec
import WfgLang
import Parser
import Interpreter

printResult :: Either ParseError Command -> IO ()
printResult (Left err) = putStrLn $ show err
printResult (Right c) = putStrLn $ show c

play :: String -> IO ()
play code = do
    let ast = parseWfg "(cli)" code
    case ast of Left err -> putStrLn $ show err
                Right cmd -> runWfg cmd

main :: IO ()
main = do
    printResult $ parseWfg "(1)" "x = 3"
    printResult $ parseWfg "(2)" "y = -1.3 * 0"
    printResult $ parseWfg "(3)" "if true then x = 1; y = 2 else output 4; end"
    printResult $ parseWfg "(4)" "while 4 + 1 == 5 do output 1; end"
