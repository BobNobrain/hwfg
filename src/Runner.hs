module Runner
    ( run
    ) where

import System.Exit
import System.Environment
import System.IO

import Cli
import Parser
import Interpreter


run :: IO ()
run = do
    args <- getArgs
    runWithMode $ parseArguments args


runWithMode :: WfgMode -> IO ()
runWithMode (ModePrintAndExit msg code) = do
    putStr msg
    exitWith (if code == 0 then ExitSuccess else ExitFailure code)

runWithMode ModeRunInteractive = do
    putStrLn "Interactive mode is not implemented yet"
    exitWith $ ExitFailure 5

runWithMode (ModeRunFile filename) = do
    fileH <- openFile filename ReadMode
    program <- hGetContents fileH
    startInterpreter $ parseWfg filename program
    hClose fileH

startInterpreter (Left err) = fail $ show err
startInterpreter (Right ast) = do
    print ast
    runWfg ast
