module Runner
    ( run
    ) where

import System.Exit
import System.Environment
import System.IO
import Control.Monad (forever, when)

import Cli
import Parser
import Interpreter
import qualified Memory


run :: IO ()
run = do
    args <- getArgs
    runWithMode $ parseArguments args


runWithMode :: WfgMode -> IO ()

runWithMode (ModePrintAndExit msg code) = do
    putStr msg
    exitWith (if code == 0 then ExitSuccess else ExitFailure code)

runWithMode (ModeRunFile filename) = do
    fileH <- openFile filename ReadMode
    program <- hGetContents fileH
    startInterpreter $ parseWfg filename program
    hClose fileH
    where
        startInterpreter (Left err) = fail $ show err
        startInterpreter (Right ast) = do
            -- print ast
            runWfg ast

runWithMode ModeRunInteractive = do
    memory <- Memory.create
    forever $ readCommand "" >>= eval memory
    where
        readCommand str = do
            putStr (if str == "" then "wfg>" else "...>")
            hFlush stdout
            newData <- getLine
            when (newData == ":exit") (exitWith ExitSuccess)
            let fullCode = (str ++ "\n" ++ newData)
            case parseExprOrCommand "(input)" fullCode of Left err -> readCommand fullCode
                                                          Right eoc -> return eoc

        eval memory (Right expr) = do
            val <- evalWfg memory expr
            putStrLn $ show val

        eval memory (Left cmd) = runWfgWithState memory cmd
