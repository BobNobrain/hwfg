module Runner
    ( run
    ) where

import System.Exit
import System.Environment
import System.IO
import Control.Monad (forever, when)
import Control.Exception (IOException, catch)

import qualified System.Log.Logger as Log
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (streamHandler)
import System.Log.Formatter (simpleLogFormatter)

import Cli
import Parser
import Interpreter
import Memory


component = "Runner"
debugM = Log.debugM component
errorM = Log.errorM component


run :: IO ()
run = do
    args <- getArgs
    runWithMode $ parseArguments args


initLoggers :: Log.Priority -> IO ()
initLoggers logLevel = do
    let fmt = simpleLogFormatter "[$prio] $loggername: $msg"
    stderrHandle <- (streamHandler stderr logLevel)
    let updater = (Log.setLevel logLevel . Log.addHandler (setFormatter stderrHandle fmt))
    Log.updateGlobalLogger "Runner" updater
    Log.updateGlobalLogger "Interpreter" updater
    debugM ("Set log level to " ++ (show logLevel))


runWithMode :: WfgMode -> IO ()

runWithMode (ModePrintAndExit msg code) = do
    putStr msg
    exitWith (if code == 0 then ExitSuccess else ExitFailure code)

runWithMode (ModeRunFile filename logLevel) = do
    initLoggers logLevel

    debugM ("Opening ReadMode file " ++ filename)
    fileH <- openFile filename ReadMode
    debugM "Reading file content"
    program <- hGetContents fileH
    debugM "Running parser"
    startInterpreter $ parseWfg filename program
    debugM "Closing input file"
    hClose fileH
    where
        startInterpreter (Left err) = do
            errorM ("Parse error: " ++ (show err))
            fail $ show err
        startInterpreter (Right ast) = do
            debugM "Running interpreter"
            runWfg ast

runWithMode (ModeRunInteractive logLevel) = do
    initLoggers logLevel

    debugM "Initializing memory"
    memory <- createMemoryAndFill
    debugM "Starting main loop"
    forever $ catch (readCommand memory "" >>= eval memory) printErr
    where
        readCommand memory str = do
            putStr (if str == "" then "wfg>" else "...>")
            hFlush stdout
            newData <- getLine
            if newData `elem` interactiveCmds then do
                execInteractiveCmd memory newData
                readCommand memory str
            else do
                let fullCode = (str ++ "\n" ++ newData)
                case parseExprOrCommand "(input)" fullCode of Left err -> readCommand memory fullCode
                                                              Right eoc -> return eoc

        eval memory (Right expr) = do
            debugM "Evalutating expression"
            val <- evalWfg memory expr
            putStrLn $ "-> " ++ (show val)

        eval memory (Left cmd) = do
            debugM "Running command"
            runWfgWithState memory cmd
            return ()

        printErr :: IOException -> IO ()
        printErr e = putStrLn ("Eval error: " ++ (show e))

        interactiveCmds = [":exit", ":e", ":break", ":b", ":q", ":dump"]

        execInteractiveCmd _ ":exit" = exitWith ExitSuccess
        execInteractiveCmd m ":e" = execInteractiveCmd m ":exit"
        execInteractiveCmd m ":q" = execInteractiveCmd m ":exit"
        execInteractiveCmd _ ":break" = fail "Interrupted"
        execInteractiveCmd m ":b" = execInteractiveCmd m ":break"
        execInteractiveCmd m ":dump" = Memory.dump True m >>= putStrLn
        execInteractiveCmd _ c = putStrLn ("Unknown interactive command: " ++ c)
