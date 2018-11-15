module Cli
    ( parseArguments
    , WfgMode (..)
    ) where

import qualified System.Log.Logger as Log
import Version (wfgVersion)
import qualified Arguments

data WfgMode
    = ModeRunFile String Log.Priority
    | ModeRunInteractive Log.Priority
    | ModePrintAndExit String Int
    deriving (Show, Eq)

helpMessage = "Usage: wfg [options|filename]\n\
              \Options:\n\
              \    -h, --help      - show this message and exit\n\
              \    -v, --version   - show wfg version and exit\n\
              \    --verbose       - print internal interpreter info\n\
              \    --debug         - print debug info\n\
              \Filename: name a file to interpret\n\
              \\n\
              \If no filename is given, wfg will be run in an\n\
              \interactive mode\n\
              \"

parseArguments :: [String] -> WfgMode

parseArguments args = result where
    argrecord = Arguments.fromArgs args
    hasHelpFlag = Arguments.hasLongOrShort argrecord 'h' "help"
    hasVersionFlag = Arguments.hasLongOrShort argrecord 'v' "version"
    hasVerboseFlag = Arguments.hasLongOption argrecord "verbose"
    hasDebugFlag = Arguments.hasLongOption argrecord "debug"
    filenames = Arguments.arguments argrecord

    logLevel =  if hasDebugFlag then
                    Log.DEBUG
                else if hasVerboseFlag then
                    Log.INFO
                else
                    Log.WARNING


    result = decision hasHelpFlag hasVersionFlag logLevel filenames

    decision True False _ [] = ModePrintAndExit helpMessage 0
    decision False True _ [] = ModePrintAndExit ("wfg " ++ wfgVersion ++ "\n") 0
    decision False False logLevel [] = ModeRunInteractive logLevel
    decision False False logLevel [filename] = ModeRunFile filename logLevel
    decision _ _ _ _ = ModePrintAndExit helpMessage 1
