module Cli
    ( parseArguments
    , WfgMode (..)
    ) where

import Version (wfgVersion)

data WfgMode
    = ModeRunFile String
    | ModeRunInteractive
    | ModePrintAndExit String Int
    deriving (Show, Eq)

helpMessage = "Usage: wfg [options|filename]\n\
              \Options:\n\
              \    -h, --help      - show this message and exit\n\
              \    -v, --version   - show wfg version and exit\n\
              \Filename: name a file to interpret\n\
              \\n\
              \If no arguments is given, wfg will be run in an\n\
              \interactive mode\n\
              \"

parseArguments :: [String] -> WfgMode

parseArguments [] = ModeRunInteractive

parseArguments ["-h"] = parseArguments ["--help"]
parseArguments ["--help"] = ModePrintAndExit helpMessage 0

parseArguments ["-v"] = parseArguments ["--version"]
parseArguments ["--version"] = ModePrintAndExit ("wfg " ++ wfgVersion ++ "\n") 0

parseArguments [filename] = ModeRunFile filename

parseArguments _ = ModePrintAndExit helpMessage 1
