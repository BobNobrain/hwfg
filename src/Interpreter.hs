module Interpreter
    ( WfgError
    , runWfg
    , runWfgWithState
    , evalWfg
    , initializeMemory
    ) where

import WfgLang
import StdLib
import Parser (parseInput)
import System.IO
import Data.Hashable
import qualified Data.HashTable.IO as H

type Memory = H.BasicHashTable Identifier Value
type WfgError = String


initializeMemory = H.new

runWfg :: Command -> IO ()
runWfg cmd = do
    memory <- initializeMemory
    runWfgWithState memory cmd


runWfgWithState :: Memory -> Command -> IO ()
runWfgWithState _ CmdNoop = return ()

-- C1; C2
runWfgWithState memory (CmdSequence (c:cs)) = do
    runWfgWithState memory c
    runWfgWithState memory (CmdSequence cs)

runWfgWithState _ (CmdSequence []) = return ()

-- output E
runWfgWithState memory (CmdOutput expr) = do
    value <- evalWfg memory expr
    case value of (ValString str) -> putStrLn str
                  val -> print val

-- I = E
runWfgWithState memory (CmdAssign name expr) = do
    value <- evalWfg memory expr
    H.insert memory name value
    return ()

-- if E then C else C end
runWfgWithState memory (CmdCondition condition thenBranch elseBranch) = do
    cond <- evalWfg memory condition
    case cond of ValBool True -> runWfgWithState memory thenBranch
                 ValBool False -> runWfgWithState memory elseBranch
                 _ -> fail "Not a boolean value in if condition"

-- while E do C end
runWfgWithState memory cmd@(CmdWhileLoop condition loop) = do
    cond <- evalWfg memory condition
    case cond of ValBool True -> do
                                    runWfgWithState memory loop
                                    runWfgWithState memory cmd
                 ValBool False -> return ()
                 _ -> fail "Not a boolean value in while condition"

evalWfg :: Memory -> Expression -> IO Value
-- value
evalWfg _ (ExprValue val) = return val

-- I
evalWfg memory (ExprIdentifier name) = do
    mbval <- H.lookup memory name
    case mbval of Just val -> return val
                  Nothing -> fail ("Access to undefined variable '" ++ name ++ "'")

-- a . b
evalWfg memory (ExprBinaryOp op left right) = do
    leftv <- evalWfg memory left
    rightv <- evalWfg memory right
    let mbresult = evalBinaryOperator op leftv rightv
    case mbresult of Just val -> return val
                     Nothing -> fail (
                         "Operator " ++ (show op) ++ " cannot be applied to " ++ (show leftv) ++ ", " ++ (show rightv)
                       )

-- .a
evalWfg memory (ExprUnaryOp op expr) = do
    value <- evalWfg memory expr
    let mbval = evalUnaryOperator op value
    case mbval of Just val -> return val
                  Nothing -> fail ("Operator" ++ (show op) ++ " cannot be applied to " ++ (show value))

-- read [prompt]
evalWfg m (ExprRead str) = do
    putStr str
    hFlush stdout
    input <- getLine
    let val = parseInput input
    case val of Right v -> return v
                Left err -> do
                                putStrLn $ show err
                                -- repeat
                                evalWfg m (ExprRead str)
