module Interpreter
    ( WfgError
    , runWfg
    , runWfgWithState
    , evalWfg
    ) where

import WfgLang
import StdLib
import Parser (parseInput)
import System.IO
-- import Data.Hashable
-- import qualified Data.HashTable.IO as H
import qualified Memory

type WfgError = String


runWfg :: Command -> IO ()
runWfg cmd = do
    memory <- Memory.create
    runWfgWithState memory cmd


runWfgWithState :: Memory.Memory -> Command -> IO ()
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
    Memory.set memory name value
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

-- do I [args]
runWfgWithState memory (CmdSubprogCall exprs) = do
    values <- mapM (evalWfg memory) exprs
    case (head values) of (ValSubprog args body) -> runSubprog memory args (tail values) body
                          anything -> fail ((show anything) ++ " is not a subprog")
    where
        runSubprog memory args values body = do
            scope <- createScope memory args values
            runWfgWithState scope body


evalWfg :: Memory.Memory -> Expression -> IO Value
-- value
evalWfg _ (ExprValue val) = return val

-- I
evalWfg memory (ExprIdentifier name) = do
    mbval <- Memory.get memory name
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

evalWfg memory (ExprCall exprs) = do
    values <- mapM (evalWfg memory) exprs
    case (head values)
        of (ValLambda args boundValues body) -> evalLambda memory args (boundValues ++ (tail values)) body
           e -> fail ((show e) ++ " is not callable")
    where
        evalLambda :: Memory.Memory -> [Identifier] -> [Value] -> Expression -> IO Value
        evalLambda memory args values body =
            if (length args) <= (length values) then do
                scope <- createScope memory args values
                result <- evalWfg scope body
                let restValues = drop (length args) values
                if (length restValues) > 0 then
                    evalWfg memory $ ExprCall $ map ExprValue (result:restValues)
                else
                    return result
            else
                return $ ValLambda args values body

evalWfg memory (ExprNativeCall body) = body memory


createScope :: Memory.Memory -> [Identifier] -> [Value] -> IO Memory.Memory
createScope memory names values = do
    scope <- Memory.push memory
    Memory.fill scope (zip names values)
    return scope
