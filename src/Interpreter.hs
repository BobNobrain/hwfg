module Interpreter
    ( WfgError
    , runWfg
    , runWfgWithState
    , evalWfg
    , createMemoryAndFill
    ) where

import System.IO
import qualified System.Log.Logger as Log
import WfgLang
import Parser (parseInput)
import qualified Memory
import qualified StdLib.All as StdLib
import StdLib.Operators (evalBinaryOperator, evalUnaryOperator)


component = "Interpreter"
debugM = Log.debugM component
infoM = Log.infoM component
errorM = Log.errorM component


type WfgError = String

createMemoryAndFill :: IO Memory.Memory
createMemoryAndFill = do
    root <- Memory.create
    debugM "Filling memory with stdlib content"
    Memory.fill root StdLib.content
    memDump <- Memory.dump True root
    debugM ("Memory is: " ++ memDump)
    memory <- Memory.push root
    return memory

runWfg :: Command -> IO ()
runWfg cmd = do
    memory <- createMemoryAndFill
    infoM "Initial state: (m, i, []) where m contains only builtin functions"
    runWfgWithState memory cmd
    return ()


runWfgWithState :: Memory.Memory -> Command -> IO CommandResult
runWfgWithState _ CmdNoop = return CmdResultEmpty

-- C1; C2
runWfgWithState memory (CmdSequence (c:cs)) = do
    debugM "run[C1; C2]"
    r <- runWfgWithState memory c
    case r of CmdResultEmpty -> runWfgWithState memory (CmdSequence cs)
              other -> (debugM ("C1 returned " ++ (show other))) >> (return other)

runWfgWithState _ (CmdSequence []) = return CmdResultEmpty

-- output E
runWfgWithState memory (CmdOutput expr) = do
    debugM "run[output E;]"
    value <- evalWfg memory expr
    infoM ("Modifying state: (m, i, o) -> (m, i, (" ++ (show value) ++ "):o)")
    case value of (ValString str) -> putStrLn str
                  val -> print val
    return CmdResultEmpty

-- I = E
runWfgWithState memory (CmdAssign name expr) = do
    debugM "run[I = E;]"
    value <- evalWfg memory expr
    infoM ("Modifying state: (m, i, o) -> (m', i, o) where m'[" ++ name ++ "] = " ++ (show value) ++ ", m'[*] = m[*]")
    Memory.set memory name value
    memDump <- Memory.dump False memory
    infoM ("    m' = " ++ memDump)
    return CmdResultEmpty

-- if E then C else C end
runWfgWithState memory (CmdCondition condition thenBranch elseBranch) = do
    debugM "run[if E then C else C end]"
    cond <- evalWfg memory condition
    case cond of ValBool True -> runWfgWithState memory thenBranch
                 ValBool False -> runWfgWithState memory elseBranch
                 _ -> fail "Not a boolean value in if condition"

-- while E do C end
runWfgWithState memory cmd@(CmdWhileLoop condition loop) = do
    debugM "run[while E do C end]"
    cond <- evalWfg memory condition
    case cond of ValBool True -> do
                                    runWfgWithState memory loop
                                    runWfgWithState memory cmd
                 ValBool False -> return CmdResultEmpty
                 _ -> fail "Not a boolean value in while condition"


-- do (subprog) args
runWfgWithState memory (CmdSubprogCall exprs) = do
    debugM "run[do E;]"
    apply memory exprs
    return CmdResultEmpty

-- return E
runWfgWithState memory (CmdReturn expr) = do
    debugM "run[return E;]"
    ret <- evalWfg memory expr
    return $ CmdResultValue ret


evalWfg :: Memory.Memory -> Expression -> IO Value
-- value
evalWfg _ (ExprValue val) = return val

-- I
evalWfg memory (ExprIdentifier name) = do
    debugM "eval[I]"
    mbval <- Memory.get memory name
    case mbval of Just val -> return val
                  Nothing -> fail ("Access to undefined variable '" ++ name ++ "'")

-- a . b
evalWfg memory (ExprBinaryOp op left right) = do
    debugM "eval[E $ E]"
    leftv <- evalWfg memory left
    rightv <- evalWfg memory right
    let mbresult = evalBinaryOperator op leftv rightv
    case mbresult of Just val -> return val
                     Nothing -> fail (
                         "Operator " ++ (show op) ++ " cannot be applied to " ++ (show leftv) ++ ", " ++ (show rightv)
                       )

-- .a
evalWfg memory (ExprUnaryOp op expr) = do
    debugM "eval[$ E]"
    value <- evalWfg memory expr
    let mbval = evalUnaryOperator op value
    case mbval of Just val -> return val
                  Nothing -> fail ("Operator" ++ (show op) ++ " cannot be applied to " ++ (show value))

-- read [prompt]
evalWfg m (ExprRead str) = do
    debugM "eval[read]"
    putStr str
    hFlush stdout
    input <- getLine
    let val = parseInput input
    case val of Right v -> do
                                infoM  "Modifying state: (m, (i:is), o) -> (m, is, o)"
                                infoM ("                 where i = " ++ (show v))
                                return v
                Left err -> do
                                putStrLn $ show err
                                -- repeat
                                evalWfg m (ExprRead str)

evalWfg memory (ExprCall exprs) = do
    debugM "eval[E E*]"
    apply memory exprs


createScope :: Memory.Memory -> [Identifier] -> [Value] -> IO Memory.Memory
createScope memory names values = do
    scope <- Memory.push memory
    debugM "Creating scope"
    Memory.fill scope (zip names values)
    memDump <- Memory.dump False scope
    infoM "Modifying state: creating memory scope"
    infoM ("Now memory is" ++ memDump)
    return scope

apply :: Memory.Memory -> [Expression] -> IO Value
apply memory exprs = do
    (callable:arguments) <- mapM (evalWfg memory) exprs
    apply' callable arguments
    where
        -- apply' anyValue [] = return anyValue
        apply' (ValCallable boundValues callable) values = applyCallable callable boundValues values
        apply' uncallableValue [] = return uncallableValue
        apply' uncallableValue _ = fail (show uncallableValue ++ " is not callable")

        applyCallable callable boundValues (v:vs) = do
            if (expectedArgsCount callable) == (length (boundValues)) + 1 then do
                result <- evaluate callable (boundValues ++ [v])
                apply' result vs
            else
                applyCallable callable (boundValues ++ [v]) vs

        -- applying a subprog to empty arguments list should be done
        applyCallable s@(Subprog _ _) boundValues [] = evaluate s boundValues
        applyCallable anotherCallable boundValues [] = return $ ValCallable boundValues anotherCallable

        expectedArgsCount (Lambda args _) = length args
        expectedArgsCount (Subprog args _) = length args
        expectedArgsCount (NativeLambda c _) = c
        expectedArgsCount (NativeSubprog c _) = c
        expectedArgsCount (NativeIOLambda c _) = c

        evaluate (Lambda args body) values = do
            scope <- createScope memory args values
            result <- evalWfg scope body
            return result

        evaluate (Subprog args body) values = do
            scope <- createScope memory args values
            result <- runWfgWithState scope body
            case result of CmdResultValue val -> return val
                           _ -> return ValNothing

        evaluate (NativeLambda _ nativeCode) values = return $ nativeCode values
        evaluate (NativeSubprog _ nativeCode) values = nativeCode values >> return ValNothing
        evaluate (NativeIOLambda _ nativeCode) values = do
            result <- nativeCode values
            return result
