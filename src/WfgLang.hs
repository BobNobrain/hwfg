module WfgLang
    ( Identifier
    , Value (..)
    , Callable (..)
    , BinaryOperator (..)
    , UnaryOperator (..)
    , Expression (..)
    , Command (..)
    , CommandResult (..)
    ) where

import Helpers

type Identifier = String

data Callable
    = Lambda [Identifier] Expression
    | Subprog [Identifier] Command
    | NativeLambda Int ([Value] -> Value)
    | NativeSubprog Int ([Value] -> IO ())
    | NativeIOLambda Int ([Value] -> IO Value)
    -- deriving (Eq)

genFakeArguments count = map (\i -> ('x':(show i))) (take count [1..])

showCallable :: Int -> Callable -> String
showCallable n (Lambda args _) = "\\" ++ (join ", " $ drop n args) ++ ". [body]"
showCallable _ (Subprog [] _) = "do [commands]"
showCallable n (Subprog args _) = "with " ++ (join ", " $ drop n args) ++ " do [commands]"
showCallable n (NativeLambda c _) = "\\" ++ (join ", " $ genFakeArguments (c - n)) ++ ". [native code]"
showCallable n (NativeSubprog c _) = "with " ++ (join ", " $ genFakeArguments (c - n)) ++ " do [native code]"
showCallable n (NativeIOLambda c _) = "\\" ++ (join ", " $ genFakeArguments (c - n)) ++ ". [native code]"

data Value
    = ValInt Integer
    | ValDouble Double
    | ValString String
    | ValBool Bool
    | ValNothing
    | ValCallable [Value] Callable
    -- deriving (Eq)

instance Show Value where
    show (ValInt i) = show i
    show (ValDouble d) = show d
    show (ValString str) = show str
    show (ValBool True) = "true"
    show (ValBool False) = "false"
    show ValNothing = "(nothing)"
    show (ValCallable boundValues callable) = showCallable (length boundValues) callable

instance Eq Value where
    (==) (ValInt i1) (ValInt i2) = i1 == i2
    (==) (ValDouble d1) (ValDouble d2) = d1 == d2
    (==) (ValString s1) (ValString s2) = s1 == s2
    (==) (ValBool b1) (ValBool b2) = b1 == b2
    (==) ValNothing ValNothing = True
    (==) _ _ = False

data BinaryOperator
    = BinPlus
    | BinMinus
    | BinMult
    | BinDiv
    | BinRemainder

    | BinEq
    | BinNEq
    | BinGT
    | BinLT
    | BinGE
    | BinLE

    | BinAnd
    | BinOr
    | BinXor
    deriving (Show, Eq)

-- binaryOperatorFromString :: String -> BinaryOperator
-- binaryOperatorFromString "+" = BinPlus
-- binaryOperatorFromString "-" = BinMinus
-- binaryOperatorFromString "*" = BinMult
-- binaryOperatorFromString "/" = BinDiv
-- binaryOperatorFromString "%" = BinRemainder
-- binaryOperatorFromString "and" = BinAnd
-- binaryOperatorFromString "or" = BinOr
-- binaryOperatorFromString "xor" = BinXor

data UnaryOperator
    = UnNot
    | UnMinus
    deriving (Show, Eq)

-- unaryOperatorFromString :: String -> UnaryOperator
-- unaryOperatorFromString "not" = UnNot
-- unaryOperatorFromString "-" = UnMinus

data Expression
    = ExprValue Value
    | ExprIdentifier Identifier
    | ExprBinaryOp BinaryOperator Expression Expression
    | ExprUnaryOp UnaryOperator Expression
    | ExprRead String
    | ExprCall [Expression]
    deriving (Show, Eq)

data Command
    = CmdOutput Expression
    | CmdAssign Identifier Expression
    | CmdCondition Expression Command Command
    | CmdWhileLoop Expression Command
    | CmdSubprogCall [Expression]
    | CmdReturn Expression
    | CmdSequence [Command]
    | CmdNoop
    deriving (Show, Eq)

data CommandResult
    = CmdResultEmpty
    | CmdResultEmptyBreak
    | CmdResultValue Value
    deriving (Show)
