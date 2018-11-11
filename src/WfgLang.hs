module WfgLang
    ( Identifier
    , Value (..)
    , BinaryOperator (..)
    , UnaryOperator (..)
    , Expression (..)
    , Command (..)
    ) where

type Identifier = String

data Value
    = ValInt Integer
    | ValDouble Double
    | ValString String
    | ValBool Bool
    | ValLambda [Identifier] [Value] Expression
    | ValSubprog [Identifier] Command
    deriving (Eq)

join _ [] = ""
join _ [single] = single
join sep (s:ss) = s ++ sep ++ (join sep ss)

instance Show Value where
    show (ValInt i) = show i
    show (ValDouble d) = show d
    show (ValString str) = show str
    show (ValBool True) = "true"
    show (ValBool False) = "false"
    show (ValLambda vars boundValues _) = "lambda " ++ (join ", " (drop (length boundValues) vars))
    show (ValSubprog vars _) = "subprog " ++ (join "," vars)

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
    | CmdSequence [Command]
    | CmdNoop
    deriving (Show, Eq)
