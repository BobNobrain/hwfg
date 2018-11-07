module WfgLang
    ( Identifier
    , Value (..)
    , BinaryOperator (..)
    -- , binaryOperatorFromString
    , UnaryOperator (..)
    -- , unaryOperatorFromString
    , Expression (..)
    , Command (..)
    ) where

type Identifier = String

data Value
    = ValInt Integer
    | ValDouble Double
    | ValString String
    | ValBool Bool
    deriving (Show, Eq)

data BinaryOperator
    = BinPlus
    | BinMinus
    | BinMult
    | BinDiv
    | BinRemainder
    | BinEq
    | BinNEq
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
    | ExprRead
    deriving (Show, Eq)

data Command
    = CmdOutput Expression
    | CmdAssign Identifier Expression
    | CmdCondition Expression Command Command
    | CmdWhileLoop Expression Command
    | CmdSequence [Command]
    | CmdNoop
    deriving (Show, Eq)
