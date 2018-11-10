module StdLib
    ( evalBinaryOperator
    , evalUnaryOperator
    ) where

import WfgLang

evalBinaryOperator :: BinaryOperator -> Value -> Value -> Maybe Value

-- +
evalBinaryOperator BinPlus (ValInt n1) (ValInt n2) = Just $ ValInt (n1 + n2)
evalBinaryOperator BinPlus (ValDouble n1) (ValInt n2) = Just $ ValDouble (n1 + (fromIntegral n2))
evalBinaryOperator BinPlus (ValInt n1) (ValDouble n2) = Just $ ValDouble ((fromIntegral n1) + n2)
evalBinaryOperator BinPlus (ValDouble n1) (ValDouble n2) = Just $ ValDouble (n1 + n2)
evalBinaryOperator BinPlus (ValString s1) (ValString s2) = Just $ ValString (s1 ++ s2)

-- -
evalBinaryOperator BinMinus (ValInt n1) (ValInt n2) = Just $ ValInt (n1 - n2)
evalBinaryOperator BinMinus (ValDouble n1) (ValInt n2) = Just $ ValDouble (n1 - (fromIntegral n2))
evalBinaryOperator BinMinus (ValInt n1) (ValDouble n2) = Just $ ValDouble ((fromIntegral n1) - n2)
evalBinaryOperator BinMinus (ValDouble n1) (ValDouble n2) = Just $ ValDouble (n1 - n2)

-- *
evalBinaryOperator BinMult (ValInt n1) (ValInt n2) = Just $ ValInt (n1 * n2)
evalBinaryOperator BinMult (ValDouble n1) (ValInt n2) = Just $ ValDouble (n1 * (fromIntegral n2))
evalBinaryOperator BinMult (ValInt n1) (ValDouble n2) = Just $ ValDouble ((fromIntegral n1) * n2)
evalBinaryOperator BinMult (ValDouble n1) (ValDouble n2) = Just $ ValDouble (n1 * n2)

-- /
evalBinaryOperator BinDiv (ValInt n1) (ValInt n2) = Just $ ValInt (n1 `quot` n2)
evalBinaryOperator BinDiv (ValDouble n1) (ValInt n2) = Just $ ValDouble (n1 / (fromIntegral n2))
evalBinaryOperator BinDiv (ValInt n1) (ValDouble n2) = Just $ ValDouble ((fromIntegral n1) / n2)
evalBinaryOperator BinDiv (ValDouble n1) (ValDouble n2) = Just $ ValDouble (n1 / n2)

-- %
evalBinaryOperator BinRemainder (ValInt n1) (ValInt n2) = Just $ ValInt (n1 `mod` n2)

-- ==
evalBinaryOperator BinEq (ValDouble n1) (ValInt n2) = Just $ ValBool (n1 == (fromIntegral n2))
evalBinaryOperator BinEq (ValInt n1) (ValDouble n2) = Just $ ValBool ((fromIntegral n1) == n2)
evalBinaryOperator BinEq v1 v2 = Just $ ValBool (v1 == v2)

-- !=
evalBinaryOperator BinNEq v1 v2 = (evalBinaryOperator BinEq v1 v2) >>= (evalUnaryOperator UnNot)

-- >
evalBinaryOperator BinGT (ValDouble d1) (ValDouble d2) = Just $ ValBool (d1 > d2)
evalBinaryOperator BinGT d@(ValDouble _) (ValInt i) = evalBinaryOperator BinGT d (ValDouble $ fromIntegral i)
evalBinaryOperator BinGT (ValInt i) d@(ValDouble _) = evalBinaryOperator BinGT (ValDouble $ fromIntegral i) d
evalBinaryOperator BinGT (ValInt i1) (ValInt i2) = Just $ ValBool (i1 > i2)

-- >=
evalBinaryOperator BinGE v1 v2 = do
    isGreater <- evalBinaryOperator BinGT v1 v2
    isEqual <- evalBinaryOperator BinEq v1 v2
    evalBinaryOperator BinOr isGreater isEqual

-- <
evalBinaryOperator BinLT v1 v2 = evalBinaryOperator BinGE v2 v1

-- <=
evalBinaryOperator BinLE v1 v2 = evalBinaryOperator BinGT v2 v1

-- and, or, xor
evalBinaryOperator BinAnd (ValBool b1) (ValBool b2) = Just $ ValBool $ b1 && b2
evalBinaryOperator BinOr (ValBool b1) (ValBool b2) = Just $ ValBool $ b1 || b2
evalBinaryOperator BinXor (ValBool b1) (ValBool b2) = Just $ ValBool $ b1 /= b2

evalBinaryOperator _ _ _ = Nothing


evalUnaryOperator :: UnaryOperator -> Value -> Maybe Value
evalUnaryOperator UnNot (ValBool b) = Just $ ValBool $ not b
evalUnaryOperator UnMinus (ValInt i) = Just $ ValInt (-i)
evalUnaryOperator UnMinus (ValDouble d) = Just $ ValDouble (-d)

evalUnaryOperator _ _ = Nothing
