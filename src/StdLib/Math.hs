module StdLib.Math
    ( content
    ) where

import WfgLang
import StdLib.Helpers

content :: [(Identifier, Value)]
content = [ ("sin",  mkIOFn 1 (stdTrigonometry sin))
          , ("cos",  mkIOFn 1 (stdTrigonometry cos))
          , ("tan",  mkIOFn 1 (stdTrigonometry tan))
          , ("sinh", mkIOFn 1 (stdTrigonometry sinh))
          , ("cosh", mkIOFn 1 (stdTrigonometry cosh))
          , ("tanh", mkIOFn 1 (stdTrigonometry tanh))
          , ("pow",  mkIOFn 2 stdPow)
          , ("pi" ,  ValDouble pi)
          ]

stdTrigonometry :: (Double -> Double) -> [Value] -> IO Value
stdTrigonometry fn [(ValInt i)] = stdTrigonometry fn [(ValDouble (fromIntegral i))]
stdTrigonometry fn [(ValDouble d)] = return $ ValDouble (fn d)
stdTrigonometry _ _ = fail "Invalid arguments for sin function"

stdPow :: [Value] -> IO Value
stdPow [(ValDouble d1), (ValDouble d2)] = return $ ValDouble (d1 ** d2)
stdPow [(ValInt i1), (ValInt i2)] = return $ ValInt (i1 ^ i2)
stdPow [(ValDouble d), (ValInt i)] = return $ ValDouble (d ^ i)
stdPow [(ValInt i), d@(ValDouble _)] = stdPow [(ValDouble (fromIntegral i)), d]
stdPow _ = fail "Invalid arguments for pow function"
