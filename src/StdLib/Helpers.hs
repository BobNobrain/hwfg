module StdLib.Helpers
    ( IOFn
    , mkIOFn
    , ValueMapper
    , mkMapper
    ) where

import WfgLang

type IOFn = [Value] -> IO Value
type ValueMapper = [Value] -> Value

mkIOFn :: Int -> IOFn -> Value
mkIOFn numArgs body = ValCallable [] $ NativeIOLambda numArgs body

mkMapper :: Int -> ValueMapper -> Value
mkMapper numArgs body = ValCallable [] $ NativeLambda numArgs body
