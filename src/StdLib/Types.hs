module StdLib.Types
    ( content
    ) where

import WfgLang
import StdLib.Helpers

content :: [(Identifier, Value)]
content = [ ("int",    mkIOFn 1 stdInt)
          , ("string", mkIOFn 1 stdString)]

stdInt :: IOFn
stdInt [i@(ValInt _)] = return i
stdInt [(ValDouble d)] = return $ ValInt (floor d)
stdInt [other] = fail ("Cannot convert " ++ (show other) ++ " to int")
stdInt _ = fail "Incorrect int call"

stdString :: IOFn
stdString [(ValString s)] = return $ ValString s
stdString [(ValCallable _ _)] = fail "Cannot convert callable value to string"
stdString [other] = return $ ValString $ show other
