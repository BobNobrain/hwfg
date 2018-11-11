module Memory
    ( Memory
    , create
    , fill
    , get
    , set
    , push
    , pop
    ) where

import qualified Data.HashTable.IO as H
import WfgLang

type Layer = H.BasicHashTable Identifier Value

type Memory = [Layer]

create :: IO Memory
create = push []

createLayer :: IO Layer
createLayer = H.new

fill :: Memory -> [(Identifier, Value)] -> IO ()
fill memory content = mapM_ (\(key, value) -> set memory key value) content

get :: Memory -> Identifier -> IO (Maybe Value)
get (l:ls) name = do
    mbval <- H.lookup l name
    case mbval of Nothing -> get ls name
                  justVal -> return justVal

get [] _ = return Nothing

set :: Memory -> Identifier -> Value -> IO ()
set (l:ls) name value = do
    H.insert l name value

set [] _ _ = fail "Invalid memory given"

push :: Memory -> IO Memory
push memory = do
    layer <- createLayer
    return (layer:memory)

pop :: Memory -> IO Memory
pop (l:ls) = return ls
pop [] = fail "Cannot pop root scope"
