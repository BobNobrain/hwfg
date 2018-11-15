module Memory
    ( Memory
    , create
    , fill
    , get
    , set
    , push
    , pop
    , dump
    ) where

import qualified Data.HashTable.IO as H
import Helpers
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


dump :: Bool -> Memory -> IO String
dump _ [] = return "(empty memory)"
dump showRoot (root:[]) = do
    layer <- dumpl root
    return ("(root scope: " ++ (if showRoot then layer else "...") ++ ")")

dump showRoot (l:ls) = do
    hd <- dumpl l
    tl <- dump showRoot ls
    return ("(scope: " ++ hd ++ "), " ++ tl)

dumpl :: Layer -> IO String
dumpl layer = do
    entries <- H.foldM folder [] layer
    return $ join ", " (map dumpvar entries)
    where
        folder acc (name, value) = return ((name, value):acc)
        dumpvar (name, value) = name ++ "=" ++ (show value)
