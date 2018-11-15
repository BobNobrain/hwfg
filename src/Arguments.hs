module Arguments
    ( Arguments (..)
    , fromArgs
    , hasOption
    , hasLongOption
    , hasLongOrShort
    ) where

import Data.List (elemIndex)


data Arguments = Arguments  { options :: [Char]
                            , longOptions :: [String]
                            , arguments :: [String]
                            }

empty = Arguments { options=[], longOptions=[], arguments=[] }

appendLongOption args v = args{ longOptions=(v:(longOptions args)) }
appendOption args v = args{ options=(v:(options args)) }
appendArgument args v = args{ arguments=(v:(arguments args)) }


fromArgs :: [String] -> Arguments
fromArgs args = foldl processNext empty args where
    processNext result ('-':'-':opname) = appendLongOption result opname
    processNext result ('-':opname:[]) = appendOption result opname
    processNext result argval = appendArgument result argval


genericHas :: (Eq a) => (Arguments -> [a]) -> Arguments -> a -> Bool
genericHas prop args opname = case opname `elemIndex` (prop args) of Just _ -> True
                                                                     Nothing -> False

hasOption :: Arguments -> Char -> Bool
hasOption = genericHas options

hasLongOption :: Arguments -> String -> Bool
hasLongOption = genericHas longOptions

hasLongOrShort :: Arguments -> Char -> String -> Bool
hasLongOrShort args opname lopname = (hasOption args opname) || (hasLongOption args lopname)
