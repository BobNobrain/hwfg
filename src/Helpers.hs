module Helpers
    ( join
    ) where

join _ [] = ""
join _ [single] = single
join sep (s:ss) = s ++ sep ++ (join sep ss)
