module StdLib.Combinators
    ( content
    ) where

import StdLib.Helpers

content = [ ("I", mkMapper 1 stdI)
          , ("K", mkMapper 2 stdK)
        --   , ("S", stdS)
        --   , ("B", stdB)
        --   , ("C", stdC)
        --   , ("W", stdW)
        --   , ("Y", stdY)
          ]

stdI [anything] = anything
stdK [anything, _] = anything
