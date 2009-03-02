{-# LANGUAGE NoMonomorphismRestriction #-}

-- naive reverse benchmark for Curry
-- compile with `cyc -O2 -o reverse.mcc reverse.curry`

import System ( getArgs )
import Read   ( readInt )

main =
 do n <- getArgs >>= return . readInt . head
    print . length . rev $ [1..n]

rev []     = []
rev (x:xs) = rev xs ++ [x]

