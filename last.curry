-- last benchmark for Curry
-- compile with `cyc -O2 -o last.mcc last.curry`

-- $ time ./last.mcc 1000000 +RTS -h1000m -k20m
-- 1000000
-- user	0m6.327s
-- $ time ./last.mcc 10000000 +RTS -h2000m -k50m
-- Not enough free memory after garbage collection

import System       ( getArgs )
import Read         ( readInt )
import AllSolutions

main = do n <- getArgs >>= return . readInt . head
          getAllValues (last (replicate n True)) >>= mapIO_ print

last :: [a] -> a
last l | l =:= xs ++ [x] = x where x,xs free

