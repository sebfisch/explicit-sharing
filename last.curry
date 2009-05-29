-- last benchmark for Curry
-- compile with `cyc -O2 -o last.mcc last.curry`

import System       ( getArgs )
import Read         ( readInt )
import AllSolutions

main = do n <- getArgs >>= return . readInt . head
          getAllValues (last (replicate n True)) >>= mapIO_ print

last :: [a] -> a
last l | l =:= xs ++ [x] = x where x,xs free

