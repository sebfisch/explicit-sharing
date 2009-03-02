-- permsort benchmark for Curry
-- compile with `cyc -O2 -o permsort.mcc permsort.curry`

import System       ( getArgs )
import Read         ( readInt )
import Maybe        ( fromJust )
import AllSolutions

sort l | isSorted p = p where p = perm l

isSorted []     = True
isSorted (x:xs) = isSorted' x xs

isSorted' _ []     = True
isSorted' x (y:ys) = x <= y && isSorted' y ys

perm []     = []
perm (x:xs) = insert x (perm xs)

insert  x xs     = (x : xs) ? (insert2 x xs)
insert2 x (y:ys) = y : insert x ys

main = do
  n <- getArgs >>= return . readInt . head
  getAllValues (sort [1..n]) >>= print . length
