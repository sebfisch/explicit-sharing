-- shared tree benchmark for Curry (deterministic with a lot of sharing)
-- compile with `cyc -O2 -o shared-tree.mcc shared-tree.curry`

import System ( getArgs )
import Read   ( readInt )

main = do n <- getArgs >>= return . readInt . head
          print . leafCount . complete $ n

data Tree = Leaf | Fork Tree Tree

leafCount :: Tree -> Int
leafCount Leaf       = 1
leafCount (Fork l r) = leafCount l + leafCount r

complete :: Int -> Tree
complete n | n > 0     = let t = complete (n-1) in Fork t t
           | otherwise = Leaf

