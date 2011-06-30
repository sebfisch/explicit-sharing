{-# LANGUAGE NoMonomorphismRestriction #-}

-- naive reverse to compare performance of purely functional programs
-- with monadic deterministic programs.

-- to compile, run:
-- ghc -O2 -o reverse.mon --make reverse.hs

-- $ time ./reverse.fun 10000
-- real 0m1.942s
-- user 0m1.920s

-- $ time ./reverse.mon 10000
-- real 0m10.774s
-- user 0m10.730s

-- $ time ./reverse.mcc 10000
-- real 0m1.945s
-- user 0m1.410s


import Control.Monad.Sharing
import Data.Monadic.List

import System ( getArgs )

main = main_mon

main_fun =
 do n <- liftM (read.head) getArgs
    print . length . rev $ [1..n]

rev []     = []
rev (x:xs) = rev xs ++ [x]

main_mon =
 do n <- liftM (read.head) getArgs
    result <- resultList (convert=<<(length'=<<rev'=<<convert[(1::Int)..n]))
    mapM_ print (result :: [Int])

length' :: Monad m => List m a -> m Int
length' Nil         = return 0
length' (Cons _ xs) = liftM succ (length' =<< xs)

rev' :: Monad m => List m Int -> m (List m Int)
rev' Nil         = nil
rev' (Cons x xs) = (`append`cons x nil) =<< rev' =<< xs

append :: Monad m => List m a -> m (List m a) -> m (List m a)
append Nil         ys = ys
append (Cons x xs) ys = cons x (xs >>= (`append`ys))
