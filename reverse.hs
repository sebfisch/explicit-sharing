{-# LANGUAGE NoMonomorphismRestriction #-}

-- naive reverse to compare performance of purely functional programs
-- with monadic deterministic programs.

-- to compile, run:
-- ghc -fglasgow-exts -hide-package monads-fd -O2 -o reverse.mon --make reverse.hs

-- $ time ./reverse.fun 20000
-- user	0m8.637s

-- $ time ./reverse.mon 20000
-- user	0m10.995s

-- $ time ./reverse.mcc 20000
-- user	0m14.522s


import Control.Monad.Sharing.Lazy

import System ( getArgs )

main = main_mon

main_fun =
 do n <- liftM (read.head) getArgs
    print . length . rev $ [1..n]

rev []     = []
rev (x:xs) = rev xs ++ [x]

main_mon =
 do n <- liftM (read.head) getArgs
    let result = evalLazy $ length' =<< rev' =<< enumFromTo' 1 n
    mapM_ print (result :: [Int])

length' :: Monad m => List m a -> m Int
length' Nil         = return 0
length' (Cons _ xs) = liftM succ (length' =<< xs)

enumFromTo' :: Monad m => Int -> Int -> m (List m Int)
enumFromTo' from to | from > to = return Nil
                    | otherwise = cons (return from) (enumFromTo' (from+1) to)

rev' :: Monad m => List m a -> m (List m a)
rev' Nil         = nil
rev' (Cons x xs) = (`append`cons x nil) =<< rev' =<< xs

append :: Monad m => List m a -> m (List m a) -> m (List m a)
append Nil         ys = ys
append (Cons x xs) ys = cons x (xs >>= (`append`ys))


-- time ./reverse.fun 20000
-- user	0m8.654s
-- time ./reverse.mon 20000
-- user	0m10.967s
-- time ./reverse.mcc 20000
-- user	0m14.461s
