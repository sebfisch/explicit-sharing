{-# LANGUAGE NoMonomorphismRestriction #-}

-- to compile, run:
-- ghc -fglasgow-exts -O2 --make permsort.hs

-- modify import in Control/Monad/Sharing/Lazy.hs to compare implementations.

-- time ./permsort.state 20
-- user	4m56.330s
-- time ./permsort.contreader 20
-- user	2m5.526s
-- time ./permsort.contreadernothunks 20
-- user	1m26.898s
-- time ./permsort.contreadernothunksinlined 20
-- user	0m7.400s

-- time ./permsort.mcc 20
-- user	0m25.067s


import Control.Monad.Sharing.Lazy

import System ( getArgs )

import List

main = do
  n <- liftM (read.head) getArgs
  print . length . evalLazy . sort . foldr cons nil . map return $
   [1..n]



perm :: MonadPlus m => List m a -> m (List m a)
perm Nil         = nil
perm (Cons x xs) = insert x (perm =<< xs)

insert :: MonadPlus m => m a -> m (List m a) -> m (List m a)
insert e l = cons e l
     `mplus` do
        Cons x xs <- l
        cons x (insert e xs)

sort :: Sharing m => m (List m Int) -> m (List m Int)
sort l = do
  xs <- share (perm =<< l)
  True <- isSorted =<< xs
  xs

isSorted :: Monad m => List m Int -> m Bool
isSorted Nil           = return True
isSorted (Cons mx mxs) = isSorted' mx =<< mxs

isSorted' :: Monad m => m Int -> List m Int -> m Bool
isSorted' _  Nil           = return True
isSorted' mx (Cons my mys) = do
  x <- mx
  y <- my
  if x <= y
   then isSorted' (return y) =<< mys
   else return False
