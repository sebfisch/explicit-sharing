{-# LANGUAGE NoMonomorphismRestriction #-}

-- to compile, run:
-- ghc -fglasgow-exts -hide-package monads-fd -O2 --make permsort.hs

-- $ time ./permsort 20
-- user	0m12.400s

-- time ./permsort.mcc 20
-- user	0m25.067s


import Control.Monad.Sharing.Lazy

import System ( getArgs )

main = do
  n <- liftM (read.head) getArgs
  let result = evalLazy . sort . foldr cons nil . map return $ [1..n]
  mapM_ print (result :: [[Int]])



perm :: MonadPlus m => List m a -> m (List m a)
perm Nil         = nil
perm (Cons x xs) = insert x (perm =<< xs)

insert :: MonadPlus m => m a -> m (List m a) -> m (List m a)
insert e l = cons e l
     `mplus` do
        Cons x xs <- l
        cons x (insert e xs)

sort :: (MonadPlus m, Sharing m) => m (List m Int) -> m (List m Int)
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
