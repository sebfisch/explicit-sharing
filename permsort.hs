{-# LANGUAGE NoMonomorphismRestriction #-}

-- to compile, run:
-- ghc -O2 --make permsort.hs

-- $ time ./permsort 20
-- user	0m8.909s

-- time ./permsort.mcc 20
-- user	0m25.067s

-- Comparing different implementations

-- standard StateT with unevaluated thunks in store
-- user	1m41.645s

-- continition monad with unevaluated thunks in store
-- user	0m37.073s

-- continuation monad with fewer store operations
-- user	0m29.517s

-- additionally with hand optimized memo function
-- user	0m8.909s


import Control.Monad.Sharing
import Data.Monadic.List

import System ( getArgs )

main = do
  n <- liftM (read.head) getArgs
  let result = evalLazy . sort . convert $ [(1::Int)..n]
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
