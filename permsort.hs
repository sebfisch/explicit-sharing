{-# LANGUAGE NoMonomorphismRestriction #-}

-- to compile, run:
-- ghc -O2 --make permsort.hs

-- $ time ./permsort 20
-- real 0m14.049s
-- user 0m13.990s

-- time ./permsort.mcc 20
-- real 0m13.121s
-- user 0m10.900s

-- Other implementations using GHC 7.0.3

-- continuation monad with unevaluated thunks in store
-- user	0m37.073s

-- continuation monad with fewer store operations
-- user	0m29.517s


import Control.Monad.Sharing
import Data.Monadic.List

import System ( getArgs )

main = do
  n <- liftM (read.head) getArgs
  result <- resultList (sort (convert [(1::Int)..n]) >>= convert)
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
