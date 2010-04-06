{-# LANGUAGE NoMonomorphismRestriction #-}

-- to compile, run:
-- ghc -O2 --make last

-- $ time ./last 1000000 +RTS -H1000M -K20M
-- True
-- user	0m1.154s

-- $ time ./last 10000000 +RTS -H1000M -K20M
-- True
-- user	0m9.263s

-- $ time ./last.mcc 1000000 +RTS -h1000m -k20m
-- 1000000
-- user	0m6.370s

-- $ time ./last.mcc 10000000 +RTS -h2000m -k50m
-- Not enough free memory after garbage collection

import Control.Monad.Sharing
import Data.Monadic.List
import System ( getArgs )

import Prelude hiding ( last )

main =
 do n <- liftM (read.head) getArgs
    let result = evalLazy . last . convert $ replicate n True
    mapM_ print (result :: [Bool])

last :: (MonadPlus m, Sharing m) => m (List m Bool) -> m Bool
last l = do x <- share freeBool
            l =:= append freeBoolList (cons x nil)
            x

append :: Monad m => m (List m a) -> m (List m a) -> m (List m a)
append mxs ys = do xs <- mxs; appendLists xs ys

appendLists :: Monad m => List m a -> m (List m a) -> m (List m a)
appendLists Nil         ys = ys
appendLists (Cons x xs) ys = cons x (append xs ys)

freeBool :: MonadPlus m => m Bool
freeBool = return False `mplus` return True

freeBoolList :: MonadPlus m => m (List m Bool)
freeBoolList = nil `mplus` cons freeBool freeBoolList

(=:=) :: MonadPlus m => m (List m Bool) -> m (List m Bool) -> m ()
mxs =:= mys = do xs <- mxs; ys <- mys; eqBoolList xs ys

eqBoolList :: MonadPlus m => List m Bool -> List m Bool -> m ()
eqBoolList Nil         Nil         = return ()
eqBoolList (Cons x xs) (Cons y ys) = do True <- liftM2 (==) x y
                                        xs =:= ys
eqBoolList _           _           = mzero
