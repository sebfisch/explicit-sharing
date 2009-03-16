{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- compile with `ghc -O2 --make shared-tree`

import Control.Monad.Sharing.Lazy

import System ( getArgs )

main = main_mon

main_fun = do n <- liftM (read.head) getArgs
              print . leafCount . complete $ n

main_mon = do n <- liftM (read.head) getArgs
              print . head . evalLazy $ leafCount' =<< complete' n

data Tree = Leaf | Fork Tree Tree

leafCount :: Tree -> Int
leafCount Leaf       = 1
leafCount (Fork l r) = leafCount l + leafCount r

complete :: Int -> Tree
complete n | n > 0     = let t = complete (n-1) in Fork t t
           | otherwise = Leaf

data MTree m = MLeaf | MFork (m (MTree m)) (m (MTree m))

instance MonadPlus m => Nondet m (MTree m)
 where mapNondet f (MFork l r) = return MFork `ap` f l `ap` f r
       mapNondet _ t           = return t

instance MonadPlus m => Nondet m Int
 where mapNondet _ n = return n

leafCount' :: Monad m => MTree m -> m Int
leafCount' MLeaf       = return 1
leafCount' (MFork l r) = liftM2 (+) (leafCount' =<< l) (leafCount' =<< r)

complete' :: Sharing m => Int -> m (MTree m)
complete' n | n > 0     = do t <- share (complete' (n-1))
                             return (MFork t t)
            | otherwise = return MLeaf

-- $ time ./shared-tree.fun 20
-- user	0m0.031s
-- $ time ./shared-tree.fun 21
-- user	0m0.054s
-- $ time ./shared-tree.fun 22
-- user	0m0.097s
-- $ time ./shared-tree.fun 23
-- user	0m0.191s
-- $ time ./shared-tree.fun 24
-- user	0m0.384s

-- $ time ./shared-tree.mcc 20
-- user	0m0.179s
-- $ time ./shared-tree.mcc 21
-- user	0m0.362s
-- $ time ./shared-tree.mcc 22
-- user	0m0.711s
-- $ time ./shared-tree.mcc 23
-- user	0m1.431s
-- $ time ./shared-tree.mcc 24
-- user	0m2.849s

-- $ time ./shared-tree.fun 20
-- user	0m0.033s
-- $ time ./shared-tree.mon 20
-- user	0m0.449s
-- $ time ./shared-tree.mon 21
-- user	0m0.915s
-- $ time ./shared-tree.mon 22
-- user	0m1.781s
-- $ time ./shared-tree.mon 23
-- user	0m3.556s
-- $ time ./shared-tree.mon 24
-- user	0m7.147s

