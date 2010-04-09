{-# LANGUAGE TemplateHaskell
  , KindSignatures
  , MultiParamTypeClasses
  , FlexibleInstances #-}

-- demonstrates store blowup due to repeated sharing

import System ( getArgs )

import Data.Monadic.Derive

import Control.Monad.Sharing

data Bin = Tip | Bin Bin Bin -- ; $(derive monadic ''Bin)

-- exponential store blowup with derived monadic data:
--
-- # for n in `gseq 1 10`; do ./repeated-sharing $n; done
-- used refs: 1
-- 2
-- used refs: 4
-- 4
-- used refs: 11
-- 8
-- used refs: 26
-- 16
-- used refs: 57
-- 32
-- used refs: 120
-- 64
-- used refs: 247
-- 128
-- used refs: 502
-- 256
-- used refs: 1013
-- 512
-- used refs: 2036
-- 1024

main = print . pow2 . read . head =<< getArgs

pow2 :: Int -> Int
pow2 = head . evalLazy . size . complete

complete :: (Monad m, Sharing m) => Int -> m (MBin m)
complete 0     = mTip
complete (n+1) = do t <- share (complete n)
                    mBin t t

size :: Monad m => m (MBin m) -> m Int
size t = matchMBin t (return 1)
                     (\l r -> do m <- size l
                                 n <- size r
                                 return (m+n))

-- custom monadic data with 'arguments are already shared' flag:

data MBin m = MTip | MBin Bool (m (MBin m)) (m (MBin m))

mTip :: Monad m => m (MBin m)
mTip = return MTip

mBin :: Monad m => m (MBin m) -> m (MBin m) -> m (MBin m)
mBin l r = return (MBin False l r)

matchMBin :: Monad m => m (MBin m)
                     -> m a -> (m (MBin m) -> m (MBin m) -> m a)
                     -> m a
matchMBin mbin tip bin = do t <- mbin
                            case t of
                              MTip       -> tip
                              MBin _ l r -> bin l r

instance Monad m => Shareable m (MBin m) where
  shareArgs _ MTip = return MTip
  shareArgs f (MBin isShared l r)
    | isShared  = return (MBin isShared l r)
    | otherwise = do x <- f l; y <- f r; return (MBin True x y)

instance Monad m => Convertible m Bin (MBin m) where
  convert Tip       = mTip
  convert (Bin l r) = mBin (convert l) (convert r)

instance Monad m => Convertible m (MBin m) Bin where
  convert MTip         = return Tip
  convert (MBin _ l r) = do x <- l >>= convert
                            y <- r >>= convert
                            return (Bin x y)

-- # for n in `gseq 1 10`; do ./repeated-sharing $n; done
-- used refs: 1
-- 2
-- used refs: 4
-- 4
-- used refs: 7
-- 8
-- used refs: 10
-- 16
-- used refs: 13
-- 32
-- used refs: 16
-- 64
-- used refs: 19
-- 128
-- used refs: 22
-- 256
-- used refs: 25
-- 512
-- used refs: 28
-- 1024
