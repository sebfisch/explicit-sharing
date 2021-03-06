{-# LANGUAGE
     NoMonomorphismRestriction,
     MultiParamTypeClasses,
     OverlappingInstances,
     IncoherentInstances,
     FlexibleInstances,
     FlexibleContexts,
     RankNTypes
  #-}

import Control.Monad.Sharing
import Data.Monadic.List

main = do
  putStr "failing tests: "
  print . map fst . filter (not . snd) . zip [1..] $ tests
 where
  tests = [ dup_coin_let, dup_coin_bind, dup_coin_share
          , lazy_share, heads_bind, heads_share, dup_first_coin
          , one_coin, two_coins, dup_coin, dupnot_coin
          , first_rep, rep_coin
          , dup_list, ignore_shared, empty_rep
          , nest_lazy, nest_share1, nest_share2
          , dup_dup, dup_two_coins, dup_head, dup_head_lazy
          ]

instance Monad m => Shareable m (Int,Int)
 where
  shareArgs _ = return

instance Monad m => Shareable m ([Int],[Int])
 where
  shareArgs _ = return

instance Monad m => Shareable m ((Int,Int),(Int,Int))
 where
  shareArgs _ = return

instance Monad m => Convertible m (Int,Int) (Int,Int)
 where
  convert = return

instance (Monad m, Shareable m a) => Shareable m (m a, m a)
 where
  shareArgs f (x,y) = return (,) `ap` f x `ap` f y

instance (Monad m, Convertible m a b) => Convertible m (m a, m a) (b, b)
 where
  convert (x,y) = return (,) `ap` (x >>= convert) `ap` (y >>= convert)

-- assertEqual :: (Shareable (Lazy []) a, Convertible (Lazy []) a b, Eq b)
--             => [b] -> Lazy [] a -> Bool
assertEqual :: Eq a => [a] -> (forall s . Sharing s => s a) -> Bool
assertEqual res test = zipEq (unsafeResults test) res
 where
  zipEq [] [] = True
  zipEq [] _  = False
  zipEq (_:_) [] = True
  zipEq (x:xs) (y:ys) = (x==y) && zipEq xs ys

coin :: MonadPlus m => m Int
coin = return 0 `mplus` return 1

-- examples from paper

duplicate :: Monad m => m a -> m (a,a)
duplicate a = do x <- a; y <- a; return (x,y)

dup_coin_let = assertEqual [(0,0)::(Int,Int),(0,1),(1,0),(1,1)] $ 
                 let x = coin in duplicate x

dup_coin_bind  = assertEqual [(0,0)::(Int,Int),(1,1)] $ do
                   x <- coin
                   duplicate (return x)

dup_coin_share = assertEqual [(0,0)::(Int,Int),(1,1)] $ do
                   x <- share coin
                   duplicate x

-- strict_bind = -- diverges intentionally
--   do x <- undefined :: Lazy [] Int
--      duplicate (const (return 2) (return x))

lazy_share = assertEqual [(2::Int,2::Int)] $
  do x <- share undef
     duplicate (const (return (2::Int)) x)

undef :: Monad m => m Int
undef = undefined

dupl :: Monad m => m a -> m (List m a)
dupl x = cons x (cons x nil)

heads_bind = assertEqual [[0,0::Int],[0,1],[1,0],[1,1]] $ do
               x <- cons coin undefined
	       dupl (first (return x)) >>= convert

heads_share = assertEqual [[0,0::Int],[1,1]] $ do
                x <- share (cons coin undefined)
                dupl (first x) >>= convert

coins :: MonadPlus m => m (List m Int)
coins = nil `mplus` cons coin coins

dup_first_coin = assertEqual [[0::Int,0],[1,1]] $ do
                   cs <- share coins
                   dupl (first cs) >>= convert

-- other examples

one_coin = assertEqual [0,1::Int] coin

two_coins = assertEqual [(0,0),(0::Int,1::Int),(1,0),(1,1)] $
              return (coin, coin) >>= convertPair
  where
    convertPair (a,b) = do x <- a; y <- b; return (x,y)

dup_coin = assertEqual [(0::Int,0::Int),(1,1)] $ dup coin >>= convert

dup :: (Monad m, Sharing m, Shareable m a) => m a -> m (m a, m a)
dup a = do
  x <- share a
  return (x,x)

dupnot_coin = assertEqual [(1::Int,1::Int),(0,0)] $ dupnot coin >>= convert

dupnot a = do
  x <- share a
  return (liftM ((-)1) x, liftM ((-)1) x)

first_rep = assertEqual [42::Int] $ 
              first (first (rep (rep (return (42::Int)))))

rep a = do
  x <- share a
  cons x (rep x)

rep_coin = assertEqual [(0::Int,0::Int),(1,1)] $ do
             Cons x xs <- rep coin
             convert (x, first xs)

dup_list = assertEqual [([],[])
                       ,([0::Int],[0::Int])
                       ,([0,0],[0,0])
                       ,([0,0,0],[0,0,0])] $
             dup coins >>= convert

ignore_shared = assertEqual [(0::Int,1::Int)] $ ign_pair mzero >>= convertPair
  where
    convertPair (a,b) = do x <- a; y <- b; return (x,y)

ign_pair :: Sharing m => m Int -> m (m Int, m Int)
ign_pair a = do
  x <- share a
  return (const (return 0) x, const (return 1) x)

empty_rep = assertEqual [False] $ isEmpty (rep undef)

nest_lazy = assertEqual [42::Int] $ do
  x <- share (cons (return 42) mzero)
  first x

nest_share1 = assertEqual [(0::Int,0::Int),(1,1)] $ do
  x <- share (share (return True) >> coin)
  convert (x,x)

nest_share2 = assertEqual [(0::Int,0::Int),(1,1)] $ do
  x <- share (share coin >>= id)
  convert (x,x)

dup_dup = assertEqual [((0::Int,0::Int),(0::Int,0::Int)),((1,1),(1,1))] $
            (dup (dup coin)) >>= convert

dup_two_coins = assertEqual [((0::Int,0::Int),(0::Int,0::Int)),((0,1),(0,1))
                            ,((1,0),(1,0)),((1,1),(1,1))] $ do
  x <- share coin
  y <- share coin
  convertPair (return (x,y),return (x,y))
 where
  convertPair (a,b) = do x <- a >>= convert; y <- b >>= convert; return (x,y)

dup_head = assertEqual [(0::Int,0::Int),(1,1)] $
             heads (cons coin nil) >>= convert

heads l = do
  xs <- share l
  return (first xs, first xs)

dup_head_lazy = assertEqual [(0::Int,0::Int),(1,1)] $
                  heads (cons coin undefined) >>= convert