{-# LANGUAGE
     NoMonomorphismRestriction,
     MultiParamTypeClasses,
     OverlappingInstances,
     FlexibleInstances,
     FlexibleContexts
  #-}

import Control.Monad.Sharing.Lazy

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

instance Monad m => Trans m (Int,Int) (Int,Int)
 where
  trans _ = return

-- instance (Trans m a a, Trans m b b) => Trans m (m a,m b) (m a,m b)
--  where
--   trans f (a,b) = return (,) `ap` f a `ap` f b

instance Monad m => Trans m (m Int, m Int) (m Int, m Int)
 where
  trans f (a,b) = return (,) `ap` f a `ap` f b

instance (Monad m, Trans m a c, Trans m b d) => Trans m (m a,m b) (c,d)
 where
  trans f (a,b) = return (,) `ap` join (f a) `ap` join (f b)

assertEqual :: (Trans (Lazy []) a b, Eq b) => [b] -> Lazy [] a -> Bool
assertEqual res test = zipEq (evalLazy test) res
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
  do x <- share (undefined :: Lazy [] Int)
     duplicate (const (return (2::Int)) x)

dupl :: Monad m => m a -> m (List m a)
dupl x = cons x (cons x nil)

heads_bind = assertEqual [[0,0::Int],[0,1],[1,0],[1,1]] $ do
               x <- cons coin undefined
	       dupl (first (return x))

heads_share = assertEqual [[0,0::Int],[1,1]] $ do
                x <- share (cons coin undefined)
                dupl (first x)

coins :: MonadPlus m => m (List m Int)
coins = nil `mplus` cons coin coins

dup_first_coin = assertEqual [[0::Int,0],[1,1]] $ do
                   cs <- share coins
                   dupl (first cs)

-- other examples

one_coin = assertEqual [0,1::Int] coin

two_coins = assertEqual [(0,0),(0::Int,1::Int),(1,0),(1,1)] $
              return (coin :: Lazy [] Int, coin :: Lazy [] Int)

dup_coin = assertEqual [(0::Int,0::Int),(1,1)] $ dup coin

dup :: (Monad m, Sharing m, Trans m a a) => m a -> m (m a, m a)
dup a = do
  x <- share a
  return (x,x)

dupnot_coin = assertEqual [(1::Int,1::Int),(0,0)] $ dupnot coin

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
             return (x, first xs)

dup_list = assertEqual [([],[])
                       ,([0::Int],[0::Int])
                       ,([0,0],[0,0])
                       ,([0,0,0],[0,0,0])] $
             dup coins

ignore_shared = assertEqual [(0::Int,1::Int)] $ ign_pair mzero

ign_pair :: Lazy [] Int -> Lazy [] (Lazy [] Int,Lazy [] Int)
ign_pair a = do
  x <- share (a :: Lazy [] Int)
  return (const (return 0) x, const (return 1) x)

empty_rep = assertEqual [False] $ isEmpty (rep (undefined::Lazy [] Int))

nest_lazy = assertEqual [42::Int] $ do
  x <- share (cons (return 42) mzero)
  first x :: Lazy [] Int

nest_share1 = assertEqual [(0::Int,0::Int),(1,1)] $ do
  x <- share (share (return True) >> coin)
  return (x,x)

nest_share2 = assertEqual [(0::Int,0::Int),(1,1)] $ do
  x <- share (share coin >>= id)
  return (x,x)

dup_dup = assertEqual [((0::Int,0::Int),(0::Int,0::Int)),((1,1),(1,1))] $ 
            (dup (dup coin :: Lazy [] (Lazy [] Int,Lazy [] Int))
              :: Lazy [] (Lazy [] (Lazy [] Int,Lazy [] Int),
                          Lazy [] (Lazy [] Int,Lazy [] Int)))

dup_two_coins = assertEqual [((0::Int,0::Int),(0::Int,0::Int)),((0,1),(0,1))
                            ,((1,0),(1,0)),((1,1),(1,1))] $ do
  x <- share coin
  y <- share coin
  return ( return (x,y) :: Lazy [] (Lazy [] Int, Lazy [] Int)
         , return (x,y) :: Lazy [] (Lazy [] Int, Lazy [] Int))

dup_head = assertEqual [(0::Int,0::Int),(1,1)] $ heads (cons coin nil)

heads l = do
  xs <- share l
  return (first xs, first xs)

dup_head_lazy = assertEqual [(0::Int,0::Int),(1,1)] $
                  heads (cons coin undefined)