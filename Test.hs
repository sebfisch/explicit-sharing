{-# LANGUAGE
     NoMonomorphismRestriction,
     MultiParamTypeClasses,
     FlexibleInstances,
     FlexibleContexts
  #-}

import Control.Monad.Sharing.Lazy

import List

main = do
  putStr "failing tests: "
  print . map fst . filter (not . snd) . zip [1..] $ tests
 where
  tests = [ dup_coin_let, dup_coin_bind, dup_coin_share
          , lazy_share, heads_bind, heads_share, dup_first_coin
          , one_coin, two_coins, dup_coin, dupnot_coin
          , first_rep, first_rep', rep_coin, rep_coin'
          , dup_list, ignore_shared, empty_rep, empty_rep'
          , nest_lazy, nest_share1, nest_share2
          , dup_dup, dup_two_coins, dup_head, dup_head_lazy
          ]

instance MonadPlus m => Nondet m Bool
 where
  mapNondet _ = return

instance MonadPlus m => Nondet m (Int,Int)
 where
  mapNondet _ = return

instance (Nondet m a, Nondet m b) => Nondet m (m a,m b)
 where
  mapNondet f (a,b) = return (,) `ap` f a `ap` f b

fromList _ Nil         = return []
fromList f (Cons x xs) = do y <- x >>= f; ys <- xs >>= fromList f; return (y:ys)

fromPair l r (a,b) = do x <- a >>= l; y <- b >>= r; return (x,y)

assertEqual :: (Nondet (Lazy []) a, Eq b)
            => (a -> Lazy [] b) -> [b] -> Lazy [] a -> Bool
assertEqual f res test = zipEq (runLazy (test >>= eval >>= f)) res
 where
  zipEq [] [] = True
  zipEq [] _  = False
  zipEq (_:_) [] = True
  zipEq (x:xs) (y:ys) = (x==y) && zipEq xs ys

coin :: MonadPlus m => m Int
coin = return 0 `mplus` return 1

ilist = fromList return
illist = fromList ilist
ipair = fromPair return return
pilist = fromPair ilist ilist
ippair = fromPair ipair ipair

-- examples from paper

duplicate :: Monad m => m a -> m (a,a)
duplicate a = do x <- a; y <- a; return (x,y)

dup_coin_let = assertEqual return [(0,0),(0,1),(1,0),(1,1)] $ 
                 let x = coin in duplicate x

dup_coin_bind  = assertEqual return [(0,0),(1,1)] $ do
                   x <- coin
                   duplicate (return x)

dup_coin_share = assertEqual return [(0,0),(1,1)] $ do
                   x <- share coin
                   duplicate x

-- strict_bind = -- diverges intentionally
--   do x <- undefined :: Lazy [] Int
--      duplicate (const (return 2) (return x))

lazy_share = assertEqual return [(2::Int,2::Int)] $
  do x <- share (undefined :: Lazy [] Int)
     duplicate (const (return 2) x)

dupl :: Monad m => m a -> m (List m a)
dupl x = cons x (cons x nil)

heads_bind = assertEqual ilist [[0,0],[0,1],[1,0],[1,1]] $ do
               x <- cons coin undefined
	       dupl (first (return x))

heads_share = assertEqual ilist [[0,0],[1,1]] $ do
                x <- share (cons coin undefined)
                dupl (first x)

coins :: MonadPlus m => m (List m Int)
coins = nil `mplus` cons coin coins

dup_first_coin = assertEqual ilist [[0,0],[1,1]] $ do
                   cs <- share coins
                   dupl (first cs)

-- other examples

one_coin = assertEqual return [0,1] coin

two_coins = assertEqual ipair [(0,0),(0,1),(1,0),(1,1)] $ return (coin,coin)

dup_coin = assertEqual ipair [(0,0),(1,1)] $ dup coin

dup a = do
  x <- share a
  return (x,x)

dupnot_coin = assertEqual ipair [(1,1),(0,0)] $ dupnot coin

dupnot a = do
  x <- share a
  return (liftM ((-)1) x, liftM ((-)1) x)

first_rep = assertEqual return [42::Int] $ first (first (rep (rep (return 42))))

rep a = do
  x <- share a
  cons x (rep x)

first_rep' = assertEqual return [42::Int] $
               first (first (rep' (rep' (return 42))))

rep' a = do
  xs <- shareRec (\l -> cons a l)
  xs

rep_coin = assertEqual ipair [(0::Int,0::Int),(1,1)] $
             rep coin >>= match mzero (\x xs -> return (x, first xs))

-- recursive bindings do not work as expected:
rep_coin' = assertEqual ipair [(0::Int,0::Int),(0,1),(1,0),(1,1)] $
              rep' coin >>= match mzero (\x xs -> return (x, first xs))

dup_list = assertEqual pilist [([],[])
                              ,([0],[0])
                              ,([0,0],[0,0])
                              ,([0,0,0],[0,0,0])] $
             dup coins

ignore_shared = assertEqual ipair [(0,1)] $ ign_pair mzero

ign_pair :: Sharing m => m Int -> m (m Int,m Int)
ign_pair a = do
  x <- share a
  return (const (return 0) x, const (return 1) x)

empty_rep = assertEqual return [False] $ empty (rep (undefined::Lazy [] Int))

empty_rep' = assertEqual return [False] $ empty (rep' (undefined::Lazy [] Int))

nest_lazy = assertEqual return [42::Int] $ do
  x <- share (cons (return 42) mzero)
  first x

nest_share1 = assertEqual ipair [(0,0),(1,1)] $ do
  x <- share (share (return True) >> coin)
  return (x,x)

nest_share2 = assertEqual ipair [(0,0),(1,1)] $ do
  x <- share (share coin >>= id)
  return (x,x)

dup_dup = assertEqual ippair [((0,0),(0,0)),((1,1),(1,1))] $ dup (dup coin)

dup_two_coins = assertEqual ippair [((0,0),(0,0)),((0,1),(0,1))
                                   ,((1,0),(1,0)),((1,1),(1,1))] $ do
  x <- share coin
  y <- share coin
  return (return (x,y), return (x,y))

dup_head = assertEqual ipair [(0,0),(1,1)] $ heads (cons coin nil)

heads l = do
  xs <- share l
  return (first xs, first xs)

dup_head_lazy = assertEqual ipair [(0,0),(1,1)] $ heads (cons coin undefined)