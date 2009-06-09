% Changes w.r.t. version 0.1

The interface of the [library for explicit sharing][explicit-sharing]
has changed since the first version to incorporate an alternative
implementation and support conversion between nested monadic data and
ordinary Haskell types.


converting nested monadic data
------------------------------

The is now a type class `Convertible` defined as follows.

~~~ { .Haskell }
class Convertible m a b
 where
  convArgs :: (forall c d . Convertible m c d => c -> m d) -> a -> m b
~~~

This type class supports converting back and forth between nested
monadic data and isomorphic ordinary Haskell data. You can find
example instances for monadic lists in the [tutorial].


`Nondet` is now `Shareable`
---------------------------

The type class `Nondet` has been replaced by a generalized type class
`Shareable` that allows the monad which is used for the transformation
of arguments to be different from the monad that wraps nested
arguments. Instead of the operation

~~~ { .Haskell }
mapNondet :: (forall b . Nondet m b => m b -> m (m b)) -> a -> m a
~~~

the class `Shareable` defines the following operation.

~~~ { .Haskell }
shareArgs :: Monad n => (forall b . Shareable m b => m b -> n (m b)) -> a -> n a
~~~

This generalization allows to use a state monad when sharing arguments
even if no state monad is used to represent monadic arguments and was
necessary to implement the alternative approach to explicit sharing.


alternative implementation
--------------------------

There is an experimental version of an alternative implementation
available via the module `Control.Monad.Sharing.FirstOrder`. It
represents monadic actions as first order data types instead of
state-passing functions and, hence, supports (some) sharing across
non-determinism.

This implementation is not (yet) as efficient as the
continuation-based default implementation, so you should usually use
the original version and only resort to the new one, if your program
benefits considerably from sharing across non-determinism as provided
by the new implementation.

An example program that benefits from sharing across non-determinism
is the following:

> import Control.Monad.Sharing.FirstOrder
> import Data.Monadic.List
>
> import Prelude hiding ( last )
>
> listOf :: (MonadPlus m, Sharing m, Shareable m a) => m a -> m (List m a)
> listOf x = nil
>    `mplus` do y <- share x
>               cons y (listOf y)

The function `listOf` non-deterministically yields a list of arbitrary
length that only contains the given element (which may itself be
non-deterministic).

If we apply `listOf` to an expensive computation this computation
would be reexecuted on every non-deterministic branch on which it is
demanded. This behaviour resembles the behaviour of the Curry systems
[PAKCS] and [MCC]. If we use the alternative implementation of
explicit sharing the expensive computation is only executed once if
its result is a first-order value which is the case in this example,
if the underlying monad is, e.g., the list monad. This behaviour
resembles the behaviour of the Curry system [KiCS].

We can observe the difference by applying the `last` function to the
result of `listOf`.

> last :: (MonadPlus m, Sharing m, Shareable m a) => List m a -> m a
> last Nil           = mzero
> last (Cons mx mxs) = do mys <- share mxs
>                         ys <- mys
>                         case ys of
>                           Nil         -> mx 
>                           Cons mz mzs -> last =<< mys

As expensive computation we use the recursive implementation of the
Fibonacci function in monadic style.

> fib :: Monad m => Int -> m Int
> fib n | n < 2     = return n
>       | otherwise = liftM2 (+) (fib (n-2)) (fib (n-1))

If we now query some solutions of the following computation

~~~
 > mapM_ print $ take 5 (evalLazy (last =<< listOf (fib 30)) :: [Int])
 832040
 832040
 832040
 832040
 832040
~~~

The solutions are printed slowly one after the other if we use the
default implementation of explicit sharing and almost simultaneously
if we use the alternative version. The default version recomputes `fib
30` the alternative version does not.

[explicit-sharing]: index.html
[tutorial]: tutorial.html#nested-monadic-data

[PAKCS]: http://www.informatik.uni-kiel.de/~pakcs/
[MCC]: http://danae.uni-muenster.de/~lux/curry/
[KiCS]: http://www.informatik.uni-kiel.de/prog/mitarbeiter/bernd-brassel/projects/
