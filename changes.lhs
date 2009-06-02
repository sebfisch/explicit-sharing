% Comparing version 0.5 with version 0.1

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

This type class supports to convert back and forth between nested
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
state-passing functions and avoids some deficiencies of the original
implementation. It avoids duplicate sharing when sharing an already
shared value and supports (some) sharing across non-determinism.

This implementation is not (yet) as efficient as the
continuation-based default implementation, so you should usually use
the original version and only resort to the new one, if your program
suffers from duplicate sharing or benefits considerably from sharing
across non-determinism as provided by the new implementation.


[explicit-sharing]: index.html
[tutorial]: tutorial.html#nested-monadic-data
