% how to use explicit sharing

<div class="nodisplay">

> {-# LANGUAGE PackageImports #-}
> import "mtl" Control.Monad.Trans

</div>

This [literate program][lhs] explains how to use the
[explicit-sharing] package which provides the following module.

> import Control.Monad.Sharing.Lazy

The interface of this module basically consists of two type
classes. Instances of `Sharing` support a combinator `share` for
explicit sharing and are usually monads. If you fell like you don't
know enough about monads you may want to look at the
[Typeclassopedia].

~~~ { .Haskell }
class Sharing m
 where
  share :: Trans m a a => m a -> m (m a)
~~~

The type class `Trans` provides an interface for generic traversals of
nested monadic data types. We will come back to them later. For now,
just think of `Trans` to specify data that can be shared explicitly.

The function `share` takes a monadic action of type `m a` and yields
an action in the same monad which yields a monadic action of the same
type as the argument. The idea is that `share` yields something like
the original action which can be duplicated without duplicating the
effects.

sharing IO effects
------------------

Let's look at an example. First consider a program that does not use
explicit sharing.

> dup_get :: IO String
> dup_get = do let get = getChar
>              a <- get
>              b <- get
>              c <- get
>              return [a,b,c]

This action renames the predefined action `getChar` and calls it three
times returning a list of the results of the three calls:

    *Main> dup_get
    xyz
    "xyz"

Each occurrence of `get` reads a different character from standard
input, i.e., has its own independent input effect and all occurrences
of `get` can have different results. Haskells `let` construct shares
the IO action `getChar` which can still be executed independently more
than once.

The `share` combinator provides a different kind of sharing. An IO
action that is shared explicitly using `share` can still be executed
multiple times but its effects are only performed on first execution
and an explicitly shared action will return the same result whenever
it is executed. Moreover, if the result of explicitly sharing an
action is never executed then the action provided to `share` is also
never executed.

Here is a variant of the above action with explicit sharing.

> dup_shared_get :: (MonadIO m, Sharing m) => m String
> dup_shared_get = do get <- share (liftIO getChar)
>                     a <- get
>                     b <- get
>                     c <- get
>                     return [a,b,c]

[lhs]: tutorial.lhs
[explicit-sharing]: index.htm
[Typeclassopedia]: http://www.haskell.org/sitewiki/images/8/85/TMR-Issue13.pdf

