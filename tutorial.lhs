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
it is executed.

Here is a variant of the above action with explicit sharing.

> dup_shared_get :: (MonadIO m, Sharing m) => m String
> dup_shared_get = do get <- share (liftIO getChar)
>                     a <- get
>                     b <- get
>                     c <- get
>                     return [a,b,c]

Instead of the built-in `let` construct this action uses the `share`
combinator to share the (now lifted) `getChar` action. We need to lift
the `getChar` action because we cannot execute `dup_shaed_get`
directly in the IO monad which does not support the `share`
combinator.

We can run this action using the operation `evalLazy`.

    *Main> evalLazy dup_shared_get :: IO String
    xyz
    "xxx"

This time, the result is `"xxx"` rather than `"xyz"`. The shared `get`
action only performs the effects of `getChar` once and yields the
result of the first execution at each duplicated occurrence. The
remaining characters (`"yz"` in the example call above) are never
read.

This behaviour may seem as if `share` simply executes the given action
and returns an action that yields the obtained result. It does
not. The action given to `share` is only executed if the action that
`share` returns is, i.e., `share` is lazy:

> ignore_shared :: (MonadIO m, Sharing m) => m String
> ignore_shared =
>  do action <- share (liftIO (error "don't touch me!" :: IO String))
>     return "didn't touch you."

Running `ignore_shared` yields `"didn't touch you."` without touching
the `error` call:

    *Main> evalLazy ignore_shared :: IO String
    "didn't touch you."


nested monadic data
-------------------

Let's return to the type class `Trans` that specifies what data can be
shared. The `share` combinator is not only applicable to predefined
Haskell types like `String` but also to user-defined types that
contain nested monadic components. For example, the library for
explicit sharing defines a type for lists with monadic heads and
tails.

~~~ { .Haskell }
data List m a = Nil | Cons (m a) (m (List m a))
~~~

In order to be able to use `share` with values of this type, we need
an instance of `Trans` (which is also provided out of the box).

~~~ { .Haskell }
instance (Monad m, Trans m a b) => Trans m (List m a) (List m b)
 where
  trans _ Nil         = return Nil
  trans f (Cons x xs) = return Cons `ap` f x `ap` f xs
~~~

The type class `Trans` defines one operation `trans` to generically
traverse nested monadic types. As you can see in the above instance
declaration, the given function `f` is applied to every monadic child
of a compound value and the results are combined using the matched
constructor.

The `share` combinator uses this functionality to share nested monadic
components of data recursively. Here is an example that shares an
infinite `List` of `getChar` operations and returns a list of some of
them that are duplicated.

> share_list :: (MonadIO m, Sharing m) => m (List m Char)
> share_list = do gets <- share getChars
>                 Cons a as <- gets
>                 Cons b bs <- as
>                 Cons c cs <- gets
>                 cons a (cons b (cons c (cons a (cons b (cons c nil)))))
>  where
>   getChars = cons (liftIO getChar) getChars

The functions `nil` and `cons` are helper functions to construct
nested monadic lists. This example is definitely contrived but it
helps to make a point: the infine list `gets` is shared and hence all
contained actions are shared too. Hence, `a` and `as` are the same as
`c` and `cs` and all actions yield the same results when
duplicated. The result of `share_list` is a list with six elements
that will read two characters from the standard input when executed.

How can we observe this list? The `List` type comes with another
instance of `Trans` that allows `evalLazy` to convert it to ordinary
Haskell lists.

~~~ { .Haskell }
instance (Monad m, Trans m a b) => Trans m (List m a) [b]
 where
  trans _ Nil         = return []
  trans f (Cons x xs) = return (:) `ap` join (f x) `ap` join (f xs)
~~~

This instance lifts all nested monadic effects to the top-level such
that a corresponding transformation yields an ordinary list without
any nested effects.

Now we can observe that indeed only two characters are read:

    *Main> evalLazy share_list :: IO String
    xyz
    "xyxxyx"

In order to convert in the other direction, there is yet another
instance of `Trans` for `List`s:

~~~ { .Haskell }
instance (Monad m, Trans m a b) => Trans m [a] (List m b)
 where
  trans _ []     = return Nil
  trans f (x:xs) = return Cons `ap` f (return x) `ap` f (return xs)
~~~

Thanks to this instance, we could use the function

~~~ { .Haskell }
eval :: (Monad m, Trans m a b) => a -> m b
~~~

to convert a list of type `[Char]` into one of type `m (List m Char)`.

outlook
-------

Now, we have seen it all: the `share` combinator from the type class
`Sharing` which implements explicit sharing of monadic effects such
that monadic actions can be duplicated without duplicating their
effects and three different instances of the type class `Trans` which
allow nested monadic data to be shared and converted back and forth to
ordinary data respectively. But what is this good for?

A monadic effect whose interaction whith sharing is particularly
interesting ins non-determinism. By combining the features for
non-determinism provided by the `MonadPlus` type class with explicit
sharing provided by the `Sharing` class we can implement lazy
functional-logic programming as advocated, e.g., by the [Curry]
language in pure Haskell.

[How to translate Curry programs to Haskell using explicit
sharing][FLP] is worth a different tutorial.

[lhs]: tutorial.lhs
[explicit-sharing]: index.html
[Typeclassopedia]: http://www.haskell.org/sitewiki/images/8/85/TMR-Issue13.pdf
[Curry]: http://curry-language.org
[FLP]: flp.html
