% lazy FLP in pure Haskell

This [literate program][lhs] explains how to express programs written
in the lazy functional-logic programming language [Curry] in pure
Haskell using the [explicit-sharing] package. If you have not done so
already you may want to read the [tutorial] introduction to this
package first.


lazy permutation sort in Curry
------------------------------

Lazy functional-logic programming is tailor made for expressing demand
driven search algorithms concisely. For example, we can express
sorting as a demand driven search problem in Curry as follows.

~~~ { .Haskell }
sort :: [Int] -> [Int]
sort l | isSorted p = p where p = permute l
~~~

Sorting is usually not expressed as a search problem but let's not
care for a moment and pretend we don't know better. Even if we don't
know anything about sorting we can implement it using the above
algorithm which reads: *to sort a list, permute it such that it is
sorted*.

Note that we have successfully split the task of sorting into two
independent tasks that are easier to implement: how to permute a list
and how to check whether a list is sorted.

Lazy functional-logic programming provides the machinery to implement
these tasks separately, yet execute them in a trickily interleaved
way, such that the generating part (which permutes lists) produces
values only to the extend demanded by the testing part (which checks
whether a list is sorted).

We can implement the tester and generator for the above sorting
algorithm in Curry as follows.

~~~ { .Haskell }
isSorted :: [Int] -> Bool
isSorted []       = True
isSorted [_]      = True
isSorted (x:y:zs) = x <= y && isSorted (y:zs)

permute :: [a] -> [a]
permute []     = []
permute (x:xs) = insert x (permute xs)

insert :: a -> [a] -> [a]
insert x xs     = x : xs
insert x (y:ys) = y : insert x ys
~~~

In Curry the rule of a function are not matched from top to bottom
(picking the first) but tried non-deterministically. Hence, `insert`
can yield different non-deterministic results when applied to a
non-empty list and so can `permute`.

A more interesting observation, however, is the following: the
predicate `isSorted` yields `False` if it sees two adjacent elements
that are out of order *without demanding subsequent elements of the
list*. The function `isSorted` can reject a permutation based on the
first two elements and if it does the permutations of the remaining
elements need not be computed. This contributes greately to the
efficiency of the presented sorting algorithm (which has nevertheless
exponential run time).


lazy permutation sort in Haskell
--------------------------------

In order to model this algorithm in pure Haskell we can use the
`MonadPlus` type class to express non-determinism. However, in order
to maintain the laziness of the algorithm we need a data type for
non-deterministic lists that can be computed on demand. More
specifically, we need to be able to yield the first element of a list
whose tail is yet to be computed non-deterministically.

The `List` data type provided by this module

> import Control.Monad.Sharing.Lazy

provides lists with nested monadic components which is exactly what we
need. All we have to do is translate the Curry functions above (which
use implicit non-determinism and implicit sharing) into equivalent
Haskell functions that use explicit non-determinism and explicit
sharing. Here we go:

 sort :: (MonadPlus m, Sharing m) => m (List m a)


[lhs]: flp.lhs
[Curry]: http://curry-language.org
[explicit-sharing]: index.htm
[tutorial]: tutorial.html
