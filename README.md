# fcf-containers [![Hackage](https://img.shields.io/hackage/v/fcf-containers.svg)](https://hackage.haskell.org/package/fcf-containers) [![Build Status](https://travis-ci.org/gspia/fcf-containers.svg)](https://travis-ci.org/gspia/fcf-containers)

Fcf-containers mimicks the containers package but for type-level computations. 
That is, we provide e.g. trees and maps. In addition to that, this package 
contains some other type-level computation utilities. 

These methods are based on the idea given in the
[first-class-families](https://github.com/Lysxia/first-class-families) -package,
or Fcf shortly. Fcf is the main dependency of fcf-containers. As some of the
methods fit badly under the name "fcf-containers", they might end up into 
the Fcf or some other package to be created. So stay tuned, be patient, check 
the [TODO.md](https://github.com/gspia/fcf-containers/blob/master/TODO.md) 
and send those PR's :)


Motivation for calculating things on type-level or on compile-time 
include

- increase the safety measures of runtime methods,
- pre-calculate complex things once on compile time and not every time the
  executable is run, 
- provide users a way to choose between different algorithms for solving
  a problem based on problem instance properties (e.g. local vs network,
  or small vs large) known in advance.
 
Why fcf-like? The kind of signatures used for functions might be easier to 
read for some people and the ability to apply partially a function is nice 
tool to have. The techniques that allows this are defunctionalization, 
encoding the functions with empty data types and the use of open type family 
to Eval the constructed expressions. 
 
If you have other motivations, please do let us know! 

Note: some of the claims on the items in the above list are such that I 
believe/hope but really don't know at the moment nor do I know how check them. 
E.g. the matter of compile time vs run time. Yes, types are erased at compile 
time but do they still leave something into executables: simple check by 
comparing outputs of the orbit example and another program that has one method 
to print integer 42 and main, reveals that sizes are almost the same, but not 
exactly.


There are lot of open interesting questions. See 
[TODO.md](https://github.com/gspia/fcf-containers/blob/master/TODO.md) file. E.g. how combine 
these techniques with singletons-lib and related techniques. 



## Installation and building 

First, get the repo with `git clone` and `cd` into the directory. 

```
nix-shell 
cabal build 
cabal test 
```

The doc-tests both document and work as main testing mechanism for this lib. 

If you don't use nix, `cabal install fcf-containers` should be enough. This
package has almost as good number of dependencies as the first-class-families.


## Example

The  
[test directory](https://github.com/gspia/fcf-containers/blob/master/test)
contains a lot of useful examples.

See [Orbits.hs](https://github.com/gspia/fcf-containers/blob/master/examples/Orbits.hs). 
It shows how to solve a real problem,
what PRAGMAs are probably needed etc.

```
cabal run orbits 
```

There is also another example that show how to use MapC, see
[Haiku.hs](https://github.com/gspia/fcf-containers/blob/master/examples/Haiku.hs)

```
cabal run haiku 
```

Please, do take a look of the notes made for the Helsinki Haskell meetup
on 11th January 2023
[notes](https://github.com/gspia/fcf-containers/blob/master/examples/20230111_hhslides.md)
and the associated 
[code examples](https://github.com/gspia/fcf-containers/blob/master/examples/20230111_hhmeetup.hs).



## Random Notes

### Partiality and anonymous functions

In the end, everything has to be total. We just post-pone the totality checking
with defunctionalization in a way by trying to evaluate our functions as late
as possible with the `Eval` function. 

We don't have lambdas, but if you can write the helper function in point-free
form, it might can be used directly without any global function definition.
Remember, that `(<=<)` corresponds to term-level `(.)` and `(=<<)` to 
term-level function application `($)`. See also Maguire's book 
(Thinking with Types).


### Conflicting family instance declarations

Transforming term-level Haskell code is relatively straigthforward. Often, 
local definitions in `where` and anonymous functions will be turned into 
separate helper functions. 

Occasionally, the pattern matching is not quite enough. Please, consider

```
isPrefixOf              :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf [] _         =  True
isPrefixOf _  []        =  False
isPrefixOf (x:xs) (y:ys)=  x == y && isPrefixOf xs ys
```

We could try to define it as 
```
data IsPrefixOf :: [a] -> [a] -> Exp Bool
type instance Eval (IsPrefixOf '[] _) = 'True
type instance Eval (IsPrefixOf _ '[]) = 'False
type instance Eval (IsPrefixOf (x ': xs) (y ': ys)) =
         Eval ((Eval (TyEq x y)) && Eval (IsPrefixOf xs ys))
```

But ghc does not like this definition: the first two type instances are
conflicting together. Instead, in these situations we can use a helper type 
family:

```
data IsPrefixOf :: [a] -> [a] -> Exp Bool
type instance Eval (IsPrefixOf xs ys) = IsPrefixOf_ xs ys

-- helper for IsPrefixOf
type family IsPrefixOf_ (xs :: [a]) (ys :: [a]) :: Bool where
    IsPrefixOf_ '[] _ = 'True
    IsPrefixOf_ _ '[] = 'False
    IsPrefixOf_ (x ': xs) (y ': ys) =
         Eval ((Eval (TyEq x y)) && IsPrefixOf_ xs ys)
```

### Using `If`

If possible, try to avoid using `Eval` in the if-branches. 
For example, consider
```
    (If (Eval (s > 0) )
        ( 'Just '( a, s TL.- 1 ))
        'Nothing
    )
```
and
```
    (If (Eval (s > 0))
        (Eval (Pure ( 'Just '( a, s TL.- 1 ))))
        (Eval (Pure 'Nothing))
    )
```

Both compile and it is easy to end up in the latter form, especially if the 
branch is more complex than in this example. 

The former, however, is much better as it doesn't have to evaluate both branches
and is thus more efficient.


### Other


The `ghci` and `:kind!` command are your friends!

Source also contains a lot of examples, see
[fcf-containers](https://github.com/gspia/fcf-containers/tree/master/src/Fcf).
The examples will be left near the code, even thou the doctest runs will be 
removed and replaced with real tests at the test-directory.

Happy :kinding!
