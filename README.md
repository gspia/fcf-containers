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
tool to have.
 
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

See [Orbits.hs](https://github.com/gspia/fcf-containers/blob/master/examples/Orbits.hs). 
It shows how to solve a real problem,
what PRAGMAs are probably needed etc.

```
cabal run orbits 
```

The `ghci` and `:kind!` command in there are your friends!

Source also contains a lot of examples, see
[fcf-containers](https://github.com/gspia/fcf-containers/tree/master/src/Fcf).


Happy :kinding!
