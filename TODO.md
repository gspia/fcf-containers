# TODO

## Examples and tests:

E.g. add examples using the other data structures. Start building the
tests in the test-directory. The doc-tests are hard to keep working for
several GHC versions at the same time and deprecate them.

This will also mean that the code modules will be come a bit cleaner.


## Data structures:

Add the missing methods and tests. Move doctests under tests.

- Set
- NatMap
- Graphs
- MapB (Map data structure) but implemented with the balanced trees
- MapC 
- Text (e.g. those folding methods)


The test structuring requires a bit thinking. New dependencies or some 
simple structure to help here?
 

## Morphisms:
 
- Apo with examples
- Futu with examples
- and other 
 
Also, consider moving morphisms into another package. 

At the moment, this lib provides `<DataStructure>ToFix`-functions. It would be 
really nice to have one method that could be overloaded, called e.g. `ToFix`. 
This would allow us to implement the morphisms similarly as in the term-level 
libs without a need for the user to do fixing. Doable?

The current definition of `Fix` might be too simple for other morphisms.


## Other things

Start using module exports.

Some of the names are really really bad/horrible. Suggestions are wellcome.
And there might be a need to some internal reorganisations of the methods between
modules.

Further, there are several methods that might have a better fit in the Fcf-lib.
(And some will be on the next published version of fcf and thus will be removed
from this fcf-containers.)

How to check if everything is really done compile time? Or alternatively, how
to check, which parts are done on compile time and what is left to run time.

Error messages... yeah, try it, you get what you write :) At the moment this
can be quite confusing occasionally. Is there a way to improve these?

Examples on how to use these with singletons? Maybe in another package?

