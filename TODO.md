# TODO

## Examples:

E.g. add examples using the other data structures.

## Data structures:

Add the missing methods and doctests.

- Set
- NatMap
- Graphs
- MapB (Map data structure) but implemented with the balanced trees
- MapC 
 

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

