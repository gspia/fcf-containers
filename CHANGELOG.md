
# 0.7.2

2023015

- Add notes about defunctionalization and associated code examples
- Add helper module for Foldl method (it would be better at first-class-families lib).

# 0.7.1

20220601

 - Clean up several modules (remove extra spaces and paranthesis)
 - Update doctest function and remove Glob dependency
 - Update ci pipeline to do tests with 9.0 and 9.2

# 0.7.0

20220424

 - Add preliminary support for Applicative and Monadic computations
 - Small whitespace fixes
 - Added dependency for mtl (to get Identity)
 - Add new compiler versions for tests and drop some old
 - Start referring to cabal version 3.0

# 0.6.1

20210130

Start using the github actions and remove travis CI.

# 0.6.0

20210130

 - use symbol as text representation (we can split predefined char-sets, as shown
   by the Csongor at https://kcsongor.github.io/symbol-parsing-haskell/ see also
   the symbols-package)
 - and hie.yaml (for language server)
 - minor documentation fixes
 - add new example that really stresses ghc (Advent-of-code 2020, day 22b)
 - MToN to Alg.List (this generates Nat-list from M to N)
 - Nat equality and inequality test

# 0.5.0

20200323

 - start using version 0.8.0 of first-class-families
 - a documentation fix

# 0.4.0

20200216

 - add Para
 - add Histo
 - add NatF
 - and examples 
 - correct the Text Length example
 - NatMap (mimick IntMap)

# 0.3.0

20200209

 - a separate Sort module
 - add possibility to sort a list of lists
 - a Text module
 - several list functions (And, Or, All, Any, ..)
 - IsProperSubsetOf, IsSubsetOf in Set
 - add new example that uses MapC and Text

# 0.2.0

 - remove the methods that can be found from fcf 0.7.0
 - fix the examples that didn't work with ghc 8.8.1 test-doc
 - add Fcf.Data.Symbol module (comparison + other functions)
 - change some of the examples from using Eval to use =<< operator
   (less parenthesis and easier to read this way)
 - format the documentation a bit
 - add PowerSet and ToList into Set-module
 - add new test module

# 0.1.0

Initial version on 20200129. The next version probably comes out quickly after 
this one.
