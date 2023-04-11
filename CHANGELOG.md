
# 0.8.2

Thank you again to Skyfold (Martin P.)

- Add KnownVal instance for typeable types, this can be used as Show for
  the types presented at the kind level. 
- Fix the Test.Alg test description
- Add Test.Alg.Morphism and corresponding doctests removed


# 0.8.1

20230410

Thank you to Skyfold (Martin P.)

- Generalized KnownVal instances
- Initialized the HSpec use and test directory organization
- Added flake definitions
- Updated stack version (not tested with stack)
- Removed shell.nix
- Fcf.Data.Set doctests removed and replaced with Test.Data.Set
- Fcf.Alg.List doctests removed and replaced with Test.Alg.List


# 0.8.0

20230226

- Fcf.Data.Char for GHC 9.2.X or higher, the Text will start using this.
- Fcf.Data.Tuple with Swap method
- Fcf.Data.Reflect gives methods to turn type-level structures into value-level.
  These include instances for trees, natMaps, maps, lists, and text. Text instances
  are only for GHC 9.2.X or higher.
- Fcf.Data.Symbol for GHC 9.2.x or higher, the Text will start using this.
- Deprecate the current Text and Text.Internal modules and introduce new using
  the Symbol module (these are for GHC 9.2.x or higher).
- Add new dependencies, containers and text (for the Reflect module).
- Several hlint suggestion fixes included

The "NewText" module could have more methods that the counterpart already has.
Also, the Reflect module could have more typical instances.

The test will be moved into the test directory and away from the doctests, which
in turn, will be deprecated. The doctests would be otherwise nice, but they are
difficult (laborous) to support for several different ghc versions.

This also includes Data.List.Utils and there Foldl and MaybeToList methods.


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
