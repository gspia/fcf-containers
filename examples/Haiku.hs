{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

{-|

Example on how to do compile-time (ie type-level) computations and how
to get the results into use on term-level (ie runtime).

This exemplifies the use of @MapC@ and @Text@.

Exercises:
 - Write a method for structurally wrong Haiku's and output something other in those cases
 - Vocabulary with syllables is somewhat parameterized (except WSmap) but 
   not the other half.
   Change CheckHaiku to accepts the vocabulary to use and the haiku the check.

-}

--------------------------------------------------------------------------------

import qualified GHC.TypeLits as TL

import           Data.Proxy

import           Fcf ( Eval, Exp, Map, type (=<<), type (@@), If, IsNothing, Pure
                     , FromMaybe, Flip)
import           Fcf.Data.Nat
import           Fcf.Data.List as L

import           Fcf.Data.MapC as M
import           Fcf.Data.Text as T

import           Fcf.Alg.List (Equal)

--------------------------------------------------------------------------------

-- | Type-level variable containing vocabulary split in syllables.
data HaikuWords :: Exp [[Text]]
type instance Eval HaikuWords =
    '[ '[ 'Text '["a","a"], 'Text '["m","u"]]
     , '[ 'Text '["a","a"], 'Text '["m","u","l"], 'Text '["l","a"]]
     , '[ 'Text '["a"], 'Text '["j","a"], 'Text '["t","u","s"]]
     , '[ 'Text '["j","o"], 'Text '["k","i","n"]]
     , '[ 'Text '["k","i","e"], 'Text '["l","i"]]
     , '[ 'Text '["l","o","i"], 'Text '["k","o","i"], 'Text '["l","e"], 'Text '["v","a"]]
     , '[ 'Text '["m","u","u"]]
     , '[ 'Text '["v","a","n"], 'Text '["h","e"], 'Text '["n","e","e"]]
     , '[ 'Text '["u","u"], 'Text '["s","i"]]
     ]

-- | Turn syllables into words
data MkWords :: [[Text]] -> Exp [Text]
type instance Eval (MkWords words) = Eval (Fcf.Map T.Concat words)

-- | We want ghc to count the the syllables per word for us
data SyllableCount :: [[Text]] -> Exp [Nat]
type instance Eval (SyllableCount words) = Eval (Fcf.Map L.Length words)

-- | Construct a mapping that maps a word to the number of syllables in it
data WordSyllables :: [[Text]] -> Exp (MapC Text Nat)
type instance Eval (WordSyllables words) =
    Eval (M.FromList =<< Zip (MkWords @@ words) (SyllableCount @@ words))

-- | Hmm, type-level global variable...
data WSmap :: Exp (MapC Text Nat)
type instance Eval WSmap = Eval (WordSyllables =<< HaikuWords)

--------------------------------------------------------------------------------

-- | The count of syllables per lines and number of lines that is required for 
-- correct Haiku. This is used for Haiku structural check.
data ReqSyllablesPerLine :: Exp [Nat]
type instance Eval ReqSyllablesPerLine = '[5,7,5]

--------------------------------------------------------------------------------

-- | Our executable associated Haiku we want to check.
data Haiku :: Exp Text
type instance Eval Haiku =
    'Text '[ "k","i","e","l","i"," ","v","a","n","h","e","n","e","e","\n"
           , "l","o","i","k","o","i","l","e","v","a"," ","a","j","a","t","u","s","\n"
           , "a","a","m","u","l","l","a"," ","u","u","s","i"
           -- , "j","o", "k","i","n" -- test with clearly wrong input (won't compile)
           ]

-- | Split the Haiku into more easily processable form
data HaikuAsLineWords :: Exp [[Text]]
type instance Eval HaikuAsLineWords = Eval (Fcf.Map Words =<< Lines =<< Haiku)

-- | After applying the lookups, we have lot's of Maybe's.
data SumJusts :: [Maybe Nat] -> Nat -> Exp Nat
type instance Eval (SumJusts '[] acc) = acc
type instance Eval (SumJusts (n ': ns) acc) = Eval
    (If (IsNothing @@ n)
        (Pure 0)
        (SumJusts ns (Eval (acc + (Eval (FromMaybe 0 n) ))))
    )

-- | The main method, we list of lines, and on each line a list of words,
-- for which we try to find out the syllable count from our map, 
-- and as a last thing we count the syllable sums for each line.
data HaikuSyllCountsPerLine :: Exp [Nat]
type instance Eval HaikuSyllCountsPerLine =
    Eval (Fcf.Map (Flip SumJusts 0)
      =<< Fcf.Map (Fcf.Map (Flip M.Lookup (Eval WSmap)))
      =<< HaikuAsLineWords)

-- | To check the Haiku, compare the correct number of syllables (and at the
-- same time, number of lines) to the figures we got from the input Haiku.
data CheckHaiku :: Exp Bool
type instance Eval CheckHaiku =
    Eval (Equal (Eval ReqSyllablesPerLine) (Eval HaikuSyllCountsPerLine))

--------------------------------------------------------------------------------

-- | We left something here as well. We don't want this executable to compile
-- if the Haiku is not ok.
showHaiku
    :: forall symbol. (symbol ~ Eval (ToSymbol =<< Haiku), 'True ~ Eval CheckHaiku)
    => String
showHaiku = TL.symbolVal @symbol Proxy

main :: IO ()
main = putStrLn $ "The Haiku is:\n" ++ showHaiku


