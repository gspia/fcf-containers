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

The original problem is described in
[aoc 20 d22](https://adventofcode.com/2020/day/22). We use the described
example input data in a bit different form. See the link for the
original data and answer.

We implement the following algorithm on type-level:

>>> playGame2h :: [([Int],[Int])] -> ([Int],[Int]) -> ([Int],Player)
>>> playGame2h _ (p1,[]) = (p1,P1)
>>> playGame2h _ ([],p2) = (p2,P2)
>>> playGame2h hist hs@(v1:r1,v2:r2) =
>>>    if elem hs hist
>>>    then (r1 ++ [v1,v2], P1)
>>>    else
>>>      if length r1 >= v1 && length r2 >= v2
>>>      then
>>>          case playGame2h [] (take v1 r1, take v2 r2) of
>>>            (_,P1) -> playGame2h (hs : hist) (r1 ++ [v1,v2], r2)
>>>            (_,P2) -> playGame2h (hs : hist) (r1, r2 ++ [v2,v1])
>>>      else
>>>         if v1 > v2
>>>         then playGame2h (hs : hist) (r1 ++ [v1,v2], r2)
>>>         else playGame2h (hs : hist) (r1, r2 ++ [v2,v1])
>>>
>>> day22h :: [String] -> Int
>>> day22h strs =
>>>     let (p,_) = playGame2h [] hnds -- 62 min if using <> and 7 s if ++ for list cat
>>>         len = length p
>>>         is =  [1..len]
>>>      in foldl (+) 0 $ zipWith (*) is $ reverse $ p
>>>   where
>>>     hnds = input2handsf strs

-}

--------------------------------------------------------------------------------

import           GHC.TypeLits (natVal)

import           Data.Proxy

import           Fcf (Eval, If, Exp, TyEq, Pure, type (&&), type (<=<), type (=<<)
                     , Uncurry, Bimap, Fst)
import           Fcf.Alg.List (MToN)
import           Fcf.Data.Nat as N
import           Fcf.Data.List (Take, type (++), Elem, Length, Foldr, ZipWith, Reverse)


--------------------------------------------------------------------------------

-- | Example input for which the answer is 291.
type ExampleInput = '( '[9, 2, 6, 3, 1], '[5, 8, 4, 7, 10]) -- :: ([Nat],[Nat])

data Player = P1 | P2

-- :kind! Eval (PlayGame '[] ExampleInput)
data PlayGame :: [([Nat],[Nat])] -> ([Nat],[Nat]) -> Exp ([Nat],Player)
type instance Eval (PlayGame _ '( p1 ': ps , '[])) = '( p1 ': ps, 'P1)
type instance Eval (PlayGame _ '( '[], p2 ': ps)) = '( p2 ': ps , 'P2)
type instance Eval (PlayGame hist '( v1 ': r1, v2 ': r2)) = Eval
    (If (Eval (Elem '(v1 ': r1, v2 ': r2) hist ))
        (Pure '( Eval (r1 ++ '[v1,v2]), 'P1)  )
        (If ( Eval (Eval (v1 <= Eval (Length r1)) && Eval (v2 <= Eval (Length r2)) ))
            (If (Eval ( TyEq (Eval (PlayGame '[] '( Eval (Take v1 r1), Eval (Take v2 r2)) ) ) 'P1))
                (PlayGame ('( v1 ': r1, v2 ': r2) ': hist) '(Eval (r1 ++ '[v1,v2]), r2))
                (PlayGame ('( v1 ': r1, v2 ': r2) ': hist) '(r1, Eval (r2 ++ '[v2,v1])))
            )
            (If (Eval (v1 > v2))
                (PlayGame ('( v1 ': r1, v2 ': r2) ': hist) '(Eval (r1 ++ '[v1,v2]), r2))
                (PlayGame ('( v1 ': r1, v2 ': r2) ': hist) '(r1, Eval (r2 ++ '[v2,v1])))
            )
        )
    )

-- | Helper.
data AsPair :: a -> Exp (a,a)
type instance Eval (AsPair a) = '(a,a)


-- | Calculating the result from the winners hand.
--
-- :kind! Eval (Day22 ExampleInput)
data Day22 :: ([Nat],[Nat]) -> Exp Nat
type instance Eval (Day22 input) = Eval
    (
    Foldr (N.+) 0
    =<< Uncurry (ZipWith (N.*))
    =<< Bimap ((MToN 1) <=< Length) Reverse
    =<< AsPair
    =<< Fst
    =<< PlayGame '[] input
    )

-- | The actual larger input. The term-level implementation time consumption
-- is very sensitive to the compiler being able to optimize it.  (E.g. use tail
-- recursion?). Interesting to see, how ghc is doing this on type-level.
--
-- Does not compile on my machine (not even with the -freduction-depth=0 at cabal),
-- not enough memory
-- Ghci also gets 'Killed'.
type Input =
    '( '[21, 50,  9, 45, 16, 47, 27, 38, 29, 48, 10, 42, 32, 31, 41, 11,  8, 33, 25, 30, 12, 40,  7, 23, 46]
     , '[22, 20, 44,  2, 26, 17, 34, 37, 43,  5, 15, 18, 36, 19, 24, 35,  3, 13, 14,  1,  6, 39, 49,  4, 28]
     ) :: ([Nat], [Nat])

-- On my machine, the compilation takes about 3 minutes
type Input2 =
    '( '[21, 50,  9, 45, 16, 47, 27, 38, 29, 48, 10,    7, 23, 46]
     , '[22, 20, 44,  2, 26, 17, 34, 37, 43,  5, 15,   49,  4, 28]
     ) :: ([Nat], [Nat])

-- On my machine, does not compilate, not enough memory.
type Input3 =
    '( '[21, 50,  9, 45, 16, 47, 27, 38, 29, 48, 10,   12, 40,  7, 23, 46]
     , '[22, 20, 44,  2, 26, 17, 34, 37, 43,  5, 15,    6, 39, 49,  4, 28]
     ) :: ([Nat], [Nat])

-- On my machine, does not compilate, not enough memory.
type Input4 =
    '( '[21, 50,  9, 45, 16, 47, 27, 38, 29, 48, 10,   40,  7, 23, 46]
     , '[22, 20, 44,  2, 26, 17, 34, 37, 43,  5, 15,   39, 49,  4, 28]
     ) :: ([Nat], [Nat])




--------------------------------------------------------------------------------

-- | Bring the result to term-level.
showExampleInputResult :: forall result. (result ~ Eval (Day22 ExampleInput)) => String
showExampleInputResult = show $ natVal @result Proxy

-- showResult :: forall result. (result ~ Eval (Day22 Input2)) => String
-- showResult :: forall result. (result ~ Eval (Day22 Input3)) => String
-- showResult :: forall result. (result ~ Eval (Day22 Input4)) => String
-- showResult = show $ natVal @result Proxy

main :: IO ()
main = putStrLn $ "The result is:\n" ++ showExampleInputResult
-- main = putStrLn $ "The result is:\n" ++ showResult

