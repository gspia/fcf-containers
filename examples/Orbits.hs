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
[aoc 19 d6](https://adventofcode.com/2019/day/6). We use the described
example input data in a bit different form. See the link for the
original data and answer.

-}

--------------------------------------------------------------------------------

import qualified GHC.TypeLits as TL

import           Data.Proxy

import           Fcf
import           Fcf.Data.Nat
import           Fcf.Data.List as L

import           Fcf.Alg.Morphism
import           Fcf.Alg.List
import           Fcf.Alg.Tree

--------------------------------------------------------------------------------


-- | Type-level variable containing all the required information.
data OrbitInput :: Exp [(TL.Symbol, TL.Symbol)]
type instance Eval OrbitInput =
                   '[ '("COM", "B")
                    , '("B", "C")
                    , '("C", "D")
                    , '("D", "E")
                    , '("E", "F")
                    , '("B", "G")
                    , '("G", "H")
                    , '("D", "I")
                    , '("E", "J")
                    , '("J", "K")
                    , '("K", "L")]

-- helper, we might could do without this one
-- This is used to check that two symbols are same.
data EqPlanet :: TL.Symbol -> TL.Symbol -> Exp Bool
type instance Eval (EqPlanet a b) = Eval (TyEq 'EQ (TL.CmpSymbol a b))

-- | :kind! Eval (FindDescs "COM" =<< OrbitInput)
data FindDescs :: TL.Symbol -> [(TL.Symbol, TL.Symbol)] -> Exp [TL.Symbol]
type instance Eval (FindDescs p lst) = 
    Eval (Map Snd =<< (Filter (EqPlanet p <=< Fst) lst))

-- | This is not used in the final solution but worked as a step towards it.
--
-- :kind! Eval (Ana BuildOrbTreeCoA "COM")
data BuildOrbTreeCoA :: CoAlgebra (TreeF TL.Symbol) TL.Symbol
type instance Eval (BuildOrbTreeCoA nd) =
    If (Eval (L.Null (Eval (FindDescs nd =<< OrbitInput))))
        ('NodeF nd '[])
        ('NodeF nd (Eval (FindDescs nd =<< OrbitInput)))

-- |
-- We need to store the depth of the node along the tree. We use the depth when
-- counting paths from root to every node.
--
-- :kind! Eval (Ana BuildOrbTreeCoA2 '(0,"COM"))
data BuildOrbTreeCoA2 :: CoAlgebra (TreeF (Nat,TL.Symbol)) (Nat,TL.Symbol)
type instance Eval (BuildOrbTreeCoA2 '(d,nd)) =
    If (Eval (L.Null (Eval (FindDescs nd =<< OrbitInput))))
        ('NodeF '(d,nd) '[])
        ('NodeF '(d,nd) (Eval (MkPairLst (1 TL.+ d) =<< FindDescs nd =<< OrbitInput)))

-- helper, we might could do without this one
data MkPair :: Nat -> TL.Symbol -> Exp (Nat,TL.Symbol)
type instance Eval (MkPair n s) = '(n,s)

-- helper, we might could do without this one
data MkPairLst :: Nat -> [TL.Symbol] -> Exp [(Nat,TL.Symbol)]
type instance Eval (MkPairLst n lst) = Eval (Map (MkPair n) lst)


-- | Count paths from root to every node.
--
-- If it is a leaf, the number of paths from root to it is the depth.
-- If it is an internal node, the number of paths to the subtree rooted
-- by it is sum of pathnums of subtrees plus the depth (paths to this one).
data CountPathsAlg :: Algebra (TreeF (Nat,TL.Symbol)) Nat
type instance Eval (CountPathsAlg ('NodeF '(d,_) '[])) = d
type instance Eval (CountPathsAlg ('NodeF '(d,_) (b ': bs))) = d TL.+ (Eval (Sum (b ': bs)))


-- | We initialize this with the known starting node and the depth of it, which
-- is 0.
--
-- :kind! Eval OrbitSolution
data OrbitSolution :: Exp Nat
type instance Eval OrbitSolution =
    Eval (Hylo CountPathsAlg BuildOrbTreeCoA2 '(0,"COM"))

-- | Helper to bring in the result of the type-level computation.
--
-- It is a nice exercise to solve this using Tree, UnfoldTree and FoldTree.
showResult :: forall n. (n ~ Eval OrbitSolution) => String
showResult = show $ TL.natVal @n Proxy

main :: IO ()
main = putStrLn $ "Number of orbits is " ++ showResult

