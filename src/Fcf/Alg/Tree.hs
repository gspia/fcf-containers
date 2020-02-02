{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

{-|
Module      : Fcf.Alg.Tree
Description : Tree-structures working with Algebras, ColAlgebras, and other stuff
Copyright   : (c) gspia 2020-
License     : BSD
Maintainer  : gspia

= Fcf.Alg.Tree

Type-level 'TreeF' and 'BTreeF' to be used with Cata, Ana and Hylo. This also
provides some algorithms: general purpose sorting with 'Qsort', 'Size' of an
Tree, Fibonaccis.

-}

--------------------------------------------------------------------------------

module Fcf.Alg.Tree where

import qualified GHC.TypeLits as TL

import           Fcf
import           Fcf.Data.Nat

import           Fcf.Alg.List
import           Fcf.Alg.Morphism 
import           Fcf.Data.Tree

--------------------------------------------------------------------------------

-- | 'TreeF' is functor for 'Tree's. 'TreeF' has Map-instance (on structure).
data TreeF a b = NodeF a [b]

-- provide Map instances for TreeF.
type instance Eval (Map f ('NodeF a '[])) = 'NodeF a '[]
type instance Eval (Map f ('NodeF a (b ': bs))) = 'NodeF a (Eval (Map f (b ': bs)))

-- | A function to transform a Tree into fixed structure that can be used
-- by Cata and Ana.
-- 
-- See the implementation of 'Size' for an example.
data TreeToFix :: Tree a -> Exp (Fix (TreeF a))
type instance Eval (TreeToFix ('Node a '[])) = 'Fix ('NodeF a '[])
type instance Eval (TreeToFix ('Node a (b ': bs))) =
    'Fix ('NodeF a (Eval (Map TreeToFix (b ': bs))))

--------------------------------------------------------------------------------

-- | Sum the nodes of TreeF containing Nats. 
-- 
-- See the implementation of 'Fib' for an example.
data SumNodesAlg :: Algebra (TreeF Nat) Nat
type instance Eval (SumNodesAlg ('NodeF x '[]))       = x
type instance Eval (SumNodesAlg ('NodeF x (b ': bs))) = x TL.+ (Eval (Sum (b ': bs)))

-- | Count the nodes of TreeF. 
-- 
-- See the 'Size' for an example.
data CountNodesAlg :: Algebra (TreeF a) Nat
type instance Eval (CountNodesAlg ('NodeF x '[]))       = 1
type instance Eval (CountNodesAlg ('NodeF x (b ': bs))) = 1 TL.+ (Eval (Sum (b ': bs)))


-- | Size of the Tree is the number of nodes in it.
-- 
-- __Example__
--
-- Size is defined as @ Cata CountNodesAlg =<< TreeToFix tr @
-- and can be used with the following. 
--
-- >>> data BuildNode :: Nat -> Exp (Nat,[Nat])
-- >>> :{
--   type instance Eval (BuildNode x) =
--       If (Eval ((2 TL.* x TL.+ 1) >= 8))
--           '(x, '[])
--           '(x, '[ 2 TL.* x, (2 TL.* x) TL.+ 1 ])
-- :}
--
-- >>> :kind! Eval (Size =<< UnfoldTree BuildNode 1)
-- Eval (Size =<< UnfoldTree BuildNode 1) :: Nat
-- = 7
data Size :: Tree a -> Exp Nat
type instance Eval (Size tr) = Eval (Cata CountNodesAlg =<< TreeToFix tr)


-- | CoAlgebra to build TreeF's.
-- This is an example from containers-package. See 'Size' and example in there.
--
-- :kind! Eval (Ana BuildNodeCoA 1)
-- :kind! Eval (Hylo CountNodesAlg BuildNodeCoA 1)
data BuildNodeCoA :: CoAlgebra (TreeF Nat) Nat
type instance Eval (BuildNodeCoA n) =
    If (Eval (((2 TL.* n) TL.+ 1) >= 8))
        ('NodeF n '[])
        ('NodeF n '[ 2 TL.* n, (2 TL.* n) TL.+ 1 ])
    

-- | CoAlgebra for the Fib-function.
data BuildFibTreeCoA :: CoAlgebra (TreeF Nat) Nat
type instance Eval (BuildFibTreeCoA n) =
    If (Eval (n >= 2))
        ('NodeF 0 '[n TL.- 1, n TL.- 2])
        ('NodeF n '[])


-- | Fibonaccis with Hylo, not efficient
-- 
-- __Example__
--
-- >>> :kind! Eval (Fib 10)
-- Eval (Fib 10) :: Nat
-- = 55
data Fib :: Nat -> Exp Nat
type instance Eval (Fib n) = Eval (Hylo SumNodesAlg BuildFibTreeCoA n)


--------------------------------------------------------------------------------

-- | BTreeF is a btree functor. At the moment, it is used to build sorting
-- algorithms.
data BTreeF a b = BEmptyF | BNodeF a b b

-- functor
type instance Eval (Map f 'BEmptyF) = 'BEmptyF
type instance Eval (Map f ('BNodeF a b1 b2)) = 'BNodeF a (Eval (f b1)) (Eval (f b2))

