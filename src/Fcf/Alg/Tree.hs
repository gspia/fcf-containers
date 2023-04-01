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
type instance Eval (SumNodesAlg ('NodeF x (b ': bs))) = x TL.+ Eval (Sum (b ': bs))

-- | Count the nodes of TreeF. 
--
-- See the 'Size' for an example.
data CountNodesAlg :: Algebra (TreeF a) Nat
type instance Eval (CountNodesAlg ('NodeF x '[]))       = 1
type instance Eval (CountNodesAlg ('NodeF x (b ': bs))) = 1 TL.+ Eval (Sum (b ': bs))


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
-- Eval (Size =<< UnfoldTree BuildNode 1) :: TL.Natural
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
-- >>> :kind! Eval (FibHylo 10)
-- Eval (FibHylo 10) :: TL.Natural
-- = 55
data FibHylo :: Nat -> Exp Nat
type instance Eval (FibHylo n) = Eval (Hylo SumNodesAlg BuildFibTreeCoA n)


--------------------------------------------------------------------------------

-- | BTreeF is a btree functor. At the moment, it is used to build sorting
-- algorithms.
data BTreeF a b = BEmptyF | BNodeF a b b

-- | BTreeF is a functor
type instance Eval (Map f 'BEmptyF) = 'BEmptyF
type instance Eval (Map f ('BNodeF a b1 b2)) = 'BNodeF a (Eval (f b1)) (Eval (f b2))

--------------------------------------------------------------------------------

-- | A kind of foldable sum class. Pun may or may not be intended.
data FSum :: f a -> Exp a

-- | Instances to make TreeF to be a foldable sum. After this one, we can write
-- the 'Sizes' example.
type instance Eval (FSum ('NodeF a '[])) = 0
type instance Eval (FSum ('NodeF a (b ': bs))) = Eval (Sum (b ': bs))


-- | Sizes example from Recursion Schemes by example, Tim Williams. This annotes
-- each node with the size of its subtree.
--
-- __Example__
--
-- >>> :kind! Eval (Sizes =<< Ana BuildNodeCoA 1)
-- Eval (Sizes =<< Ana BuildNodeCoA 1) :: Fix
--                                          (AnnF (TreeF TL.Natural) TL.Natural)
-- = 'Fix
--     ('AnnF
--        '( 'NodeF
--             1
--             '[ 'Fix
--                  ('AnnF
--                     '( 'NodeF
--                          2
--                          '[ 'Fix ('AnnF '( 'NodeF 4 '[], 1)),
--                             'Fix ('AnnF '( 'NodeF 5 '[], 1))],
--                        3)),
--                'Fix
--                  ('AnnF
--                     '( 'NodeF
--                          3
--                          '[ 'Fix ('AnnF '( 'NodeF 6 '[], 1)),
--                             'Fix ('AnnF '( 'NodeF 7 '[], 1))],
--                        3))],
--           7))
type instance Eval (Sizes fx) = Eval (Synthesize ( (+) 1 <=< FSum) fx)
data Sizes :: Fix f -> Exp (Ann f Nat)

--------------------------------------------------------------------------------


-- | A NatF functor that can be used with different morphisms. This tree-module
-- is probably a wrong place to this one. Now it is here for the Fibonacci
-- example.
data NatF r = Succ r | Zero

-- | NatF has to have functor-instances so that morphisms will work.
type instance Eval (Map f 'Zero) = 'Zero
type instance Eval (Map f ('Succ r)) = 'Succ (Eval (f r))

-- | We want to be able to build NatF Fix-structures out of Nat's.
data NatToFix :: Nat -> Exp (Fix NatF)
type instance Eval (NatToFix n) = Eval
    (If (Eval (n < 1))
        (Pure ('Fix 'Zero))
        (RecNTF =<< n - 1)
    )

-- helper for 'NatToFix' -function
data RecNTF :: Nat -> Exp (Fix NatF)
type instance Eval (RecNTF n) = 'Fix ('Succ (Eval (NatToFix n)))

-- | Efficient Fibonacci algebra from Recursion Schemes by example, Tim Williams.
data FibAlgebra :: NatF (Ann NatF Nat) -> Exp Nat
type instance Eval (FibAlgebra 'Zero) = 0
type instance Eval (FibAlgebra ('Succ ('Fix ('AnnF '( 'Zero, _) )))) = 1
type instance Eval (FibAlgebra ('Succ ('Fix ('AnnF '( 'Succ ('Fix ('AnnF '( _, n))) , m) )))) = Eval (n + m)

-- | Efficient Fibonacci type-level function
-- (from Recursion Schemes by example, Tim Williams). Compare this to
-- 'FibHylo'.
--
-- __Example__
--
-- >>> :kind! Eval (FibHisto 100)
-- Eval (FibHisto 100) :: TL.Natural
-- = 354224848179261915075
data FibHisto :: Nat -> Exp Nat
type instance Eval (FibHisto n) = Eval (Histo FibAlgebra =<< NatToFix n)


