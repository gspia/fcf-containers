{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

{-|
Module      : Fcf.Alg.List
Description : ListF structure working with Algebras, ColAlgebras, and other stuff
Copyright   : (c) gspia 2020-
License     : BSD
Maintainer  : gspia

= Fcf.Alg.List

Type-level 'ListF' to be used with Cata, Ana and Hylo.

This module also contains other list-related functions (that might move to
other place some day).

-}

--------------------------------------------------------------------------------

module Fcf.Alg.List where

import qualified GHC.TypeLits as TL

import           Fcf.Core (Eval, Exp)
import           Fcf.Class.Foldable (And)
import           Fcf.Class.Functor (FMap)
import           Fcf.Combinators (type (=<<), Pure)
import           Fcf.Data.List (Foldr, Reverse, ZipWith, Elem, Take, Unfoldr)
import           Fcf.Utils (If, TyEq)
import           Fcf.Data.Nat

import           Fcf.Alg.Morphism

--------------------------------------------------------------------------------

-- For the doctests:

-- $setup
-- >>> import           Fcf.Combinators

--------------------------------------------------------------------------------

-- | Base functor for a list of type @[a]@.
data ListF a b = ConsF a b | NilF

-- We need a functor instance for Cata and Ana to work.
type instance Eval (FMap f 'NilF) = 'NilF
type instance Eval (FMap f ('ConsF a b)) = 'ConsF a (Eval (f b))

--------------------------------------------------------------------------------

-- | ListToFix can be used to turn a norma type-level list into the base
-- functor type ListF, to be used with e.g. Cata. For examples in use, see
-- 'LenAlg' and 'SumAlg'.
--
-- Ideally, we would have one ToFix type-level function for which we could
-- give type instances for different type-level types, like lists, trees
-- etc. See TODO.md.
--
-- === __Example__
--
-- >>> :kind! Eval (ListToFix '[1,2,3])
-- Eval (ListToFix '[1,2,3]) :: Fix (ListF Nat)
-- = 'Fix ('ConsF 1 ('Fix ('ConsF 2 ('Fix ('ConsF 3 ('Fix 'NilF))))))
data ListToFix :: [a] -> Exp (Fix (ListF a))
type instance Eval (ListToFix '[]) = 'Fix 'NilF
type instance Eval (ListToFix (a ': as)) = 'Fix ('ConsF a (Eval (ListToFix as)))

-- | Example algebra to calculate list length.
--
-- === __Example__
--
-- >>> :kind! Eval (Cata LenAlg =<< ListToFix '[1,2,3])
-- Eval (Cata LenAlg =<< ListToFix '[1,2,3]) :: Nat
-- = 3
data LenAlg :: Algebra (ListF a) Nat
type instance Eval (LenAlg 'NilF) = 0
type instance Eval (LenAlg ('ConsF a b)) = 1 TL.+ b

-- | Example algebra to calculate the sum of Nats in a list.
--
-- === __Example__
--
-- >>> :kind! Eval (Cata SumAlg =<< ListToFix '[1,2,3,4])
-- Eval (Cata SumAlg =<< ListToFix '[1,2,3,4]) :: Nat
-- = 10
data SumAlg :: Algebra (ListF Nat) Nat
type instance Eval (SumAlg 'NilF) = 0
type instance Eval (SumAlg ('ConsF a b)) = a TL.+ b

-- | Example algebra to calculate the prod of Nats in a list.
--
-- === __Example__
--
-- >>> :kind! Eval (Cata ProdAlg =<< ListToFix '[1,2,3,4])
-- Eval (Cata ProdAlg =<< ListToFix '[1,2,3,4]) :: Nat
-- = 24
data ProdAlg :: Algebra (ListF Nat) Nat
type instance Eval (ProdAlg 'NilF) = 1
type instance Eval (ProdAlg ('ConsF a b)) = a TL.* b

--------------------------------------------------------------------------------

-- | Form a Fix-structure that can be used with Para.
--
-- === __Example__
--
-- >>> :kind! Eval (ListToParaFix '[1,2,3])
-- Eval (ListToParaFix '[1,2,3]) :: Fix (ListF (Nat, [Nat]))
-- = 'Fix
--     ('ConsF
--        '(1, '[2, 3])
--        ('Fix ('ConsF '(2, '[3]) ('Fix ('ConsF '(3, '[]) ('Fix 'NilF))))))
data ListToParaFix :: [a] -> Exp (Fix (ListF (a,[a])))
type instance Eval (ListToParaFix '[]) = 'Fix 'NilF
type instance Eval (ListToParaFix (a ': as)) =
    'Fix ('ConsF '(a,as) (Eval (ListToParaFix as)))

-- | Example from recursion-package by Vanessa McHale.
--
-- This removes duplicates from a list (by keeping the right-most one).
--
-- === __Example__
--
-- >>> :kind! Eval (Para DedupAlg =<< ListToParaFix '[1,1,3,2,5,1,3,2])
-- Eval (Para DedupAlg =<< ListToParaFix '[1,1,3,2,5,1,3,2]) :: [Nat]
-- = '[5, 1, 3, 2]
data DedupAlg :: RAlgebra (ListF (a,[a])) [a]
type instance Eval (DedupAlg 'NilF) = '[]
type instance Eval (DedupAlg ('ConsF '(a,as) '(_fxs, past))) = Eval
    (If (Eval (TyEq (Eval (Elem a past)) 'True ))
        (Pure past)
        (Pure (a ': as))
    )


-- | Example from Recursion Schemes by example by Tim Williams.
--
-- === __Example__
--
-- >>> :kind! Eval (Sliding 3 '[1,2,3,4,5,6])
-- Eval (Sliding 3 '[1,2,3,4,5,6]) :: [[Nat]]
-- = '[ '[1, 2, 3], '[2, 3, 4], '[3, 4, 5], '[4, 5, 6], '[5, 6], '[6]]
data Sliding :: Nat -> [a] -> Exp [[a]]
type instance Eval (Sliding n lst) =
    Eval (Para (SlidingAlg n) =<< ListToParaFix lst)

-- | Tim Williams, Recursion Schemes by example, example for Para.
-- See 'Sliding'-function.
data SlidingAlg :: Nat -> RAlgebra (ListF (a, [a])) [[a]]
type instance Eval (SlidingAlg _ 'NilF) = '[]
type instance Eval (SlidingAlg n ('ConsF '(a,as) '(_fxs,past))) =
    Eval (Take n (a ': as)) ': past


-- | Tim Williams, Recursion Schemes by example, example for Histo.
data EvensStrip :: ListF a (Ann (ListF a) [a]) -> Exp [a]
type instance Eval (EvensStrip 'NilF) = '[]
type instance Eval (EvensStrip ('ConsF x y)) = x ': Eval (Attr y)


-- | Tim Williams, Recursion Schemes by example, example for Histo.
data EvensAlg :: ListF a (Ann (ListF a) [a]) -> Exp [a]
type instance Eval (EvensAlg 'NilF) = '[]
type instance Eval (EvensAlg ('ConsF _ rst )) = Eval (EvensStrip =<< Strip rst)

-- | This picks up the elements on even positions on a list and is an 
-- example on how to use Histo. This example is
-- from Tim Williams, Recursion Schemes by example.
--
-- === __Example__
--
-- >>> :kind! Eval (Evens =<< RunInc 8)
-- Eval (Evens =<< RunInc 8) :: [Nat]
-- = '[2, 4, 6, 8]
data Evens :: [a] -> Exp [a]
type instance Eval (Evens lst) = Eval (Histo EvensAlg =<< ListToFix lst)

-- | For the ListRunAlg
data NumIter :: a -> Nat -> Exp (Maybe (a,Nat))
type instance Eval (NumIter a s) =
    (If (Eval (s > 0) )
        ( 'Just '( a, s TL.- 1 ))
        'Nothing
    )

-- | For the RunInc
data ListRunAlg :: Nat -> Exp (Maybe (Nat,Nat))
type instance Eval (ListRunAlg s) = Eval (NumIter s s )

-- | Construct a run (that is, a natuaral number sequence from 1 to arg).
--
-- === __Example__
--
-- >>> :kind! Eval (RunInc 8)
-- Eval (RunInc 8) :: [Nat]
-- = '[1, 2, 3, 4, 5, 6, 7, 8]
data RunInc :: Nat -> Exp [Nat]
type instance Eval (RunInc n) = Eval (Reverse =<< Unfoldr ListRunAlg n)


--------------------------------------------------------------------------------

-- | Sum a Nat-list.
--
-- === __Example__
--
-- >>> :kind! Eval (Sum '[1,2,3])
-- Eval (Sum '[1,2,3]) :: Nat
-- = 6
data Sum :: [Nat] -> Exp Nat
type instance Eval (Sum ns) = Eval (Foldr (+) 0 ns)

--------------------------------------------------------------------------------

-- | Helper to form Nat lists like [1..5] or [3..10]
--
-- :kind! Eval (Unfoldr ToThree 1)
data MToNHelp :: Nat -> Nat -> Exp (Maybe (Nat, Nat))
type instance Eval (MToNHelp n b) =
    If (Eval (b >= (n TL.+1)))
        'Nothing
        ('Just '(b, b TL.+ 1))

-- | Function to form Nat lists like [1..5] or [3..10]
--
-- === __Example__
--
-- >>> :kind! Eval (MToN 1 3)
-- Eval (MToN 1 3) :: [Nat]
-- = '[1, 2, 3]
data MToN :: Nat -> Nat -> Exp [Nat]
type instance Eval (MToN m n) = Eval (Unfoldr (MToNHelp n) m)



-- | ToList for type-level lists.
--
-- === __Example__
--
-- >>> :kind! Eval (ToList 1)
-- Eval (ToList 1) :: [Nat]
-- = '[1]
data ToList :: a -> Exp [a]
type instance Eval (ToList a) = '[a]


-- | Equal tests for list equality. We may change the name to (==).
--
-- === __Example__
--
-- >>> :kind! Eval (Equal '[1,2,3] '[1,2,3])
-- Eval (Equal '[1,2,3] '[1,2,3]) :: Bool
-- = 'True
--
-- >>> :kind! Eval (Equal '[1,2,3] '[1,3,2])
-- Eval (Equal '[1,2,3] '[1,3,2]) :: Bool
-- = 'False
data Equal :: [a] -> [a] -> Exp Bool
type instance Eval (Equal as bs) = Eval (And =<< ZipWith TyEq as bs)

