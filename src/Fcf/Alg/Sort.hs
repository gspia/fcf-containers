{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

{-|
Module      : Fcf.Alg.Sort
Description : Type-level sorting algorithms and utilities
Copyright   : (c) gspia 2020-
License     : BSD
Maintainer  : gspia

= Fcf.Alg.Sort

-}

--------------------------------------------------------------------------------

module Fcf.Alg.Sort where

import qualified GHC.TypeLits as TL

import           Fcf ( If, Eval, Exp, type (<=<), type (=<<)
                     , Flip, Not, TyEq, Pure )
import           Fcf.Data.List ( ZipWith, Filter, type (++) )
import           Fcf.Data.Nat (Nat)
import           Fcf.Data.Symbol (Symbol)

--------------------------------------------------------------------------------

import           Fcf.Alg.Tree (BTreeF(..))
import           Fcf.Alg.Morphism
import           Fcf.Alg.Symbol (SymbolOrd)

--------------------------------------------------------------------------------

-- For the doctests:

-- $setup
-- >>> import qualified Fcf.Data.Nat as N ( type (<) )
-- >>> import qualified Fcf.Alg.Symbol as S ( type (<) )

--------------------------------------------------------------------------------

-- helper for the ListCmp
data ListCmpFnd :: [Ordering] -> Exp Ordering
type instance Eval (ListCmpFnd '[]) = 'EQ
type instance Eval (ListCmpFnd (a ': as)) = Eval
    (If (Eval (TyEq a 'EQ))
        (ListCmpFnd as)
        (Pure a)
    )

-- | Compare lists with the given comparison for the elements.
data ListCmp :: (a -> a -> Exp Ordering) -> [a] -> [a] -> Exp Ordering
type instance Eval (ListCmp f as bs) = Eval (ListCmpFnd =<< ZipWith f as bs)

-- | Give true if the first list is before the second, given the comparison
-- function for the elements.
data ListOrd :: (a -> a -> Exp Ordering) -> [a] -> [a] -> Exp Bool
type instance Eval (ListOrd f as bs) = Eval
    (If (Eval (TyEq 'LT (Eval (ListCmp f as bs))))
        (Pure 'True)
        (Pure 'False)
    )

-- | Comparison for the Nats.
-- 
-- TODO: Would this fit to Fcf.Data.Nat on first-class-families?
data NatOrd :: Nat -> Nat -> Exp Ordering
type instance Eval (NatOrd a b) = TL.CmpNat a b

-- | Comparison for Symbol lists.
--
-- Useful when sorting with Qsort.
data SymbolListOrd :: [Symbol] -> [Symbol] -> Exp Bool
type instance Eval (SymbolListOrd as bs) = Eval (ListOrd SymbolOrd as bs)

-- | Comparison for Nat lists.
--
-- Useful when sorting with Qsort.
data NatListOrd :: [Nat] -> [Nat] -> Exp Bool
type instance Eval (NatListOrd as bs) = Eval (ListOrd NatOrd as bs)

--------------------------------------------------------------------------------


-- helper
data PartHlp :: (a -> a -> Exp Bool) -> CoAlgebra (BTreeF a) [a]
type instance Eval (PartHlp _ '[]) = 'BEmptyF
type instance Eval (PartHlp smaller (h ': t)) =
    'BNodeF h
        (Eval (Filter (smaller h) t))
        (Eval (Filter (Not <=< smaller h) t))

-- helper
data Inord :: Algebra (BTreeF a) [a]
type instance Eval (Inord 'BEmptyF) = '[]
type instance Eval (Inord ('BNodeF v l r)) = Eval (l ++ (Eval ('[v] ++ r)))


-- | Qsort - give the comparison function @a -> a -> Exp Bool@ comparing your 
-- list elements and then Qsort will order the list.
--
-- __Example__
--
-- >>> :kind! Eval (Qsort (N.<) '[5,3,1,9,4,6,3])
-- Eval (Qsort (N.<) '[5,3,1,9,4,6,3]) :: [Nat]
-- = '[1, 3, 3, 4, 5, 6, 9]
--
-- >>> :kind! Eval (Qsort (S.<) '[ "bb", "e", "a", "e", "d" ])
-- Eval (Qsort (S.<) '[ "bb", "e", "a", "e", "d" ]) :: [Symbol]
-- = '["a", "bb", "d", "e", "e"]
data Qsort :: (a -> a -> Exp Bool) -> [a] -> Exp [a]
type instance Eval (Qsort cmp lst) = Eval (Hylo Inord (PartCmp cmp) lst)

-- Helper
--
-- We use the Flip version so that using <-comparison will give an inreasing
-- Nat-list. Sorting would work without PartCmp.
data PartCmp :: (a -> a -> Exp Bool) -> CoAlgebra (BTreeF a) [a] 
type instance Eval (PartCmp cmp coalg) = Eval (PartHlp (Flip cmp) coalg)


