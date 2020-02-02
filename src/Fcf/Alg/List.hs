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
import           Fcf.Classes (Map)
import           Fcf.Data.List (Foldr)
import           Fcf.Utils (If)
import           Fcf.Data.Bool (type (&&), type  (||))
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
type instance Eval (Map f 'NilF) = 'NilF
type instance Eval (Map f ('ConsF a b)) = 'ConsF a (Eval (f b))


-- | ListToFix can be used to turn a norma type-level list into the base
-- functor type ListF, to be used with e.g. Cata. For examples in use, see
-- 'LenAlg' and 'SumAlg'.
-- 
-- Ideally, we would have one ToFix type-level function for which we could
-- give type instances for different type-level types, like lists, trees
-- etc. See TODO.md.
data ListToFix :: [a] -> Exp (Fix (ListF a))
type instance Eval (ListToFix '[]) = 'Fix 'NilF
type instance Eval (ListToFix (a ': as)) = 'Fix ('ConsF a (Eval (ListToFix as)))

-- | Example algebra to calculate list length.
-- 
-- >>> :kind! Eval (Cata LenAlg =<< ListToFix '[1,2,3])
-- Eval (Cata LenAlg =<< ListToFix '[1,2,3]) :: Nat
-- = 3
data LenAlg :: Algebra (ListF a) Nat
type instance Eval (LenAlg 'NilF) = 0
type instance Eval (LenAlg ('ConsF a b)) = 1 TL.+ b

-- | Example algebra to calculate the sum of Nats in a list.
-- 
-- >>> :kind! Eval (Cata SumAlg =<< ListToFix '[1,2,3,4])
-- Eval (Cata SumAlg =<< ListToFix '[1,2,3,4]) :: Nat
-- = 10
data SumAlg :: Algebra (ListF Nat) Nat
type instance Eval (SumAlg 'NilF) = 0
type instance Eval (SumAlg ('ConsF a b)) = a TL.+ b

-- | Example algebra to calculate the prod of Nats in a list.
--
-- >>> :kind! Eval (Cata ProdAlg =<< ListToFix '[1,2,3,4])
-- Eval (Cata ProdAlg =<< ListToFix '[1,2,3,4]) :: Nat
-- = 24
data ProdAlg :: Algebra (ListF Nat) Nat
type instance Eval (ProdAlg 'NilF) = 1
type instance Eval (ProdAlg ('ConsF a b)) = a TL.* b

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

-- | Partition
-- 
-- === __Example__
-- 
-- >>> :kind! Eval (Fcf.Alg.List.Partition ((>=) 35) '[ 20, 30, 40, 50])
-- Eval (Fcf.Alg.List.Partition ((>=) 35) '[ 20, 30, 40, 50]) :: ([Nat],
--                                                                [Nat])
-- = '( '[20, 30], '[40, 50])
data Partition :: (a -> Exp Bool) -> [a] -> Exp ([a],[a])
type instance Eval (Partition p lst) = Eval (Foldr (PartHelp p) '( '[], '[]) lst)

-- helper
data PartHelp :: (a -> Exp Bool) -> a -> ([a],[a]) -> Exp ([a],[a])
type instance Eval (PartHelp p a '(xs,ys)) =
    If (Eval (p a))
        '(a ': xs, ys)
        '(xs, a ': ys)

--------------------------------------------------------------------------------


-- | Give true if all of the booleans in the list are true.
--
-- === __Example__
-- 
-- >>> :kind! Eval (All '[ 'True, 'True])
-- Eval (All '[ 'True, 'True]) :: Bool
-- = 'True
--
-- >>> :kind! Eval (All '[ 'True, 'True, 'False])
-- Eval (All '[ 'True, 'True, 'False]) :: Bool
-- = 'False
data All :: [Bool] -> Exp Bool
type instance Eval (All lst) = Eval (Foldr (&&) 'True lst)


-- | Give true if any of the booleans in the list is true.
--
-- === __Example__
-- 
-- >>> :kind! Eval (Any '[ 'True, 'True])
-- Eval (Any '[ 'True, 'True]) :: Bool
-- = 'True
-- 
-- >>> :kind! Eval (Any '[ 'False, 'False])
-- Eval (Any '[ 'False, 'False]) :: Bool
-- = 'False
data Any :: [Bool] -> Exp Bool
type instance Eval (Any lst) = Eval (Foldr (||) 'False lst)




