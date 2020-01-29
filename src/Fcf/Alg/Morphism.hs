{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

{-|
Module      : Fcf.Alg.Morphism
Description : Algebra, ColAlgebra, and other stuff
Copyright   : (c) gspia 2020-
License     : BSD
Maintainer  : gspia

= Fcf.Alg.Morphism

Type-level 'Cata' and 'Ana' can be used to do complex computation that live only
on type-level or on compile-time. As an example, see the sorting algorithm
in Fcf.Alg.Tree -module.

This module also provides some other type-level functions that probably will
find other place after a while. E.g. 'First' and 'Second' and their instances
on Either and tuple.

-}

--------------------------------------------------------------------------------

module Fcf.Alg.Morphism where

import           Fcf

--------------------------------------------------------------------------------

-- For the doctests:

-- $setup
-- >>> import qualified GHC.TypeLits as TL
-- >>> import           Fcf.Alg.List
-- >>> import           Fcf.Data.Nat

--------------------------------------------------------------------------------

-- | Structure that 'Cata' can fold and that is a result structure of 'Ana'.
data Fix f = Fix (f (Fix f))

-- | Commonly used name describing the method 'Cata' eats.
type Algebra f a = f a -> Exp a

-- | Commonly used name describing the method 'Ana' eats.
type CoAlgebra f a = a -> Exp (f a)

--------------------------------------------------------------------------------


-- | Write the function to give a 'Fix', and feed it in together with an
-- 'Algebra'.
-- 
-- Check Fcf.Alg.List to see example algebras in use.
data Cata :: Algebra f a -> Fix f -> Exp a
type instance Eval (Cata alg ('Fix b)) = alg @@ (Eval (Map (Cata alg) b))

-- | Ana can also be used to build a 'Fix' structure.
--
-- __Example__
-- 
-- >>> data NToOneCoA :: CoAlgebra (ListF Nat) Nat
-- >>> :{ 
--   type instance Eval (NToOneCoA b) = 
--     If (Eval (b < 1) )
--         'NilF
--         ('ConsF b ( b TL.- 1))
-- :}
-- 
-- >>> :kind! Eval (Ana NToOneCoA 3)
-- Eval (Ana NToOneCoA 3) :: Fix (ListF Nat)
-- = 'Fix ('ConsF 3 ('Fix ('ConsF 2 ('Fix ('ConsF 1 ('Fix 'NilF))))))
data Ana :: CoAlgebra f a -> a -> Exp (Fix f)
type instance Eval (Ana coalg a) = 'Fix (Eval (Map (Ana coalg) (Eval (coalg a))))


-- | 
-- Hylomorphism uses first 'Ana' to build a structure (unfold) and then 'Cata'
-- to process the structure (fold).
-- 
-- __Example__
-- 
-- >>> data NToOneCoA :: CoAlgebra (ListF Nat) Nat
-- >>> :{ 
--   type instance Eval (NToOneCoA b) = 
--     If (Eval (b < 1) )
--         'NilF
--         ('ConsF b ( b TL.- 1))
-- :}
-- 
-- >>> :kind! Eval (Hylo SumAlg NToOneCoA 5)
-- Eval (Hylo SumAlg NToOneCoA 5) :: Nat
-- = 15
data Hylo :: Algebra f a -> CoAlgebra f b -> b -> Exp a
type instance Eval (Hylo alg coalg a) = Eval (Cata alg =<< Ana coalg a)

--------------------------------------------------------------------------------

-- | Type-level First. Tuples @(,)@ and @Either@ have First-instances.
-- 
-- __Example__
-- 
-- >>> :kind! Eval (First ((+) 1) '(3,"a"))
-- Eval (First ((+) 1) '(3,"a")) :: (Nat, TL.Symbol)
-- = '(4, "a")
data First :: (a -> Exp b) -> f a c -> Exp (f b c)

-- (,)
type instance Eval (First f '(a,b)) = '(f @@ a, b)

-- Either
type instance Eval (First f ('Left a)) = 'Left (f @@ a)
type instance Eval (First f ('Right b)) = 'Right b

-- | Type-level Second. Tuples @(,)@ and @Either@ have Second-instances.
-- 
-- __Example__
-- 
-- >>> :kind! Eval (Second ((+) 1) '("a",3))
-- Eval (Second ((+) 1) '("a",3)) :: (TL.Symbol, Nat)
-- = '("a", 4)
data Second :: (c -> Exp d) -> f a c -> Exp (f a d)

-- (,)
type instance Eval (Second f '(a,b)) = '(a, f @@ b)

-- Either
type instance Eval (Second f ('Left a)) = 'Left a
type instance Eval (Second f ('Right b)) = 'Right (f @@ b)

