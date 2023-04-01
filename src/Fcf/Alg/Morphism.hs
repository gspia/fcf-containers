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
newtype Fix f = Fix (f (Fix f))

-- | Commonly used name describing the method 'Cata' eats.
type Algebra f a = f a -> Exp a

-- | Commonly used name describing the method 'Ana' eats.
type CoAlgebra f a = a -> Exp (f a)

-- | Commonly used name describing the method 'Para' eats.
type RAlgebra f a = f (Fix f, a) -> Exp a

--------------------------------------------------------------------------------


-- | Write the function to give a 'Fix', and feed it in together with an
-- 'Algebra'.
--
-- Check Fcf.Alg.List to see example algebras in use. There we have e.g.
-- ListToFix-function.
data Cata :: Algebra f a -> Fix f -> Exp a
type instance Eval (Cata alg ('Fix b)) = alg @@ Eval (Map (Cata alg) b)

-- | Ana can also be used to build a 'Fix' structure.
--
-- === __Example__
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
-- Eval (Ana NToOneCoA 3) :: Fix (ListF TL.Natural)
-- = 'Fix ('ConsF 3 ('Fix ('ConsF 2 ('Fix ('ConsF 1 ('Fix 'NilF))))))
data Ana :: CoAlgebra f a -> a -> Exp (Fix f)
type instance Eval (Ana coalg a) = 'Fix (Eval (Map (Ana coalg) (Eval (coalg a))))


-- |
-- Hylomorphism uses first 'Ana' to build a structure (unfold) and then 'Cata'
-- to process the structure (fold).
--
-- === __Example__
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
-- Eval (Hylo SumAlg NToOneCoA 5) :: TL.Natural
-- = 15
data Hylo :: Algebra f a -> CoAlgebra f b -> b -> Exp a
type instance Eval (Hylo alg coalg a) = Eval (Cata alg =<< Ana coalg a)


-- Helper for Para so that we can do fmap
data Fanout :: RAlgebra f a -> Fix f -> Exp ( Fix f, a)
type instance Eval (Fanout ralg ('Fix f)) = '( 'Fix f, Eval (Para ralg ('Fix f)))


-- | Write a function to give a 'Fix', and feed it in together with an
-- 'RAlgebra'
--
-- Check Fcf.Alg.List to see example algebras in use. There we have e.g.
-- ListToParaFix-function.
data Para :: RAlgebra f a -> Fix f -> Exp a
type instance Eval (Para ralg ('Fix  a)) =  ralg @@ Eval (Map (Fanout ralg) a)

--------------------------------------------------------------------------------

-- | Annotate (f r) with attribute a
-- (from Recursion Schemes by example, Tim Williams).
newtype AnnF f a r = AnnF (f r, a)

-- | Annotated fixed-point type. A cofree comonad
-- (from Recursion Schemes by example, Tim Williams).
type Ann f a = Fix (AnnF f a)

-- | Attribute of the root node
-- (from Recursion Schemes by example, Tim Williams).
data Attr :: Ann f a -> Exp a
type instance Eval (Attr ('Fix ( 'AnnF '(_, a)))) = a

-- | Strip attribute from root
-- (from Recursion Schemes by example, Tim Williams).
data Strip :: Ann f a -> Exp (f (Ann f a))
type instance Eval (Strip ('Fix ( 'AnnF '(x,_)))) = x

-- | Annotation constructor
-- (from Recursion Schemes by example, Tim Williams).
data AnnConstr :: (f (Ann f a), a) -> Exp (Fix (AnnF f a))
type instance Eval (AnnConstr fxp) = Eval (Pure ('Fix ('AnnF fxp)))

-- | Synthesized attributes are created in a bottom-up traversal
-- using a catamorphism
-- (from Recursion Schemes by example, Tim Williams).
--
-- This is the algebra that is fed to the cata.
data SynthAlg :: (f a -> Exp a) -> f (Ann f a) -> Exp (Ann f a)
type instance Eval (SynthAlg alg faf) =
    Eval (AnnConstr '(faf, Eval (alg  =<< Map Attr faf)))

-- | Synthesized attributes are created in a bottom-up traversal
-- using a catamorphism
-- (from Recursion Schemes by example, Tim Williams).
--
-- For the example, see "Fcf.Data.Alg.Tree.Sizes".
data Synthesize :: (f a -> Exp a) -> Fix f -> Exp (Ann f a)
type instance Eval (Synthesize f fx) = Eval (Cata (SynthAlg f) fx)

--------------------------------------------------------------------------------

-- | Histo takes annotation algebra and takes a Fix-structure
-- (from Recursion Schemes by example, Tim Williams).
--
-- This is a helper for 'Histo' as it is implemented with 'Cata'.
data HistoAlg :: (f (Ann f a) -> Exp a) -> f (Ann f a) -> Exp (Ann f a)
type instance Eval (HistoAlg alg faf) =
    Eval (AnnConstr '(faf, Eval (alg faf)))

-- | Histo takes annotation algebra and takes a Fix-structure
-- (from Recursion Schemes by example, Tim Williams).
--
-- Examples can be found from "Fcf.Data.Alg.Tree" and "Fcf.Data.Alg.List"
-- modules.
data Histo :: (f (Ann f a) -> Exp a) -> Fix f -> Exp a
type instance Eval (Histo alg fx) = Eval (Attr =<< Cata (HistoAlg alg) fx)

--------------------------------------------------------------------------------

-- | Type-level First. Tuples @(,)@ and @Either@ have First-instances.
--
-- === __Example__
--
-- >>> :kind! Eval (First ((+) 1) '(3,"a"))
-- Eval (First ((+) 1) '(3,"a")) :: (TL.Natural, TL.Symbol)
-- = '(4, "a")
data First :: (a -> Exp b) -> f a c -> Exp (f b c)

-- (,)
type instance Eval (First f '(a,b)) = '(f @@ a, b)

-- Either
type instance Eval (First f ('Left a)) = 'Left (f @@ a)
type instance Eval (First f ('Right b)) = 'Right b

-- | Type-level Second. Tuples @(,)@ and @Either@ have Second-instances.
--
-- === __Example__
--
-- >>> :kind! Eval (Second ((+) 1) '("a",3))
-- Eval (Second ((+) 1) '("a",3)) :: (TL.Symbol, TL.Natural)
-- = '("a", 4)
data Second :: (c -> Exp d) -> f a c -> Exp (f a d)

-- (,)
type instance Eval (Second f '(a,b)) = '(a, f @@ b)

-- Either
type instance Eval (Second f ('Left a)) = 'Left a
type instance Eval (Second f ('Right b)) = 'Right (f @@ b)

