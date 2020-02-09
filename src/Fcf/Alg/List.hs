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

import           Fcf.Core (Eval, Exp, type (@@))
import           Fcf.Classes (Map)
import           Fcf.Combinators (type (=<<), type (<=<))
import           Fcf.Data.List (Foldr, Concat, TakeWhile, DropWhile, Reverse
                               , type (++), ZipWith)
import           Fcf.Utils (If, TyEq)
import           Fcf.Data.Bool (type (&&), type  (||), Not)
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

-- | Intersperse for type-level lists.
--
-- === __Example__
-- 
-- >>> :kind! Eval (Intersperse 0 '[1,2,3,4])
-- Eval (Intersperse 0 '[1,2,3,4]) :: [Nat]
-- = '[1, 0, 2, 0, 3, 0, 4]
data Intersperse :: a -> [a] -> Exp [a]
type instance Eval (Intersperse _   '[]      ) = '[]
type instance Eval (Intersperse sep (x ': xs)) = x ': Eval (PrependToAll sep xs)

-- helper for Intersperse
data PrependToAll :: a -> [a] -> Exp [a]
type instance Eval (PrependToAll _   '[]      ) = '[]
type instance Eval (PrependToAll sep (x ': xs)) = sep ': x ': Eval (PrependToAll sep xs)

-- | Intercalate for type-level lists.
-- 
-- === __Example__
-- 
-- >>> :kind! Eval (Intercalate '[", "] '[ '["Lorem"], '["ipsum"], '["dolor"] ])
-- Eval (Intercalate '[", "] '[ '["Lorem"], '["ipsum"], '["dolor"] ]) :: [TL.Symbol]
-- = '["Lorem", ", ", "ipsum", ", ", "dolor"]
data Intercalate :: [a] -> [[a]] -> Exp [a]
type instance Eval (Intercalate xs xss) = Eval (Concat =<< Intersperse xs xss)


-- | 'Span', applied to a predicate @p@ and a type-level list @xs@, returns a 
-- type-level tuple where
-- first element is longest prefix (possibly empty) of @xs@ of elements that
-- satisfy @p@ and second element is the remainder of the list:
--
-- === __Example__
-- 
-- >>> :kind! Eval (Span (Flip (<) 3) '[1,2,3,4,1,2,3,4])
-- Eval (Span (Flip (<) 3) '[1,2,3,4,1,2,3,4]) :: ([Nat], [Nat])
-- = '( '[1, 2], '[3, 4, 1, 2, 3, 4])
--
-- >>> :kind! Eval (Span (Flip (<) 9) '[1,2,3])
-- Eval (Span (Flip (<) 9) '[1,2,3]) :: ([Nat], [Nat])
-- = '( '[1, 2, 3], '[])
--
-- >>> :kind! Eval (Span (Flip (<) 0) '[1,2,3])
-- Eval (Span (Flip (<) 0) '[1,2,3]) :: ([Nat], [Nat])
-- = '( '[], '[1, 2, 3])
data Span :: (a -> Exp Bool) -> [a] -> Exp ([a],[a])
type instance Eval (Span p lst) = '( Eval (TakeWhile p lst), Eval (DropWhile p lst))


-- | 'Break', applied to a predicate @p@ and a type-level list @xs@, returns a 
-- type-level tuple where
-- first element is longest prefix (possibly empty) of @xs@ of elements that
-- /do not satisfy/ @p@ and second element is the remainder of the list:
--
-- === __Example__
-- 
-- >>> :kind! Eval (Break (Flip (>) 3) '[1,2,3,4,1,2,3,4])
-- Eval (Break (Flip (>) 3) '[1,2,3,4,1,2,3,4]) :: ([Nat], [Nat])
-- = '( '[1, 2, 3], '[4, 1, 2, 3, 4])
--
-- >>> :kind! Eval (Break (Flip (<) 9) '[1,2,3])
-- Eval (Break (Flip (<) 9) '[1,2,3]) :: ([Nat], [Nat])
-- = '( '[], '[1, 2, 3])
--
-- >>> :kind! Eval (Break (Flip (>) 9) '[1,2,3])
-- Eval (Break (Flip (>) 9) '[1,2,3]) :: ([Nat], [Nat])
-- = '( '[1, 2, 3], '[])
data Break :: (a -> Exp Bool) -> [a] -> Exp ([a],[a])
type instance Eval (Break p lst) = Eval (Span (Not <=< p) lst)


-- | IsPrefixOf takes two type-level lists and returns true
-- iff the first list is a prefix of the second.
--
-- === __Example__
-- 
-- >>> :kind! Eval (IsPrefixOf '[0,1,2] '[0,1,2,3,4,5])
-- Eval (IsPrefixOf '[0,1,2] '[0,1,2,3,4,5]) :: Bool
-- = 'True
--
-- >>> :kind! Eval (IsPrefixOf '[0,1,2] '[0,1,3,2,4,5])
-- Eval (IsPrefixOf '[0,1,2] '[0,1,3,2,4,5]) :: Bool
-- = 'False
--
-- >>> :kind! Eval (IsPrefixOf '[] '[0,1,3,2,4,5])
-- Eval (IsPrefixOf '[] '[0,1,3,2,4,5]) :: Bool
-- = 'True
--
-- >>> :kind! Eval (IsPrefixOf '[0,1,3,2,4,5] '[])
-- Eval (IsPrefixOf '[0,1,3,2,4,5] '[]) :: Bool
-- = 'False
data IsPrefixOf :: [a] -> [a] -> Exp Bool
type instance Eval (IsPrefixOf xs ys) = IsPrefixOf_ xs ys

-- helper for IsPrefixOf
type family IsPrefixOf_ (xs :: [a]) (ys :: [a]) :: Bool where
    IsPrefixOf_ '[] _ = 'True
    IsPrefixOf_ _ '[] = 'False
    IsPrefixOf_ (x ': xs) (y ': ys) =
         Eval ((Eval (TyEq x y)) && IsPrefixOf_ xs ys)


-- | IsSuffixOf take two type-level lists and returns true
-- iff the first list is a suffix of the second.
--
-- === __Example__
-- 
-- >>> :kind! Eval (IsSuffixOf '[3,4,5] '[0,1,2,3,4,5])
-- Eval (IsSuffixOf '[3,4,5] '[0,1,2,3,4,5]) :: Bool
-- = 'True
--
-- >>> :kind! Eval (IsSuffixOf '[3,4,5] '[0,1,3,2,4,5])
-- Eval (IsSuffixOf '[3,4,5] '[0,1,3,2,4,5]) :: Bool
-- = 'False
--
-- >>> :kind! Eval (IsSuffixOf '[] '[0,1,3,2,4,5])
-- Eval (IsSuffixOf '[] '[0,1,3,2,4,5]) :: Bool
-- = 'True
--
-- >>> :kind! Eval (IsSuffixOf '[0,1,3,2,4,5] '[])
-- Eval (IsSuffixOf '[0,1,3,2,4,5] '[]) :: Bool
-- = 'False
data IsSuffixOf :: [a] -> [a] -> Exp Bool
type instance Eval (IsSuffixOf xs ys) =
    Eval (IsPrefixOf (Reverse @@ xs) (Reverse @@ ys))


-- | IsInfixOf take two type-level lists and returns true
-- iff the first list is a infix of the second.
--
-- === __Example__
-- 
-- >>> :kind! Eval (IsInfixOf '[2,3,4]  '[0,1,2,3,4,5,6])
-- Eval (IsInfixOf '[2,3,4]  '[0,1,2,3,4,5,6]) :: Bool
-- = 'True
--
-- >>> :kind! Eval (IsInfixOf '[2,4,4]  '[0,1,2,3,4,5,6])
-- Eval (IsInfixOf '[2,4,4]  '[0,1,2,3,4,5,6]) :: Bool
-- = 'False
data IsInfixOf :: [a] -> [a] -> Exp Bool
type instance Eval (IsInfixOf xs ys) = Eval (Any (IsPrefixOf xs) =<< Tails ys)


-- | Tails
-- 
-- === __Example__
-- 
-- >>> :kind! Eval (Tails '[0,1,2,3])
-- Eval (Tails '[0,1,2,3]) :: [[Nat]]
-- = '[ '[0, 1, 2, 3], '[1, 2, 3], '[2, 3], '[3]]
data Tails :: [a] -> Exp [[a]]
type instance Eval (Tails '[]) = '[]
type instance Eval (Tails (a ': as)) = (a ': as) ': Eval (Tails as)

--------------------------------------------------------------------------------


-- | Give true if all of the booleans in the list are true.
--
-- === __Example__
-- 
-- >>> :kind! Eval (And '[ 'True, 'True])
-- Eval (And '[ 'True, 'True]) :: Bool
-- = 'True
--
-- >>> :kind! Eval (And '[ 'True, 'True, 'False])
-- Eval (And '[ 'True, 'True, 'False]) :: Bool
-- = 'False
data And :: [Bool] -> Exp Bool
type instance Eval (And lst) = Eval (Foldr (&&) 'True lst)


-- | Type-level All.
--
-- === __Example__
--
-- >>> :kind! Eval (All (Flip (<) 6) '[0,1,2,3,4,5])
-- Eval (All (Flip (<) 6) '[0,1,2,3,4,5]) :: Bool
-- = 'True
--
-- >>> :kind! Eval (All (Flip (<) 5) '[0,1,2,3,4,5])
-- Eval (All (Flip (<) 5) '[0,1,2,3,4,5]) :: Bool
-- = 'False
data All :: (a -> Exp Bool) -> [a] -> Exp Bool
type instance Eval (All p lst) = Eval (And =<< Map p lst)


-- | Give true if any of the booleans in the list is true.
--
-- === __Example__
-- 
-- >>> :kind! Eval (Or '[ 'True, 'True])
-- Eval (Or '[ 'True, 'True]) :: Bool
-- = 'True
-- 
-- >>> :kind! Eval (Or '[ 'False, 'False])
-- Eval (Or '[ 'False, 'False]) :: Bool
-- = 'False
data Or :: [Bool] -> Exp Bool
type instance Eval (Or lst) = Eval (Foldr (||) 'False lst)


-- | Type-level Any.
--
-- === __Example__
-- 
-- >>> :kind! Eval (Any (Flip (<) 5) '[0,1,2,3,4,5])
-- Eval (Any (Flip (<) 5) '[0,1,2,3,4,5]) :: Bool
-- = 'True
--
-- >>> :kind! Eval (Any (Flip (<) 0) '[0,1,2,3,4,5])
-- Eval (Any (Flip (<) 0) '[0,1,2,3,4,5]) :: Bool
-- = 'False
data Any :: (a -> Exp Bool) -> [a] -> Exp Bool
type instance Eval (Any p lst) = Eval (Or =<< Map p lst)

--------------------------------------------------------------------------------

-- | Snoc for type-level lists.
--
-- === __Example__
-- 
-- >>> :kind! Eval (Snoc '[1,2,3] 4)
-- Eval (Snoc '[1,2,3] 4) :: [Nat]
-- = '[1, 2, 3, 4]
data Snoc :: [a] -> a -> Exp [a]
type instance Eval (Snoc lst a) = Eval (lst ++ '[a])

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


