{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

{-|
Module      : Fcf.Data.Set
Description : Set data-structure for type-level programming
Copyright   : (c) gspia 2020-
License     : BSD
Maintainer  : gspia

= Fcf.Data.Set

Set provides an interface which is similar to the that given by the
container-package. If a method is missing here that you need, please do open
up an issue or better, make a PR.

Many of the examples are from containers-package. The internal representation
is based on lists. We hope that a more efficient one will be used one day.

-}


--------------------------------------------------------------------------------

module Fcf.Data.Set where

import qualified GHC.TypeLits as TL

import           Fcf ( Eval, Exp, type (=<<), type (<=<)
                     , Not, If
                     , TyEq, Length)
-- import           Fcf.Data.List as Fcf
import qualified Fcf as Fcf (Foldr, Filter)
import           Fcf.Data.List (Elem)

--------------------------------------------------------------------------------

-- For the doctests:

-- $setup
-- >>> import qualified GHC.TypeLits as TL
-- >>> import           Fcf.Data.Nat

--------------------------------------------------------------------------------

-- | Set-definition.
data Set a = Set [a]

-- | Empty
-- 
-- __Example__
-- 
-- >>> :kind! (Eval Empty :: Set Nat)
-- (Eval Empty :: Set Nat) :: Set Nat
-- = 'Set '[]
--
-- See also the other examples in this module.
data Empty :: Exp (Set v)
type instance Eval Empty = 'Set '[]

-- | Singleton
-- 
-- __Example__
-- 
-- >>> :kind! Eval (Singleton 1)
-- Eval (Singleton 1) :: Set Nat
-- = 'Set '[1]
data Singleton :: v -> Exp (Set v)
type instance Eval (Singleton v) = 'Set '[v]

-- | Use FromList to construct a Set from type-level list.
--
-- __Example__
-- 
-- >>> :kind! Eval (FromList '[1, 2])
-- Eval (FromList '[1, 2]) :: Set Nat
-- = 'Set '[1, 2]
data FromList :: [v] -> Exp (Set v)
type instance Eval (FromList lst) = 'Set lst


-- | Insert
--
-- __Example__
--
-- >>> :kind! Eval (Insert 3 =<< FromList '[1, 2])
-- Eval (Insert 3 =<< FromList '[1, 2]) :: Set Nat
-- = 'Set '[3, 1, 2]
--
-- >>> :kind! Eval (Insert 2 =<< FromList '[1, 2])
-- Eval (Insert 2 =<< FromList '[1, 2]) :: Set Nat
-- = 'Set '[1, 2]
data Insert :: v -> Set v -> Exp (Set v)
type instance Eval (Insert v ('Set lst)) =
    If (Eval (Elem v lst))
        ('Set lst)
        ('Set (v ': lst))

-- | Delete
-- 
-- __Example__
-- 
-- >>> :kind! Eval (Delete 5 =<< FromList '[5, 3])
-- Eval (Delete 5 =<< FromList '[5, 3]) :: Set Nat
-- = 'Set '[3]
--
-- >>> :kind! Eval (Delete 7 =<< FromList '[5, 3])
-- Eval (Delete 7 =<< FromList '[5, 3]) :: Set Nat
-- = 'Set '[5, 3]
data Delete :: v -> Set v -> Exp (Set v)
type instance Eval (Delete v ('Set lst)) =
    'Set (Eval (Fcf.Filter (Not <=< TyEq v) lst))

-- | Member
--
-- __Example__
-- 
-- >>> :kind! Eval (Member 5 =<< FromList '[5, 3])
-- Eval (Member 5 =<< FromList '[5, 3]) :: Bool
-- = 'True
-- >>> :kind! Eval (Member 7 =<< FromList '[5, 3])
-- Eval (Member 7 =<< FromList '[5, 3]) :: Bool
-- = 'False
data Member :: v -> Set v -> Exp Bool
type instance Eval (Member v ('Set lst)) = Eval (Elem v lst)

-- | NotMember
--
-- __Example__
-- 
-- >>> :kind! Eval (NotMember 5 =<< FromList '[5, 3])
-- Eval (NotMember 5 =<< FromList '[5, 3]) :: Bool
-- = 'False
-- >>> :kind! Eval (NotMember 7 =<< FromList '[5, 3])
-- Eval (NotMember 7 =<< FromList '[5, 3]) :: Bool
-- = 'True
data NotMember :: v -> Set v -> Exp Bool
type instance Eval (NotMember k ('Set lst)) =
    Eval (Not =<< Elem k lst)

-- | Null
--
-- __Example__
-- 
-- >>> :kind! Eval (Null =<< FromList '[5, 3])
-- Eval (Null =<< FromList '[5, 3]) :: Bool
-- = 'False
-- >>> :kind! Eval (Null =<< Empty)
-- Eval (Null =<< Empty) :: Bool
-- = 'True
data Null :: Set v -> Exp Bool
type instance Eval (Null ('Set '[])) = 'True
type instance Eval (Null ('Set (_ ': _))) = 'False

-- | Size
--
-- __Example__
-- 
-- >>> :kind! Eval (Size =<< FromList '[5, 3])
-- Eval (Size =<< FromList '[5, 3]) :: Nat
-- = 2
data Size :: Set v -> Exp TL.Nat
type instance Eval (Size ('Set lst)) = Eval (Length lst)

-- | Union
--
-- __Example__
-- 
-- >>> :kind! Eval (Union (Eval (FromList '[5, 3])) (Eval (FromList '[5, 7])) )
-- Eval (Union (Eval (FromList '[5, 3])) (Eval (FromList '[5, 7])) ) :: Set 
--                                                                        Nat
-- = 'Set '[7, 5, 3]
data Union :: Set v -> Set v -> Exp (Set v)
type instance Eval (Union ('Set lst1) ('Set lst2)) =
    'Set (Eval (Fcf.Foldr UComb lst1 lst2))

data UComb :: v -> [v] -> Exp [v]
type instance Eval (UComb v lst) =
    If (Eval (Elem v lst))
        lst
        (v ': lst)


-- | Difference
-- 
-- __Example__
-- 
-- >>> :kind! Eval (Difference (Eval (FromList '[3, 5])) (Eval (FromList '[5, 7])))
-- Eval (Difference (Eval (FromList '[3, 5])) (Eval (FromList '[5, 7]))) :: Set 
--                                                                            Nat
-- = 'Set '[3]
data Difference :: Set v -> Set v -> Exp (Set v)
type instance Eval (Difference ('Set lst1) ('Set lst2)) =
    'Set (Eval (Fcf.Filter (DiffNotMem lst2) lst1))

-- helper
data DiffNotMem :: [v] -> v -> Exp Bool
type instance Eval (DiffNotMem lst v) = Eval (Not =<< Elem v lst)


-- | Intersection
--
-- __Example__
-- 
-- >>> :kind! Eval (Intersection (Eval (FromList '[3, 5])) (Eval (FromList '[5, 7])))
-- Eval (Intersection (Eval (FromList '[3, 5])) (Eval (FromList '[5, 7]))) :: Set 
--                                                                              Nat
-- = 'Set '[5]
data Intersection :: Set v -> Set v -> Exp (Set v)
type instance Eval (Intersection ('Set lst1) ('Set lst2)) =
    'Set (Eval (Fcf.Filter (InterMem lst2) lst1))

-- helper
data InterMem :: [v ]-> v -> Exp Bool
type instance Eval (InterMem lst v) = Eval (Elem v lst)
