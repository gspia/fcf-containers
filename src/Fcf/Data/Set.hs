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
containers-package. If a method is missing here that you need, please do open
up an issue or better, make a PR.

Many of the examples are from containers-package. The internal representation
is based on lists. We hope that a more efficient one will be used one day.

-}


--------------------------------------------------------------------------------

module Fcf.Data.Set
    ( Set (..)

    -- * Query
    , Member
    , NotMember
    , Null
    , Size
    , IsSubsetOf
    , IsProperSubsetOf
      
    -- * Construction
    , Empty
    , Singleton
    , Insert
    , Delete
    , PowerSet

    -- * Combine
    , Union
    , Difference
    , Intersection

    -- * List
    , FromList
    , ToList
    )
  where

--------------------------------------------------------------------------------

import qualified GHC.TypeLits as TL

import           Fcf ( Eval, Exp, type (=<<), type (<=<), type (&&)
                     , Not, If, Map, Flip, TyEq )
import qualified Fcf (Foldr, Filter)
import           Fcf.Class.Foldable (All, Any)
import           Fcf.Data.List ( Elem, Cons, Concat, Reverse, Length, type (++)
                               , ZipWith, Replicate)

--------------------------------------------------------------------------------

import           Fcf.Alg.List ( ListF(..), ListToFix)
import           Fcf.Alg.Morphism

--------------------------------------------------------------------------------

-- | Set-definition.
newtype Set a = Set [a]


--------------------------------------------------------------------------------

-- | Empty
--
-- === __Example__
--
-- > :kind! (Eval Empty :: Set Nat)
-- (Eval Empty :: Set Nat) :: Set TL.Natural
-- = 'Set '[]
--
-- See also the other examples in this module.
data Empty :: Exp (Set v)
type instance Eval Empty = 'Set '[]

-- | Singleton
--
-- === __Example__
--
-- > :kind! Eval (Singleton 1)
-- Eval (Singleton 1) :: Set TL.Natural
-- = 'Set '[1]
data Singleton :: v -> Exp (Set v)
type instance Eval (Singleton v) = 'Set '[v]


-- | Insert
--
-- === __Example__
--
-- > :kind! Eval (Insert 3 =<< FromList '[1, 2])
-- Eval (Insert 3 =<< FromList '[1, 2]) :: Set TL.Natural
-- = 'Set '[3, 1, 2]
--
-- > :kind! Eval (Insert 2 =<< FromList '[1, 2])
-- Eval (Insert 2 =<< FromList '[1, 2]) :: Set TL.Natural
-- = 'Set '[1, 2]
data Insert :: v -> Set v -> Exp (Set v)
type instance Eval (Insert v ('Set lst)) =
    If (Eval (Elem v lst))
        ('Set lst)
        ('Set (v ': lst))

-- | Delete
--
-- === __Example__
--
-- > :kind! Eval (Delete 5 =<< FromList '[5, 3])
-- Eval (Delete 5 =<< FromList '[5, 3]) :: Set TL.Natural
-- = 'Set '[3]
--
-- > :kind! Eval (Delete 7 =<< FromList '[5, 3])
-- Eval (Delete 7 =<< FromList '[5, 3]) :: Set TL.Natural
-- = 'Set '[5, 3]
data Delete :: v -> Set v -> Exp (Set v)
type instance Eval (Delete v ('Set lst)) =
    'Set (Eval (Fcf.Filter (Not <=< TyEq v) lst))

-- | Member
--
-- === __Example__
--
-- > :kind! Eval (Member 5 =<< FromList '[5, 3])
-- Eval (Member 5 =<< FromList '[5, 3]) :: Bool
-- = 'True
--
-- > :kind! Eval (Member 7 =<< FromList '[5, 3])
-- Eval (Member 7 =<< FromList '[5, 3]) :: Bool
-- = 'False
data Member :: v -> Set v -> Exp Bool
type instance Eval (Member v ('Set lst)) = Eval (Elem v lst)

-- | NotMember
--
-- === __Example__
--
-- > :kind! Eval (NotMember 5 =<< FromList '[5, 3])
-- Eval (NotMember 5 =<< FromList '[5, 3]) :: Bool
-- = 'False
--
-- > :kind! Eval (NotMember 7 =<< FromList '[5, 3])
-- Eval (NotMember 7 =<< FromList '[5, 3]) :: Bool
-- = 'True
data NotMember :: v -> Set v -> Exp Bool
type instance Eval (NotMember k ('Set lst)) =
    Eval (Not =<< Elem k lst)


-- | Null
--
-- === __Example__
--
-- > :kind! Eval (Null =<< FromList '[5, 3])
-- Eval (Null =<< FromList '[5, 3]) :: Bool
-- = 'False
-- 
-- > :kind! Eval (Null =<< Empty)
-- Eval (Null =<< Empty) :: Bool
-- = 'True
data Null :: Set v -> Exp Bool
type instance Eval (Null ('Set '[])) = 'True
type instance Eval (Null ('Set (_ ': _))) = 'False


-- | Size
--
-- === __Example__
--
-- > :kind! Eval (Size =<< FromList '[5, 3])
-- Eval (Size =<< FromList '[5, 3]) :: TL.Natural
-- = 2
data Size :: Set v -> Exp TL.Nat
type instance Eval (Size ('Set lst)) = Eval (Length lst)


-- | IsSubsetOf
--
-- === __Example__
--
-- > :kind! Eval (IsSubsetOf ('Set '[]) ('Set '[0,1,2,3,4]))
-- Eval (IsSubsetOf ('Set '[]) ('Set '[0,1,2,3,4])) :: Bool
-- = 'True
--
-- > :kind! Eval (IsSubsetOf ('Set '[0,1]) ('Set '[0,1,2,3,4]))
-- Eval (IsSubsetOf ('Set '[0,1]) ('Set '[0,1,2,3,4])) :: Bool
-- = 'True
--
-- > :kind! Eval (IsSubsetOf ('Set '[0,2,1,3,4]) ('Set '[0,1,2,3,4]))
-- Eval (IsSubsetOf ('Set '[0,2,1,3,4]) ('Set '[0,1,2,3,4])) :: Bool
-- = 'True
--
-- > :kind! Eval (IsSubsetOf ('Set '[0,1,2,3,4,5]) ('Set '[0,1,2,3,4]))
-- Eval (IsSubsetOf ('Set '[0,1,2,3,4,5]) ('Set '[0,1,2,3,4])) :: Bool
-- = 'False
data IsSubsetOf :: Set a -> Set a -> Exp Bool
type instance Eval (IsSubsetOf ('Set s1) ('Set s2)) =
    Eval (All (Flip Elem s2) s1)


-- | IsProperSubsetOf
--
-- === __Example__
--
-- > :kind! Eval (IsProperSubsetOf ('Set '[0,1,2,3,4]) ('Set '[0,1,2,3,4]))
-- Eval (IsProperSubsetOf ('Set '[0,1,2,3,4]) ('Set '[0,1,2,3,4])) :: Bool
-- = 'False
--
-- > :kind! Eval (IsProperSubsetOf ('Set '[0,2,1,3]) ('Set '[0,1,2,3,4]))
-- Eval (IsProperSubsetOf ('Set '[0,2,1,3]) ('Set '[0,1,2,3,4])) :: Bool
-- = 'True
data IsProperSubsetOf :: Set a -> Set a -> Exp Bool
type instance Eval (IsProperSubsetOf ('Set s1) ('Set s2)) = Eval
    (Eval (All (Flip Elem s2) s1) &&
    Eval (Any (Not <=< Flip Elem s1) s2))



-- | Type-level set union.
--
-- === __Example__
--
-- > :kind! Eval (Union (Eval (FromList '[5, 3])) (Eval (FromList '[5, 7])) )
-- Eval (Union (Eval (FromList '[5, 3])) (Eval (FromList '[5, 7])) ) :: Set
--                                                                        TL.Natural
-- = 'Set '[7, 5, 3]
data Union :: Set v -> Set v -> Exp (Set v)
type instance Eval (Union ('Set lst1) ('Set lst2)) =
    'Set (Eval (Fcf.Foldr UComb lst1 lst2))

data UComb :: v -> [v] -> Exp [v]
type instance Eval (UComb v lst) =
    If (Eval (Elem v lst))
        lst
        (v ': lst)


-- | Type-level set difference.
--
-- === __Example__
--
-- > :kind! Eval (Difference (Eval (FromList '[3, 5])) (Eval (FromList '[5, 7])))
-- Eval (Difference (Eval (FromList '[3, 5])) (Eval (FromList '[5, 7]))) :: Set
--                                                                            TL.Natural
-- = 'Set '[3]
data Difference :: Set v -> Set v -> Exp (Set v)
type instance Eval (Difference ('Set lst1) ('Set lst2)) =
    'Set (Eval (Fcf.Filter (DiffNotMem lst2) lst1))

-- helper
data DiffNotMem :: [v] -> v -> Exp Bool
type instance Eval (DiffNotMem lst v) = Eval (Not =<< Elem v lst)


-- | Type-level set intersection.
--
-- === __Example__
--
-- > :kind! Eval (Intersection (Eval (FromList '[3, 5])) (Eval (FromList '[5, 7])))
-- Eval (Intersection (Eval (FromList '[3, 5])) (Eval (FromList '[5, 7]))) :: Set
--                                                                              TL.Natural
-- = 'Set '[5]
data Intersection :: Set v -> Set v -> Exp (Set v)
type instance Eval (Intersection ('Set lst1) ('Set lst2)) =
    'Set (Eval (Fcf.Filter (InterMem lst2) lst1))

-- helper
data InterMem :: [v ]-> v -> Exp Bool
type instance Eval (InterMem lst v) = Eval (Elem v lst)

--------------------------------------------------------------------------------

-- helper for PowerSet
data AddFalses :: [[Bool]] -> Exp [[Bool]]
type instance Eval (AddFalses lst) = Eval (Map (Cons 'False) lst)

-- helper for PowerSet
data AddTrues :: [[Bool]] -> Exp [[Bool]]
type instance Eval (AddTrues lst) = Eval (Map (Cons 'True) lst)

-- helper for PowerSet
data ExtendBitList :: [[Bool]] -> Exp [[Bool]]
type instance Eval (ExtendBitList lst) =
    Eval (Eval (AddFalses lst) ++ Eval (AddTrues =<< Map Reverse lst))

-- helper for PowerSet
-- :kind! Eval (Cata BuildGrayBitListsAlg =<< ListToFix =<< Replicate 3 '[])
data BuildGrayBitListsAlg :: Algebra (ListF [[Bool]]) [[Bool]]
type instance Eval (BuildGrayBitListsAlg 'NilF) = '[ '[] ]
type instance Eval (BuildGrayBitListsAlg ('ConsF _ lst)) = Eval (ExtendBitList lst)

-- helper for PowerSet
-- :kind! Eval (BuildGrayBitLists '[1,2,3,4])
data BuildGrayBitLists :: [a] -> Exp [[Bool]]
type instance Eval (BuildGrayBitLists lst) =
    Eval (Cata BuildGrayBitListsAlg =<< ListToFix =<< Replicate (Eval (Length lst)) '[])

-- helper for PowerSet
data SelWithBool :: a -> Bool -> Exp [a]
type instance Eval (SelWithBool a b) = If b '[a] '[]

-- helper for PowerSet
data SelectWithBools :: [a] -> [Bool] -> Exp [a]
type instance Eval (SelectWithBools elms bls) =
    Eval (Concat =<< ZipWith SelWithBool elms bls)

-- | Calculate the power sets of a given type-level list. The algorithm is based
-- on Gray codes.
--
-- === __Example__
--
-- > :kind! Eval (PowerSet =<< FromList '["a", "b", "c"])
-- Eval (PowerSet =<< FromList '["a", "b", "c"]) :: Set
--                                                    (Set TL.Symbol)
-- = 'Set
--     '[ 'Set '[], 'Set '["c"], 'Set '["b"], 'Set '["b", "c"],
--        'Set '["a"], 'Set '["a", "b"], 'Set '["a", "c"],
--        'Set '["a", "b", "c"]]
--
-- > :kind! Eval (PowerSet =<< FromList '[Int, Char, Maybe Int])
-- Eval (PowerSet =<< FromList '[Int, Char, Maybe Int]) :: Set
--                                                           (Set (*))
-- = 'Set
--     '[ 'Set '[], 'Set '[Maybe Int], 'Set '[Char],
--        'Set '[Char, Maybe Int], 'Set '[Int], 'Set '[Int, Char],
--        'Set '[Int, Maybe Int], 'Set '[Int, Char, Maybe Int]]
--
data PowerSet :: Set a -> Exp (Set (Set a))
type instance Eval (PowerSet ('Set lst)) =
    'Set (Eval (Map FromList
               =<< Map (SelectWithBools lst)
               =<< BuildGrayBitLists lst))

--------------------------------------------------------------------------------

-- | Use FromList to construct a Set from type-level list.
--
-- === __Example__
--
-- > :kind! Eval (FromList '[1, 2])
-- Eval (FromList '[1, 2]) :: Set TL.Natural
-- = 'Set '[1, 2]
data FromList :: [v] -> Exp (Set v)
type instance Eval (FromList lst) = 'Set lst

-- | Get the type-level list out of the 'Set'.
--
-- === __Example__
--
-- > :kind! Eval (ToList =<< PowerSet =<< FromList '[1,2,3])
-- Eval (ToList =<< PowerSet =<< FromList '[1,2,3]) :: [Set
--                                                        TL.Natural]
-- = '[ 'Set '[], 'Set '[3], 'Set '[2], 'Set '[2, 3], 'Set '[1],
--      'Set '[1, 2], 'Set '[1, 3], 'Set '[1, 2, 3]]
--
-- > :kind! Eval (Qsort NatListOrd =<< Map ToList =<< ToList =<< PowerSet =<< FromList '[1,2,3])
-- Eval (Qsort NatListOrd =<< Map ToList =<< ToList =<< PowerSet =<< FromList '[1,2,3]) :: [[TL.Natural]]
-- = '[ '[], '[1], '[1, 2], '[1, 2, 3], '[1, 3], '[2], '[2, 3], '[3]]
data ToList :: Set v -> Exp [v]
type instance Eval (ToList ('Set lst)) = lst
