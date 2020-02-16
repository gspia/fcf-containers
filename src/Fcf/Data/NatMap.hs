{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

{-|
Module      : Fcf.Data.NatMap
Description : NatMap data-type for the type-level programming (like IntMap)
Copyright   : (c) gspia 2020-
License     : BSD
Maintainer  : gspia

= Fcf.Data.NatNatMap

NatMap provides an interface to mapping keys (Nat's) to values, which is
similar to
NatIntMap given by the containers-package. Note that the this module still misses
some of the methods that can be found in containers. If you need some, please
do open up an issue or better, make a PR.

Many of the examples are from containers-package.

-}

--------------------------------------------------------------------------------

module Fcf.Data.NatMap
    ( -- * NatMap type
      NatMap (..)

    -- * Query
    , Null
    , Size
    , Lookup
    , Member
    , NotMember
    , Disjoint
    , Elems
    , Keys
    , Assocs

    -- * Construction
    , Empty
    , Singleton
    , Insert
    , InsertWith
    , Delete

    -- * Combine
    , Union
    , Difference
    , Intersection

    -- * Modify
    , Adjust
    , Map
    , NatMapWithKey
    , Foldr
    , Filter
    , FilterWithKey
    , Partition

    -- * List
    , FromList
    , ToList

    )
  where

-- import qualified GHC.TypeLits as TL

import           Fcf ( Eval, Exp, Fst, Snd, type (=<<), type (<=<), type (@@)
                     , type (++), Not, If
                     , Pure, TyEq, Length, Uncurry)
import qualified Fcf as Fcf (Map, Foldr, Filter)
import           Fcf.Data.List (Elem)

import           Fcf.Data.Nat
import           Fcf.Alg.Morphism
import qualified Fcf.Alg.List as Fcf (Partition)

--------------------------------------------------------------------------------

-- For the doctests:

-- $setup
-- >>> import           Fcf (type (>=))
-- >>> import           Fcf.Data.Nat
-- >>> import           Fcf.Data.Symbol (Symbol,Append)

--------------------------------------------------------------------------------


-- | A type corresponding to IntMap in the containers.
-- 
-- The representation is based on type-level lists. Please, do not use
-- that fact but rather use the exposed API. (We hope to change the 
-- internal data type to balanced tree similar to the one used in containers.
-- See TODO.md.)
data NatMap v = NatMap [(Nat,v)]

-- | Empty
-- 
-- === __Example__
-- 
-- >>> :kind! (Eval Empty :: NatMap Symbol)
-- (Eval Empty :: NatMap Symbol) :: NatMap Symbol
-- = 'NatMap '[]
--
-- >>> :kind! (Eval Empty :: NatMap String)
-- (Eval Empty :: NatMap String) :: NatMap [Char]
-- = 'NatMap '[]
-- 
-- See also the other examples in this module.
data Empty :: Exp (NatMap v)
type instance Eval Empty = 'NatMap '[]

-- | Singleton
-- 
-- === __Example__
-- 
-- >>> :kind! Eval (Singleton 1 "haa")
-- Eval (Singleton 1 "haa") :: NatMap Symbol
-- = 'NatMap '[ '(1, "haa")]
data Singleton :: Nat -> v -> Exp (NatMap v)
type instance Eval (Singleton k v) = 'NatMap '[ '(k,v)]

-- | Use FromList to construct a NatMap from type-level list.
--
-- === __Example__
-- 
-- >>> :kind! Eval (FromList '[ '(1,"haa"), '(2,"hoo")])
-- Eval (FromList '[ '(1,"haa"), '(2,"hoo")]) :: NatMap Symbol
-- = 'NatMap '[ '(1, "haa"), '(2, "hoo")]
data FromList :: [(Nat,v)] -> Exp (NatMap v)
type instance Eval (FromList lst) = 'NatMap lst

-- | Insert
--
-- === __Example__
--
-- >>> :kind! Eval (Insert 3 "hih" =<< FromList '[ '(1,"haa"), '(2,"hoo")])
-- Eval (Insert 3 "hih" =<< FromList '[ '(1,"haa"), '(2,"hoo")]) :: NatMap 
--                                                                    Symbol
-- = 'NatMap '[ '(3, "hih"), '(1, "haa"), '(2, "hoo")]
data Insert :: Nat -> v -> NatMap v -> Exp (NatMap v)
type instance Eval (Insert k v ('NatMap lst)) =
    If (Eval (Elem k =<< Fcf.Map Fst lst))
        ('NatMap lst)
        ('NatMap ( '(k,v) ': lst))


-- | InsertWith
-- if old there, map
-- if no old, add
--
-- === __Example__
-- 
-- >>> :kind! Eval (InsertWith Append 5 "xxx" =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (InsertWith Append 5 "xxx" =<< FromList '[ '(5,"a"), '(3,"b")]) :: NatMap
--                                                                           Symbol
-- = 'NatMap '[ '(5, "xxxa"), '(3, "b")]
--
-- >>> :kind! Eval (InsertWith Append 7 "xxx" =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (InsertWith Append 7 "xxx" =<< FromList '[ '(5,"a"), '(3,"b")]) :: NatMap
--                                                                           Symbol
-- = 'NatMap '[ '(5, "a"), '(3, "b"), '(7, "xxx")]
--
-- >>> :kind! Eval (InsertWith Append 7 "xxx" =<< Empty)
-- Eval (InsertWith Append 7 "xxx" =<< Empty) :: NatMap Symbol
-- = 'NatMap '[ '(7, "xxx")]
data InsertWith :: (v -> v -> Exp v) -> Nat -> v -> NatMap v -> Exp (NatMap v)
type instance Eval (InsertWith f k v ('NatMap lst)) =
    If (Eval (Elem k =<< Fcf.Map Fst lst))
        ('NatMap (Eval (Fcf.Map (InsWithHelp f k v) lst)))
        ('NatMap (Eval (lst ++ '[ '(k,v)])))

-- helper
data InsWithHelp :: (v -> v -> Exp v) -> Nat -> v -> (Nat,v) -> Exp (Nat,v)
type instance Eval (InsWithHelp f k1 vNew '(k2,vOld)) =
    If (Eval (TyEq k1 k2))
        '(k1, Eval (f vNew vOld))
        '(k2, vOld)


-- | Delete
-- 
-- === __Example__
-- 
-- >>> :kind! Eval (Delete 5 =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Delete 5 =<< FromList '[ '(5,"a"), '(3,"b")]) :: NatMap
--                                                          Symbol
-- = 'NatMap '[ '(3, "b")]
--
-- >>> :kind! Eval (Delete 7 =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Delete 7 =<< FromList '[ '(5,"a"), '(3,"b")]) :: NatMap
--                                                          Symbol
-- = 'NatMap '[ '(5, "a"), '(3, "b")]
--
-- >>> :kind! Eval (Delete 7 =<< Empty)
-- Eval (Delete 7 =<< Empty) :: NatMap v
-- = 'NatMap '[]
data Delete :: Nat -> NatMap v -> Exp (NatMap v)
type instance Eval (Delete k ('NatMap lst)) =
    'NatMap (Eval (Fcf.Filter (Not <=< TyEq k <=< Fst) lst))

-- | Adjust
-- 
-- === __Example__
-- 
-- >>> :kind! Eval (Adjust (Append "new ") 5 =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Adjust (Append "new ") 5 =<< FromList '[ '(5,"a"), '(3,"b")]) :: NatMap
--                                                                          Symbol
-- = 'NatMap '[ '(5, "new a"), '(3, "b")]
--
-- >>> :kind! Eval (Adjust (Append "new ") 7 =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Adjust (Append "new ") 7 =<< FromList '[ '(5,"a"), '(3,"b")]) :: NatMap
--                                                                          Symbol
-- = 'NatMap '[ '(5, "a"), '(3, "b")]
--
-- >>> :kind! Eval (Adjust (Append "new ") 7 =<< Empty)
-- Eval (Adjust (Append "new ") 7 =<< Empty) :: NatMap Symbol
-- = 'NatMap '[]
data Adjust :: (v -> Exp v) -> Nat -> NatMap v -> Exp (NatMap v)
type instance Eval (Adjust f k ('NatMap lst)) =
    'NatMap (Eval (AdjustHelp f k lst))

data AdjustHelp :: (v -> Exp v) -> k -> [(Nat,v)] -> Exp [(Nat,v)]
type instance Eval (AdjustHelp _f _k '[]) = '[]
type instance Eval (AdjustHelp f k ( '(k1,v) ': rst)) =
    If (Eval (TyEq k k1))
        ('(k, f @@ v) ': Eval (AdjustHelp f k rst))
        ('(k1,v) ': Eval (AdjustHelp f k rst))


-- | Lookup
--
-- === __Example__
-- 
-- >>> :kind! Eval (Lookup 5 =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Lookup 5 =<< FromList '[ '(5,"a"), '(3,"b")]) :: Maybe Symbol
-- = 'Just "a"
--
-- >>> :kind! Eval (Lookup 7 =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Lookup 7 =<< FromList '[ '(5,"a"), '(3,"b")]) :: Maybe Symbol
-- = 'Nothing
data Lookup :: Nat -> NatMap v -> Exp (Maybe v)
type instance Eval (Lookup k ('NatMap '[])) = 'Nothing
type instance Eval (Lookup k ('NatMap ('(k1,v) ': rst))) =
     Eval (If (Eval (TyEq k k1))
            (Pure ('Just v))
            (Lookup k ('NatMap rst))
          )

-- | Member
--
-- === __Example__
-- 
-- >>> :kind! Eval (Member 5 =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Member 5 =<< FromList '[ '(5,"a"), '(3,"b")]) :: Bool
-- = 'True
-- >>> :kind! Eval (Member 7 =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Member 7 =<< FromList '[ '(5,"a"), '(3,"b")]) :: Bool
-- = 'False
data Member :: Nat -> NatMap v -> Exp Bool
type instance Eval (Member k mp) =
    Eval (Elem k =<< Keys mp)

-- | NotMember
--
-- === __Example__
-- 
-- >>> :kind! Eval (NotMember 5 =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (NotMember 5 =<< FromList '[ '(5,"a"), '(3,"b")]) :: Bool
-- = 'False
-- >>> :kind! Eval (NotMember 7 =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (NotMember 7 =<< FromList '[ '(5,"a"), '(3,"b")]) :: Bool
-- = 'True
data NotMember :: Nat -> NatMap v -> Exp Bool
type instance Eval (NotMember k mp) =
    Eval (Not =<< Elem k =<< Keys mp)

-- | Null
--
-- === __Example__
-- 
-- >>> :kind! Eval (Null =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Null =<< FromList '[ '(5,"a"), '(3,"b")]) :: Bool
-- = 'False
-- >>> :kind! Eval (Null =<< Empty)
-- Eval (Null =<< Empty) :: Bool
-- = 'True
data Null :: NatMap v -> Exp Bool
type instance Eval (Null ('NatMap '[])) = 'True
type instance Eval (Null ('NatMap (_ ': _))) = 'False

-- | Size
--
-- === __Example__
-- 
-- >>> :kind! Eval (Size =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Size =<< FromList '[ '(5,"a"), '(3,"b")]) :: Nat
-- = 2
data Size :: NatMap v -> Exp Nat
type instance Eval (Size ('NatMap lst)) = Eval (Length lst)

-- | Union
--
-- === __Example__
-- 
-- >>> :kind! Eval (Union (Eval (FromList '[ '(5,"a"), '(3,"b")])) (Eval (FromList '[ '(5,"A"), '(7,"c")])) )
-- Eval (Union (Eval (FromList '[ '(5,"a"), '(3,"b")])) (Eval (FromList '[ '(5,"A"), '(7,"c")])) ) :: NatMap 
--                                                                                                      Symbol
-- = 'NatMap '[ '(7, "c"), '(5, "a"), '(3, "b")]
data Union :: NatMap v -> NatMap v -> Exp (NatMap v)
type instance Eval (Union ('NatMap lst1) ('NatMap lst2)) =
    'NatMap (Eval (Fcf.Foldr UComb lst1 lst2))

data UComb :: (k,v) -> [(k,v)] -> Exp [(k,v)]
type instance Eval (UComb '(k,v) lst) =
    If (Eval (Elem k =<< Fcf.Map Fst lst))
        lst
        ('(k,v) ': lst)


-- | Difference
-- 
-- === __Example__
-- 
-- >>> :kind! Eval (Difference (Eval (FromList '[ '(3,"a"), '(5,"b")])) (Eval (FromList '[ '(5,"B"), '(7,"C")])))
-- Eval (Difference (Eval (FromList '[ '(3,"a"), '(5,"b")])) (Eval (FromList '[ '(5,"B"), '(7,"C")]))) :: NatMap 
--                                                                                                          Symbol
-- = 'NatMap '[ '(3, "a")]
data Difference :: NatMap v -> NatMap v -> Exp (NatMap v)
type instance Eval (Difference mp1 mp2) =
    Eval (FilterWithKey (DiffNotMem mp2) mp1)

-- helper
data DiffNotMem :: NatMap v -> k -> v -> Exp Bool
type instance Eval (DiffNotMem mp k _) =
    Eval (Not =<< Elem k =<< Keys mp)


-- | Intersection
--
-- === __Example__
-- 
-- >>> :kind! Eval (Intersection (Eval (FromList '[ '(3,"a"), '(5,"b")])) (Eval (FromList '[ '(5,"B"), '(7,"C")])))
-- Eval (Intersection (Eval (FromList '[ '(3,"a"), '(5,"b")])) (Eval (FromList '[ '(5,"B"), '(7,"C")]))) :: NatMap 
--                                                                                                            Symbol
-- = 'NatMap '[ '(5, "b")]
data Intersection :: NatMap v -> NatMap v -> Exp (NatMap v)
type instance Eval (Intersection mp1 mp2) =
    Eval (FilterWithKey (InterMem mp2) mp1)

-- helper
data InterMem :: NatMap v -> Nat -> v -> Exp Bool
type instance Eval (InterMem mp k _) = Eval (Elem k =<< Keys mp)


-- | Disjoint
--
-- === __Example__
-- 
-- >>> :kind! Eval (Disjoint (Eval (FromList '[ '(3,"a"), '(5,"b")])) (Eval (FromList '[ '(5,"B"), '(7,"C")])))
-- Eval (Disjoint (Eval (FromList '[ '(3,"a"), '(5,"b")])) (Eval (FromList '[ '(5,"B"), '(7,"C")]))) :: Bool
-- = 'False
--
-- >>> :kind! Eval (Disjoint (Eval (FromList '[ '(3,"a"), '(5,"b")])) (Eval (FromList '[ '(2,"B"), '(7,"C")])))
-- Eval (Disjoint (Eval (FromList '[ '(3,"a"), '(5,"b")])) (Eval (FromList '[ '(2,"B"), '(7,"C")]))) :: Bool
-- = 'True
-- >>> :kind! Eval (Disjoint (Eval Empty) (Eval Empty))
-- Eval (Disjoint (Eval Empty) (Eval Empty)) :: Bool
-- = 'True
data Disjoint :: NatMap v -> NatMap v -> Exp Bool
type instance Eval (Disjoint mp1 mp2) =
    Eval (Null =<< Intersection mp1 mp2)


-- | Map
--
-- === __Example__
--
-- >>> :kind! Eval (Fcf.Data.NatMap.Map (Append "x") =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Fcf.Data.NatMap.Map (Append "x") =<< FromList '[ '(5,"a"), '(3,"b")]) :: NatMap 
--                                                                                  Symbol
-- = 'NatMap '[ '(5, "xa"), '(3, "xb")]
data Map :: (v -> Exp w) -> NatMap v -> Exp (NatMap w)
type instance Eval (Map f mp) =
    'NatMap (Eval (Fcf.Map (Second f) =<< Assocs mp))

-- | NatMapWithKey
--
-- === __Example__
-- 
data NatMapWithKey :: (Nat -> v -> Exp w) -> NatMap v -> Exp (NatMap w)
type instance Eval (NatMapWithKey f mp) =
    'NatMap (Eval (Fcf.Map (Second (Uncurry f))
        =<< MWKhelp
        =<< Assocs mp))

data MWKhelp :: [(Nat,v)] -> Exp [(Nat,(Nat,v))]
type instance Eval (MWKhelp '[]) = '[]
type instance Eval (MWKhelp ('(k,v) ': rst)) = '(k, '(k,v)) : Eval (MWKhelp rst)


-- | Foldr
--
-- Fold the values in the map using the given right-associative binary operator, 
-- such that 'foldr f z == foldr f z . elems'.
--
-- Note: the order of values in NatMap is not well defined at the moment.
--
-- === __Example__
-- 
-- >>> :kind! Eval (Fcf.Data.NatMap.Foldr (+) 0  =<< (FromList '[ '(1,1), '(2,2)]))
-- Eval (Fcf.Data.NatMap.Foldr (+) 0  =<< (FromList '[ '(1,1), '(2,2)])) :: Nat
-- = 3
data Foldr :: (v -> w -> Exp w) -> w -> NatMap v -> Exp w
type instance Eval (Foldr f w mp) = Eval (Fcf.Foldr f w =<< Elems mp)


-- | Elems
--
-- === __Example__
-- 
-- >>> :kind! Eval (Elems =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Elems =<< FromList '[ '(5,"a"), '(3,"b")]) :: [Symbol]
-- = '["a", "b"]
-- >>> :kind! Eval (Elems =<< Empty)
-- Eval (Elems =<< Empty) :: [v]
-- = '[]
data Elems :: NatMap v -> Exp [v]
type instance Eval (Elems ('NatMap lst)) = Eval (Fcf.Map Snd lst)

-- | Keys
--
-- === __Example__
-- 
-- >>> :kind! Eval (Keys =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Keys =<< FromList '[ '(5,"a"), '(3,"b")]) :: [Nat]
-- = '[5, 3]
-- >>> :kind! Eval (Keys =<< Empty)
-- Eval (Keys =<< Empty) :: [Nat]
-- = '[]
data Keys :: NatMap v -> Exp [Nat]
type instance Eval (Keys ('NatMap lst)) = Eval (Fcf.Map Fst lst)

-- | Assocs
--
-- === __Example__
-- 
-- >>> :kind! Eval (Assocs =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Assocs =<< FromList '[ '(5,"a"), '(3,"b")]) :: [(Nat, 
--                                                        Symbol)]
-- = '[ '(5, "a"), '(3, "b")]
-- >>> :kind! Eval (Assocs =<< Empty)
-- Eval (Assocs =<< Empty) :: [(Nat, v)]
-- = '[]
data Assocs :: NatMap v -> Exp [(Nat,v)]
type instance Eval (Assocs ('NatMap lst)) = lst

-- | ToList
--
-- === __Example__
-- 
-- >>> :kind! Eval (ToList =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (ToList =<< FromList '[ '(5,"a"), '(3,"b")]) :: [(Nat, 
--                                                        Symbol)]
-- = '[ '(5, "a"), '(3, "b")]
-- >>> :kind! Eval (ToList =<< Empty)
-- Eval (ToList =<< Empty) :: [(Nat, v)]
-- = '[]
data ToList :: NatMap v -> Exp [(Nat,v)]
type instance Eval (ToList ('NatMap lst)) = lst

-- | Filter
--
-- === __Example__
-- 
-- >>> :kind! Eval (Filter ((>=) 35) =<< FromList '[ '(5,50), '(3,30)])
-- Eval (Filter ((>=) 35) =<< FromList '[ '(5,50), '(3,30)]) :: NatMap 
--                                                                Nat
-- = 'NatMap '[ '(3, 30)]
data Filter :: (v -> Exp Bool) -> NatMap v -> Exp (NatMap v)
type instance Eval (Filter f ('NatMap lst)) =
    'NatMap (Eval (Fcf.Filter (f <=< Snd) lst))

-- | FilterWithKey
--
-- === __Example__
--
-- >>> :kind! Eval (FilterWithKey (>=) =<< FromList '[ '(3,5), '(6,4)])
-- Eval (FilterWithKey (>=) =<< FromList '[ '(3,5), '(6,4)]) :: NatMap
--                                                                Nat
-- = 'NatMap '[ '(6, 4)]
data FilterWithKey :: (Nat -> v -> Exp Bool) -> NatMap v -> Exp (NatMap v)
type instance Eval (FilterWithKey f ('NatMap lst)) =
    'NatMap (Eval (Fcf.Filter (Uncurry f) lst))

-- | Partition
--
-- === __Example__
-- 
-- >>> :kind! Eval (Partition ((>=) 35) =<< FromList '[ '(5,50), '(3,30)])
-- Eval (Partition ((>=) 35) =<< FromList '[ '(5,50), '(3,30)]) :: (NatMap 
--                                                                    Nat, 
--                                                                  NatMap Nat)
-- = '( 'NatMap '[ '(3, 30)], 'NatMap '[ '(5, 50)])
data Partition :: (v -> Exp Bool) -> NatMap v -> Exp (NatMap v, NatMap v)
type instance Eval (Partition f ('NatMap lst)) =
    Eval (PartitionHlp (Eval (Fcf.Partition (f <=< Snd) lst)))

data PartitionHlp :: ([(Nat,v)],[(Nat,v)]) -> Exp (NatMap v, NatMap v)
type instance Eval (PartitionHlp '(xs,ys)) = '( 'NatMap xs, 'NatMap ys)


