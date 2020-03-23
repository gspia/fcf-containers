{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

{-|
Module      : Fcf.Data.MapC
Description : Map (association) data-type for the type-level programming
Copyright   : (c) gspia 2020-
License     : BSD
Maintainer  : gspia

= Fcf.Data.MapC

MapC provides an interface to mapping keys to values, which is similar to
that given by the containers-package. Note that the this module still misses
some of the methods that can be found in containers. If you need some, please
do open up an issue or better, make a PR.

Many of the examples are from containers-package.

We call this MapC because name Map is reserved to the map-function in
Fcf-package. The internal representation is type-level list. We hope that
one day the internal representation is based on balanced trees similar to the
one used in the containers.


-}

--------------------------------------------------------------------------------

module Fcf.Data.MapC
    ( -- * MapC type
      MapC (..)

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
    , MapWithKey
    , Foldr
    , Filter
    , FilterWithKey
    , Partition

    -- * List
    , FromList
    , ToList

    )
  where

import qualified GHC.TypeLits as TL

import           Fcf ( Eval, Exp, Fst, Snd, type (=<<), type (<=<), type (@@)
                     , type (++), Not, If
                     , Pure, TyEq, Length, Uncurry)
import qualified Fcf as Fcf (Map, Foldr, Filter)
import qualified Fcf.Data.List as L (Elem, Partition)

import           Fcf.Alg.Morphism
-- import qualified Fcf.Alg.List as Fcf (Partition)

--------------------------------------------------------------------------------

-- For the doctests:

-- $setup
-- >>> import           Fcf (type (>=))
-- >>> import           Fcf.Data.Nat
-- >>> import           Fcf.Alg.Symbol (Symbol,Append)

--------------------------------------------------------------------------------


-- | A type corresponding to Map in the containers. We call this MapC because
-- name Map is reserved to the map-function in Fcf-package.
-- 
-- The representation is based on type-level lists. Please, do not use
-- that fact but rather use the exposed API. (We hope to change the 
-- internal data type to balanced tree similar to the one used in containers.
-- See TODO.md.)
data MapC k v = MapC [(k,v)]

-- | Empty
-- 
-- === __Example__
-- 
-- >>> :kind! (Eval Empty :: MapC Nat Symbol)
-- (Eval Empty :: MapC Nat Symbol) :: MapC Nat Symbol
-- = 'MapC '[]
--
-- >>> :kind! (Eval Empty :: MapC Int String)
-- (Eval Empty :: MapC Int String) :: MapC Int [Char]
-- = 'MapC '[]
-- 
-- See also the other examples in this module.
data Empty :: Exp (MapC k v)
type instance Eval Empty = 'MapC '[]

-- | Singleton
-- 
-- === __Example__
-- 
-- >>> :kind! Eval (Singleton 1 "haa")
-- Eval (Singleton 1 "haa") :: MapC Nat Symbol
-- = 'MapC '[ '(1, "haa")]
data Singleton :: k -> v -> Exp (MapC k v)
type instance Eval (Singleton k v) = 'MapC '[ '(k,v)]

-- | Use FromList to construct a MapC from type-level list.
--
-- === __Example__
-- 
-- >>> :kind! Eval (FromList '[ '(1,"haa"), '(2,"hoo")])
-- Eval (FromList '[ '(1,"haa"), '(2,"hoo")]) :: MapC Nat Symbol
-- = 'MapC '[ '(1, "haa"), '(2, "hoo")]
data FromList :: [(k,v)] -> Exp (MapC k v)
type instance Eval (FromList lst) = 'MapC lst

-- | Insert
--
-- === __Example__
--
-- >>> :kind! Eval (Insert 3 "hih" =<< FromList '[ '(1,"haa"), '(2,"hoo")])
-- Eval (Insert 3 "hih" =<< FromList '[ '(1,"haa"), '(2,"hoo")]) :: MapC 
--                                                                    Nat Symbol
-- = 'MapC '[ '(3, "hih"), '(1, "haa"), '(2, "hoo")]
data Insert :: k -> v -> MapC k v -> Exp (MapC k v)
type instance Eval (Insert k v ('MapC lst)) =
    If (Eval (L.Elem k =<< Fcf.Map Fst lst))
        ('MapC lst)
        ('MapC ( '(k,v) ': lst))


-- | InsertWith
-- if old there, map
-- if no old, add
--
-- === __Example__
-- 
-- >>> :kind! Eval (InsertWith Append 5 "xxx" =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (InsertWith Append 5 "xxx" =<< FromList '[ '(5,"a"), '(3,"b")]) :: MapC
--                                                                           Nat Symbol
-- = 'MapC '[ '(5, "xxxa"), '(3, "b")]
--
-- >>> :kind! Eval (InsertWith Append 7 "xxx" =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (InsertWith Append 7 "xxx" =<< FromList '[ '(5,"a"), '(3,"b")]) :: MapC
--                                                                           Nat Symbol
-- = 'MapC '[ '(5, "a"), '(3, "b"), '(7, "xxx")]
-- >>> :kind! Eval (InsertWith Append 7 "xxx" =<< Empty)
-- Eval (InsertWith Append 7 "xxx" =<< Empty) :: MapC Nat Symbol
-- = 'MapC '[ '(7, "xxx")]
data InsertWith :: (v -> v -> Exp v) -> k -> v -> MapC k v -> Exp (MapC k v)
type instance Eval (InsertWith f k v ('MapC lst)) =
    If (Eval (L.Elem k =<< Fcf.Map Fst lst))
        ('MapC (Eval (Fcf.Map (InsWithHelp f k v) lst)))
        ('MapC (Eval (lst ++ '[ '(k,v)])))

-- helper
data InsWithHelp :: (v -> v -> Exp v) -> k -> v -> (k,v) -> Exp (k,v)
type instance Eval (InsWithHelp f k1 vNew '(k2,vOld)) =
    If (Eval (TyEq k1 k2))
        '(k1, Eval (f vNew vOld))
        '(k2, vOld)


-- | Delete
-- 
-- === __Example__
-- 
-- >>> :kind! Eval (Delete 5 =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Delete 5 =<< FromList '[ '(5,"a"), '(3,"b")]) :: MapC
--                                                          Nat Symbol
-- = 'MapC '[ '(3, "b")]
--
-- >>> :kind! Eval (Delete 7 =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Delete 7 =<< FromList '[ '(5,"a"), '(3,"b")]) :: MapC
--                                                          Nat Symbol
-- = 'MapC '[ '(5, "a"), '(3, "b")]
--
-- >>> :kind! Eval (Delete 7 =<< Empty)
-- Eval (Delete 7 =<< Empty) :: MapC Nat v
-- = 'MapC '[]
data Delete :: k -> MapC k v -> Exp (MapC k v)
type instance Eval (Delete k ('MapC lst)) =
    'MapC (Eval (Fcf.Filter (Not <=< TyEq k <=< Fst) lst))

-- | Adjust
-- 
-- === __Example__
-- 
-- >>> :kind! Eval (Adjust (Append "new ") 5 =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Adjust (Append "new ") 5 =<< FromList '[ '(5,"a"), '(3,"b")]) :: MapC
--                                                                          Nat Symbol
-- = 'MapC '[ '(5, "new a"), '(3, "b")]
--
-- >>> :kind! Eval (Adjust (Append "new ") 7 =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Adjust (Append "new ") 7 =<< FromList '[ '(5,"a"), '(3,"b")]) :: MapC
--                                                                          Nat Symbol
-- = 'MapC '[ '(5, "a"), '(3, "b")]
--
-- >>> :kind! Eval (Adjust (Append "new ") 7 =<< Empty)
-- Eval (Adjust (Append "new ") 7 =<< Empty) :: MapC Nat Symbol
-- = 'MapC '[]
data Adjust :: (v -> Exp v) -> k -> MapC k v -> Exp (MapC k v)
type instance Eval (Adjust f k ('MapC lst)) =
    'MapC (Eval (AdjustHelp f k lst))

data AdjustHelp :: (v -> Exp v) -> k -> [(k,v)] -> Exp [(k,v)]
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
data Lookup :: k -> MapC k v -> Exp (Maybe v)
type instance Eval (Lookup k ('MapC '[])) = 'Nothing
type instance Eval (Lookup k ('MapC ('(k1,v) ': rst))) =
     Eval (If (Eval (TyEq k k1))
            (Pure ('Just v))
            (Lookup k ('MapC rst))
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
data Member :: k -> MapC k v -> Exp Bool
type instance Eval (Member k mp) =
    Eval (L.Elem k =<< Keys mp)

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
data NotMember :: k -> MapC k v -> Exp Bool
type instance Eval (NotMember k mp) =
    Eval (Not =<< L.Elem k =<< Keys mp)

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
data Null :: MapC k v -> Exp Bool
type instance Eval (Null ('MapC '[])) = 'True
type instance Eval (Null ('MapC (_ ': _))) = 'False

-- | Size
--
-- === __Example__
-- 
-- >>> :kind! Eval (Size =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Size =<< FromList '[ '(5,"a"), '(3,"b")]) :: Nat
-- = 2
data Size :: MapC k v -> Exp TL.Nat
type instance Eval (Size ('MapC lst)) = Eval (Length lst)

-- | Union
--
-- === __Example__
-- 
-- >>> :kind! Eval (Union (Eval (FromList '[ '(5,"a"), '(3,"b")])) (Eval (FromList '[ '(5,"A"), '(7,"c")])) )
-- Eval (Union (Eval (FromList '[ '(5,"a"), '(3,"b")])) (Eval (FromList '[ '(5,"A"), '(7,"c")])) ) :: MapC 
--                                                                                                      Nat 
--                                                                                                      Symbol
-- = 'MapC '[ '(7, "c"), '(5, "a"), '(3, "b")]
data Union :: MapC k v -> MapC k v -> Exp (MapC k v)
type instance Eval (Union ('MapC lst1) ('MapC lst2)) =
    'MapC (Eval (Fcf.Foldr UComb lst1 lst2))

data UComb :: (k,v) -> [(k,v)] -> Exp [(k,v)]
type instance Eval (UComb '(k,v) lst) =
    If (Eval (L.Elem k =<< Fcf.Map Fst lst))
        lst
        ('(k,v) ': lst)


-- | Difference
-- 
-- === __Example__
-- 
-- >>> :kind! Eval (Difference (Eval (FromList '[ '(3,"a"), '(5,"b")])) (Eval (FromList '[ '(5,"B"), '(7,"C")])))
-- Eval (Difference (Eval (FromList '[ '(3,"a"), '(5,"b")])) (Eval (FromList '[ '(5,"B"), '(7,"C")]))) :: MapC 
--                                                                                                          Nat 
--                                                                                                          Symbol
-- = 'MapC '[ '(3, "a")]
data Difference :: MapC k v -> MapC k v -> Exp (MapC k v)
type instance Eval (Difference mp1 mp2) =
    Eval (FilterWithKey (DiffNotMem mp2) mp1)

-- helper
data DiffNotMem :: MapC k v -> k -> v -> Exp Bool
type instance Eval (DiffNotMem mp k _) =
    Eval (Not =<< L.Elem k =<< Keys mp)


-- | Intersection
--
-- === __Example__
-- 
-- >>> :kind! Eval (Intersection (Eval (FromList '[ '(3,"a"), '(5,"b")])) (Eval (FromList '[ '(5,"B"), '(7,"C")])))
-- Eval (Intersection (Eval (FromList '[ '(3,"a"), '(5,"b")])) (Eval (FromList '[ '(5,"B"), '(7,"C")]))) :: MapC 
--                                                                                                            Nat 
--                                                                                                            Symbol
-- = 'MapC '[ '(5, "b")]
data Intersection :: MapC k v -> MapC k v -> Exp (MapC k v)
type instance Eval (Intersection mp1 mp2) =
    Eval (FilterWithKey (InterMem mp2) mp1)

-- helper
data InterMem :: MapC k v -> k -> v -> Exp Bool
type instance Eval (InterMem mp k _) = Eval (L.Elem k =<< Keys mp)


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
data Disjoint :: MapC k v -> MapC k v -> Exp Bool
type instance Eval (Disjoint mp1 mp2) =
    Eval (Null =<< Intersection mp1 mp2)


-- Map 
--
-- === __Example__
--
-- >>> :kind! Eval (Fcf.Data.MapC.Map (Append "x") =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Fcf.Data.MapC.Map (Append "x") =<< FromList '[ '(5,"a"), '(3,"b")]) :: MapC Nat Symbol
-- = 'MapC '[ '(5, "xa"), '(3, "xb")]
data Map :: (v -> Exp w) -> MapC k v -> Exp (MapC k w)
type instance Eval (Map f mp) =
    'MapC (Eval (Fcf.Map (Second f) =<< Assocs mp))

-- MapWithKey 
--
-- === __Example__
-- 
data MapWithKey :: (k -> v -> Exp w) -> MapC k v -> Exp (MapC k w)
type instance Eval (MapWithKey f mp) =
    'MapC (Eval (Fcf.Map (Second (Uncurry f))
        =<< MWKhelp
        =<< Assocs mp))

data MWKhelp :: [(k,v)] -> Exp [(k,(k,v))]
type instance Eval (MWKhelp '[]) = '[]
type instance Eval (MWKhelp ('(k,v) ': rst)) = '(k, '(k,v)) : Eval (MWKhelp rst)


-- | Foldr
--
-- Fold the values in the map using the given right-associative binary operator, 
-- such that 'foldr f z == foldr f z . elems'.
--
-- Note: the order of values in MapC is not well defined at the moment.
--
-- === __Example__
-- 
-- >>> :kind! Eval (Fcf.Data.MapC.Foldr (+) 0  =<< (FromList '[ '(1,1), '(2,2)]))
-- Eval (Fcf.Data.MapC.Foldr (+) 0  =<< (FromList '[ '(1,1), '(2,2)])) :: Nat
-- = 3
data Foldr :: (v -> w -> Exp w) -> w -> MapC k v -> Exp w
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
data Elems :: MapC k v -> Exp [v]
type instance Eval (Elems ('MapC lst)) = Eval (Fcf.Map Snd lst)

-- | Keys
--
-- === __Example__
-- 
-- >>> :kind! Eval (Keys =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Keys =<< FromList '[ '(5,"a"), '(3,"b")]) :: [Nat]
-- = '[5, 3]
-- >>> :kind! Eval (Keys =<< Empty)
-- Eval (Keys =<< Empty) :: [k]
-- = '[]
data Keys :: MapC k v -> Exp [k]
type instance Eval (Keys ('MapC lst)) = Eval (Fcf.Map Fst lst)

-- | Assocs
--
-- === __Example__
-- 
-- >>> :kind! Eval (Assocs =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Assocs =<< FromList '[ '(5,"a"), '(3,"b")]) :: [(Nat, 
--                                                        Symbol)]
-- = '[ '(5, "a"), '(3, "b")]
-- >>> :kind! Eval (Assocs =<< Empty)
-- Eval (Assocs =<< Empty) :: [(k, v)]
-- = '[]
data Assocs :: MapC k v -> Exp [(k,v)]
type instance Eval (Assocs ('MapC lst)) = lst

-- | ToList
--
-- === __Example__
-- 
-- >>> :kind! Eval (ToList =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (ToList =<< FromList '[ '(5,"a"), '(3,"b")]) :: [(Nat, 
--                                                        Symbol)]
-- = '[ '(5, "a"), '(3, "b")]
-- >>> :kind! Eval (ToList =<< Empty)
-- Eval (ToList =<< Empty) :: [(k, v)]
-- = '[]
data ToList :: MapC k v -> Exp [(k,v)]
type instance Eval (ToList ('MapC lst)) = lst

-- | Filter
--
-- === __Example__
-- 
-- >>> :kind! Eval (Filter ((>=) 35) =<< FromList '[ '(5,50), '(3,30)])
-- Eval (Filter ((>=) 35) =<< FromList '[ '(5,50), '(3,30)]) :: MapC 
--                                                                Nat Nat
-- = 'MapC '[ '(3, 30)]
data Filter :: (v -> Exp Bool) -> MapC k v -> Exp (MapC k v)
type instance Eval (Filter f ('MapC lst)) =
    'MapC (Eval (Fcf.Filter (f <=< Snd) lst))

-- | FilterWithKey
--
-- === __Example__
--
-- >>> :kind! Eval (FilterWithKey (>=) =<< FromList '[ '(3,5), '(6,4)])
-- Eval (FilterWithKey (>=) =<< FromList '[ '(3,5), '(6,4)]) :: MapC
--                                                                Nat Nat
-- = 'MapC '[ '(6, 4)]
data FilterWithKey :: (k -> v -> Exp Bool) -> MapC k v -> Exp (MapC k v)
type instance Eval (FilterWithKey f ('MapC lst)) =
    'MapC (Eval (Fcf.Filter (Uncurry f) lst))

-- | Partition
--
-- === __Example__
-- 
-- >>> :kind! Eval (Partition ((>=) 35) =<< FromList '[ '(5,50), '(3,30)])
-- Eval (Partition ((>=) 35) =<< FromList '[ '(5,50), '(3,30)]) :: (MapC 
--                                                                    Nat Nat, 
--                                                                  MapC Nat Nat)
-- = '( 'MapC '[ '(3, 30)], 'MapC '[ '(5, 50)])
data Partition :: (v -> Exp Bool) -> MapC k v -> Exp (MapC k v, MapC k v)
type instance Eval (Partition f ('MapC lst)) =
    Eval (PartitionHlp (Eval (L.Partition (f <=< Snd) lst)))

data PartitionHlp :: ([(k,v)],[(k,v)]) -> Exp (MapC k v, MapC k v)
type instance Eval (PartitionHlp '(xs,ys)) = '( 'MapC xs, 'MapC ys)


