{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeInType               #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances     #-}
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
      MapC(..)

    -- * Query
    -- , Null
    , Size
    -- , Lookup
    -- , Member
    -- , NotMember
    -- , Disjoint
    -- , Elems
    -- , Keys
    -- , Assocs

    -- -- * Construction
    , Empty
    , Singleton
    , Insert
    , InsertExp
    , InsertWith
    , InsertWithExp
    , InsertWithKey
    , Delete
    , DeleteExp

    -- -- * Combine
    -- , Union
    -- , Difference
    -- , Intersection

    -- -- * Modify
    -- , Adjust
    -- , Map
    -- , MapWithKey
    -- , Foldr
    -- , Filter
    -- , FilterWithKey
    -- , Partition

    -- -- * List
    , FromList
    -- , ToList
    , Uncurry1

    )
  where

-- import qualified GHC.TypeLits as TL

import           GHC.TypeLits (TypeError, ErrorMessage(..))

import           Fcf ( Eval, Exp, Fst, Snd, type (=<<), type (<=<), type (@@)
                     , type (++), Not, If
                     , Pure, TyEq, Length, Uncurry
                     , Case, type (-->), Flip)
import           Fcf (Map, Filter)
import           Fcf.Data.List.Utils (Foldl)
import qualified Fcf.Data.List as L (Elem, Partition)
import           Fcf.Data.Nat (type (>), type (<))
import           GHC.TypeLits as Nat
import qualified Fcf.Combinators as C
import           Fcf.Alg.Morphism
import           Fcf.Class.Ord (Compare)
-- import qualified Fcf.Alg.List as Fcf (Partition)

--------------------------------------------------------------------------------

-- For the doctests:

-- $setup
-- >>> import           Fcf (Eval)
-- >>> import           Fcf.Class.Functor (FMap)
-- >>> import           Fcf.Data.Nat
-- >>> import           Fcf.Data.Symbol
-- >>> import           Fcf.Class.Foldable (Foldr)
-- >>> import           Fcf.Data.List.Utils (Foldl)
-- >>> import           Fcf.Data.Common (Uncurry)
-- >>> import           Fcf.Class.Monoid (type (.<>))

--------------------------------------------------------------------------------

type Delta = 3
type Ratio = 2

--------------------------------------------------------------------------------


-- | A type corresponding to Map in the containers. We call this MapC because
-- name Map is reserved to the map-function in Fcf-package.
--
-- The representation is based on type-level lists. Please, do not use
-- that fact but rather use the exposed API. (We hope to change the
-- internal data type to balanced tree similar to the one used in containers.
-- See TODO.md.)
data MapC k v = Bin Nat k v (MapC k v) (MapC k v)
              | Tip

-- | Empty
--
-- === __Example__
--
-- >>> :kind! Empty :: MapC Nat Symbol
-- Empty :: MapC Nat Symbol :: MapC Natural Symbol
-- = 'Tip
--
-- >>> :kind! Empty :: MapC Int String
-- Empty :: MapC Int String :: MapC Int [Char]
-- = 'Tip
--
-- See also the other examples in this module.
--
type Empty :: MapC k v
type Empty = 'Tip
-- type instance Eval Empty = 'MapC '[]

-- | Singleton
--
-- === __Example__
--
-- >>> :kind! Singleton 1 "haa"
-- Singleton 1 "haa" :: MapC Natural Symbol
-- = 'Bin 1 1 "haa" 'Tip 'Tip
type Singleton :: k -> v -> MapC k v
type Singleton k v = 'Bin 1 k v 'Tip 'Tip

-- | SingletonExp
--
-- === __Example__
--
-- >>> :kind! Eval (FMap (Uncurry SingletonExp) '[ '("key1", 1), '("key2", 2) ])
-- Eval (FMap (Uncurry SingletonExp) '[ '("key1", 1), '("key2", 2) ]) :: [MapC
--                                                                          Symbol Natural]
-- = '[ 'Bin 1 "key1" 1 'Tip 'Tip, 'Bin 1 "key2" 2 'Tip 'Tip]
--
data SingletonExp :: k -> v -> Exp (MapC k v)
type instance Eval (SingletonExp k v) = Singleton k v

-- | Use FromList to construct a MapC from type-level list.
--
-- === __Example__
--
-- >>> :kind! (FromList '[ '(1,"haa"), '(2,"hoo")])
-- (FromList '[ '(1,"haa"), '(2,"hoo")]) :: MapC Natural Symbol
-- = 'Bin 2 1 "haa" 'Tip ('Bin 1 2 "hoo" 'Tip 'Tip)
--
type FromList :: [(k,v)] -> MapC k v
type FromList list = Foldl (Flip (Uncurry1 InsertExp)) Empty @@ list

-- | Insert
--
-- === __Example__
--
-- >>> :kind! Eval (FMap (InsertExp 3 "hih") '[FromList '[ '(1,"haa"), '(2,"hoo")], Empty])
-- Eval (FMap (InsertExp 3 "hih") '[FromList '[ '(1,"haa"), '(2,"hoo")], Empty]) :: [MapC
--                                                                                     Natural Symbol]
-- = '[ 'Bin
--        3 2 "hoo" ('Bin 1 1 "haa" 'Tip 'Tip) ('Bin 1 3 "hih" 'Tip 'Tip),
--      'Bin 1 3 "hih" 'Tip 'Tip]
--
data InsertExp :: k -> v -> MapC k v -> Exp (MapC k v)
type instance Eval (InsertExp k v map) = Insert k v map

type Insert :: k -> v -> MapC k v -> MapC k v
type family Insert kx x map where
  Insert kx x 'Tip = Singleton kx x
  Insert kx x ('Bin sz ky y l r) =
    Case [ 'LT --> BalanceL ky y (Insert kx x l) r
         , 'GT --> BalanceR ky y l (Insert kx x r)
         , 'EQ --> 'Bin sz kx x l r
         ] @@ Eval (Compare kx ky)


--------------------------------------------------------------------------------


-- | InsertWith
--
-- === __Example__
--
-- >>> :kind! Eval (Foldr (Uncurry1 (InsertWithExp (.<>))) Empty '[ '(5,"a"), '(5,"b")])
-- Eval (Foldr (Uncurry1 (InsertWithExp (.<>))) Empty '[ '(5,"a"), '(5,"b")]) :: MapC
--                                                                                 Natural Symbol
-- = 'Bin 1 5 "ba" 'Tip 'Tip
--
data InsertWithExp :: (a -> a -> Exp a) -> k -> a -> MapC k a -> Exp (MapC k a)
type instance Eval (InsertWithExp f kx x map) = InsertWith f kx x map

-- | /O(log n)/. Insert with a function, combining new value and old value.
-- @'InsertWith' f key value mp@
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert the pair @(key, f new_value old_value)@.
--
-- === __Example__
--
-- >>> :kind! InsertWith (.<>) 5 "xxx" (FromList '[ '(5,"a"), '(3,"b")])
-- InsertWith (.<>) 5 "xxx" (FromList '[ '(5,"a"), '(3,"b")]) :: MapC
--                                                                 Natural Symbol
-- = 'Bin 2 5 "axxx" ('Bin 1 3 "b" 'Tip 'Tip) 'Tip
--
-- >>> :kind! InsertWith (.<>) 7 "xxx" (FromList '[ '(5,"a"), '(3,"b")])
-- InsertWith (.<>) 7 "xxx" (FromList '[ '(5,"a"), '(3,"b")]) :: MapC
--                                                                 Natural Symbol
-- = 'Bin 3 5 "a" ('Bin 1 3 "b" 'Tip 'Tip) ('Bin 1 7 "xxx" 'Tip 'Tip)
--
-- >>> :kind! InsertWith (.<>) 7 "xxx" Empty
-- InsertWith (.<>) 7 "xxx" Empty :: MapC Natural Symbol
-- = 'Bin 1 7 "xxx" 'Tip 'Tip
--
type InsertWith :: (a -> a -> Exp a) -> k -> a -> MapC k a -> MapC k a
type family InsertWith f kx x map where
  InsertWith _ kx x 'Tip = Singleton kx x
  InsertWith f kx x ('Bin sy ky y l r) = 
    Case [ 'LT --> BalanceL ky y (InsertWith f kx x l) r
         , 'GT --> BalanceR ky y l (InsertWith f kx x r)
         , 'EQ --> 'Bin sy kx (Eval (f y x)) l r
         ] @@ Eval (Compare kx ky)

-- | A helper function for 'unionWith'. When the key is already in
-- the map, the key is left alone, not replaced. The combining
-- function is flipped--it is applied to the old value and then the
-- new value.
type InsertWithR :: (a -> a -> Exp a) -> k -> a -> MapC k a -> MapC k a
type family InsertWithR f kx x map where
  InsertWithR _ kx x 'Tip = Singleton kx x
  InsertWithR f kx x ('Bin sy ky y l r) = 
    Case [ 'LT --> BalanceL ky y (InsertWithR f kx x l) r
         , 'GT --> BalanceR ky y l (InsertWithR f kx x r)
         , 'EQ --> 'Bin sy ky (Eval (f y x)) l r
         ] @@ Eval (Compare kx ky)

-- | /O(log n)/. Insert with a function, combining key, new value and old value.
-- @'InsertWithKey' f key value mp@
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert the pair @(key,f key new_value old_value)@.
-- Note that the key passed to f is the same key passed to 'insertWithKey'.
--
-- > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- > insertWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:xxx|a")]
-- > insertWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "xxx")]
-- > insertWithKey f 5 "xxx" empty                         == singleton 5 "xxx"
type InsertWithKey :: (k -> a -> a -> a) -> k -> a -> MapC k a -> MapC k a
type family InsertWithKey f kx x map where
  InsertWithKey _ kx x 'Tip = Singleton kx x
  InsertWithKey f kx x ('Bin sy ky y l r) =
        Case '[ 'LT --> BalanceL ky y (InsertWithKey f kx x l) r
              , 'GT --> BalanceR ky y l (InsertWithKey f kx x r)
              , 'EQ --> 'Bin sy kx (f kx x y) l r
              ] @@ Eval (Compare kx ky)

data DeleteExp :: k -> MapC k a -> Exp (MapC k a)
type instance Eval (DeleteExp k map) = Delete k map

-- | Delete
--
-- === __Example__
--
-- >>> :kind! Delete 5 (FromList '[ '(5,"a"), '(3,"b")])
-- Delete 5 (FromList '[ '(5,"a"), '(3,"b")]) :: MapC Natural Symbol
-- = 'Bin 1 3 "b" 'Tip 'Tip
--
-- >>> :kind! Delete 7 (FromList '[ '(5,"a"), '(3,"b")])
-- Delete 7 (FromList '[ '(5,"a"), '(3,"b")]) :: MapC Natural Symbol
-- = 'Bin 2 5 "a" ('Bin 1 3 "b" 'Tip 'Tip) 'Tip
--
-- >>> :kind! Delete 7 Empty
-- Delete 7 Empty :: MapC Natural a
-- = 'Tip
--
type Delete :: k -> MapC k a -> MapC k a
type family Delete k map where
  Delete _ 'Tip = 'Tip
  Delete k ('Bin sx kx x l r) =
    Case '[ 'LT --> DeleteLTIf k ('Bin sx kx x l r) (Delete k l) l kx x r
          , 'GT --> DeleteGTIf k ('Bin sx kx x l r) (Delete k r) l kx x r
          , 'EQ --> Glue l r
          ] @@ Eval (Compare k kx)

-- This lets me compute 'Delete k l' once 
type DeleteLTIf :: 
     k -- ^ k
  -> MapC k a -- ^ t@(Bin _ kx x l r)
  -> MapC k a -- ^ where !l' = go k l
  -> MapC k a -- ^ l
  -> k -- ^ kx 
  -> a -- ^ x
  -> MapC k a -- ^ r
  -> MapC k a
type DeleteLTIf k t l' l kx x r = 
  If (ShallowEq l' l)
     t
     (BalanceR kx x l' r)

-- This lets me compute 'Delete k r' once 
type DeleteGTIf ::
     k -- ^ k
  -> MapC k a -- ^ t@(Bin _ kx x l r)
  -> MapC k a -- ^ where !r' = go k r
  -> MapC k a -- ^ l
  -> k -- ^ kx 
  -> a -- ^ x
  -> MapC k a -- ^ r
  -> MapC k a
type DeleteGTIf k t r' l kx x r =
  If (ShallowEq r' r)
     t
     (BalanceL kx x l r')

-- Data.Map.Strict.Internal uses 'ptrEq' which should behave the same as this
type ShallowEq :: MapC k a -> MapC k a -> Bool
type family ShallowEq l r where
  ShallowEq 'Tip 'Tip = 'True
  ShallowEq ('Bin n k v _ _) ('Bin n k v _ _) = 'True
  ShallowEq _ _ = 'False

data MinView k a = MinView k a (MapC k a)
data MaxView k a = MaxView k a (MapC k a)

type Glue :: MapC k a -> MapC k a -> MapC k a
type family Glue l r where
  Glue 'Tip r = r
  Glue l 'Tip = l
  Glue ('Bin sl kl xl ll lr) ('Bin sr kr xr rl rr) =
    If (Eval (sl > sr))
       (GlueLet1 ('Bin sr kr xr rl rr) (MaxViewSure kl xl ll lr))
       (GlueLet2 ('Bin sl kl xl ll lr) (MinViewSure kr xr rl rr))

type GlueLet1 :: MapC k a -> MaxView k a -> MapC k a
type family GlueLet1 rightMap m where
  GlueLet1 r ('MaxView km m l') = BalanceR km m l' r

type GlueLet2 :: MapC k a -> MinView k a -> MapC k a
type family GlueLet2 leftMap m where
  GlueLet2 l ('MinView km m r') = BalanceL km m l r'

type MinViewSure :: k -> a -> MapC k a -> MapC k a -> MinView k a
type family MinViewSure k x l r where
  MinViewSure k x 'Tip r = 'MinView k x r
  MinViewSure k x ('Bin _ kl xl ll lr) r = MinViewSureCase k x r (MinViewSure kl xl ll lr)

type MinViewSureCase :: 
     k -- ^ k
  -> a -- ^ x
  -> MapC k a -- ^ r
  -> MinView k a -- ^ go kl xl ll lr
  -> MinView k a
type family MinViewSureCase k x r mv where
  MinViewSureCase k x r ('MinView km xm l') = 'MinView km xm (BalanceR k x l' r)

type MaxViewSure :: k -> a -> MapC k a -> MapC k a -> MaxView k a
type family MaxViewSure k x l r where
  MaxViewSure k x l 'Tip = 'MaxView k x l
  MaxViewSure k x l ('Bin _ kr xr rl rr) = MaxViewSureCase k x l (MaxViewSure kr xr rl rr)

type MaxViewSureCase ::
     k -- ^ k
  -> a -- ^ x
  -> MapC k a -- ^ l
  -> MaxView k a -- ^ go kr xr rl rr
  -> MaxView k a
type family MaxViewSureCase k x l mv where
  MaxViewSureCase k x l ('MaxView km xm r') = 'MaxView km xm (BalanceL k x l r')

-- | Adjust
--
-- === __Example__
--
-- > :kind! Eval (Adjust (AppendSymbol "new ") 5 =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Adjust (AppendSymbol "new ") 5 =<< FromList '[ '(5,"a"), '(3,"b")]) :: MapC
--                                                                                TL.Natural TL.Symbol
-- = 'MapC '[ '(5, "new a"), '(3, "b")]
--
-- > :kind! Eval (Adjust (AppendSymbol "new ") 7 =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Adjust (AppendSymbol "new ") 7 =<< FromList '[ '(5,"a"), '(3,"b")]) :: MapC
--                                                                                TL.Natural TL.Symbol
-- = 'MapC '[ '(5, "a"), '(3, "b")]
--
-- > :kind! Eval (Adjust (AppendSymbol "new ") 7 =<< Empty)
-- Eval (Adjust (AppendSymbol "new ") 7 =<< Empty) :: MapC
--                                                      TL.Natural TL.Symbol
-- = 'MapC '[]
-- data Adjust :: (v -> Exp v) -> k -> MapC k v -> Exp (MapC k v)
-- type instance Eval (Adjust f k ('MapC lst)) =
--     'MapC (Eval (AdjustHelp f k lst))

-- data AdjustHelp :: (v -> Exp v) -> k -> [(k,v)] -> Exp [(k,v)]
-- type instance Eval (AdjustHelp _f _k '[]) = '[]
-- type instance Eval (AdjustHelp f k ( '(k1,v) ': rst)) =
--     If (Eval (TyEq k k1))
--         ('(k, f @@ v) ': Eval (AdjustHelp f k rst))
--         ('(k1,v) ': Eval (AdjustHelp f k rst))


-- | Lookup
--
-- === __Example__
--
-- > :kind! Eval (Lookup 5 =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Lookup 5 =<< FromList '[ '(5,"a"), '(3,"b")]) :: Maybe
--                                                          TL.Symbol
-- = 'Just "a"
--
-- > :kind! Eval (Lookup 7 =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Lookup 7 =<< FromList '[ '(5,"a"), '(3,"b")]) :: Maybe
--                                                          TL.Symbol
-- = 'Nothing
-- data Lookup :: k -> MapC k v -> Exp (Maybe v)
-- type instance Eval (Lookup k ('MapC '[])) = 'Nothing
-- type instance Eval (Lookup k ('MapC ('(k1,v) ': rst))) =
--      Eval (If (Eval (TyEq k k1))
--             (Pure ('Just v))
--             (Lookup k ('MapC rst))
--           )

-- | Member
--
-- === __Example__
--
-- > :kind! Eval (Member 5 =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Member 5 =<< FromList '[ '(5,"a"), '(3,"b")]) :: Bool
-- = 'True
-- > :kind! Eval (Member 7 =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Member 7 =<< FromList '[ '(5,"a"), '(3,"b")]) :: Bool
-- = 'False
-- data Member :: k -> MapC k v -> Exp Bool
-- type instance Eval (Member k mp) =
--     Eval (L.Elem k =<< Keys mp)

-- | NotMember
--
-- === __Example__
--
-- > :kind! Eval (NotMember 5 =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (NotMember 5 =<< FromList '[ '(5,"a"), '(3,"b")]) :: Bool
-- = 'False
-- > :kind! Eval (NotMember 7 =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (NotMember 7 =<< FromList '[ '(5,"a"), '(3,"b")]) :: Bool
-- = 'True
-- data NotMember :: k -> MapC k v -> Exp Bool
-- type instance Eval (NotMember k mp) =
--     Eval (Not =<< L.Elem k =<< Keys mp)

-- | Null
--
-- === __Example__
--
-- > :kind! Eval (Null =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Null =<< FromList '[ '(5,"a"), '(3,"b")]) :: Bool
-- = 'False
-- > :kind! Eval (Null =<< Empty)
-- Eval (Null =<< Empty) :: Bool
-- = 'True
-- data Null :: MapC k v -> Exp Bool
-- type instance Eval (Null ('MapC '[])) = 'True
-- type instance Eval (Null ('MapC (_ ': _))) = 'False

-- | Size
--
-- === __Example__
--
-- >>> :kind! (Size (FromList '[ '(5,"a"), '(3,"b")]))
-- (Size (FromList '[ '(5,"a"), '(3,"b")])) :: Natural
-- = 2
type Size :: MapC k v -> Nat
type family Size map where
  Size 'Tip = 0
  Size ('Bin sz _ _ _ _) = sz

-- | Union
--
-- === __Example__
--
-- > :kind! Eval (Union (Eval (FromList '[ '(5,"a"), '(3,"b")])) (Eval (FromList '[ '(5,"A"), '(7,"c")])) )
-- Eval (Union (Eval (FromList '[ '(5,"a"), '(3,"b")])) (Eval (FromList '[ '(5,"A"), '(7,"c")])) ) :: MapC
--                                                                                                      TL.Natural
--                                                                                                      TL.Symbol
-- = 'MapC '[ '(7, "c"), '(5, "a"), '(3, "b")]
-- data Union :: MapC k v -> MapC k v -> Exp (MapC k v)
-- type instance Eval (Union ('MapC lst1) ('MapC lst2)) =
--     'MapC (Eval (Fcf.Foldr UComb lst1 lst2))

-- data UComb :: (k,v) -> [(k,v)] -> Exp [(k,v)]
-- type instance Eval (UComb '(k,v) lst) =
--     If (Eval (L.Elem k =<< Fcf.Map Fst lst))
--         lst
--         ('(k,v) ': lst)


-- | Difference
--
-- === __Example__
--
-- > :kind! Eval (Difference (Eval (FromList '[ '(3,"a"), '(5,"b")])) (Eval (FromList '[ '(5,"B"), '(7,"C")])))
-- Eval (Difference (Eval (FromList '[ '(3,"a"), '(5,"b")])) (Eval (FromList '[ '(5,"B"), '(7,"C")]))) :: MapC
--                                                                                                          TL.Natural
--                                                                                                          TL.Symbol
-- = 'MapC '[ '(3, "a")]
-- data Difference :: MapC k v -> MapC k v -> Exp (MapC k v)
-- type instance Eval (Difference mp1 mp2) =
--     Eval (FilterWithKey (DiffNotMem mp2) mp1)

-- -- helper
-- data DiffNotMem :: MapC k v -> k -> v -> Exp Bool
-- type instance Eval (DiffNotMem mp k _) =
--     Eval (Not =<< L.Elem k =<< Keys mp)


-- | Intersection
--
-- === __Example__
--
-- > :kind! Eval (Intersection (Eval (FromList '[ '(3,"a"), '(5,"b")])) (Eval (FromList '[ '(5,"B"), '(7,"C")])))
-- Eval (Intersection (Eval (FromList '[ '(3,"a"), '(5,"b")])) (Eval (FromList '[ '(5,"B"), '(7,"C")]))) :: MapC
--                                                                                                            TL.Natural
--                                                                                                            TL.Symbol
-- = 'MapC '[ '(5, "b")]
-- data Intersection :: MapC k v -> MapC k v -> Exp (MapC k v)
-- type instance Eval (Intersection mp1 mp2) =
--     Eval (FilterWithKey (InterMem mp2) mp1)

-- -- helper
-- data InterMem :: MapC k v -> k -> v -> Exp Bool
-- type instance Eval (InterMem mp k _) = Eval (L.Elem k =<< Keys mp)


-- | Disjoint
--
-- === __Example__
--
-- > :kind! Eval (Disjoint (Eval (FromList '[ '(3,"a"), '(5,"b")])) (Eval (FromList '[ '(5,"B"), '(7,"C")])))
-- Eval (Disjoint (Eval (FromList '[ '(3,"a"), '(5,"b")])) (Eval (FromList '[ '(5,"B"), '(7,"C")]))) :: Bool
-- = 'False
--
-- > :kind! Eval (Disjoint (Eval (FromList '[ '(3,"a"), '(5,"b")])) (Eval (FromList '[ '(2,"B"), '(7,"C")])))
-- Eval (Disjoint (Eval (FromList '[ '(3,"a"), '(5,"b")])) (Eval (FromList '[ '(2,"B"), '(7,"C")]))) :: Bool
-- = 'True
-- > :kind! Eval (Disjoint (Eval Empty) (Eval Empty))
-- Eval (Disjoint (Eval Empty) (Eval Empty)) :: Bool
-- = 'True
-- data Disjoint :: MapC k v -> MapC k v -> Exp Bool
-- type instance Eval (Disjoint mp1 mp2) =
--     Eval (Null =<< Intersection mp1 mp2)


-- | Map
--
-- === __Example__
--
-- > :kind! Eval (Fcf.Data.MapC.Map (AppendSymbol "x") =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Fcf.Data.MapC.Map (AppendSymbol "x") =<< FromList '[ '(5,"a"), '(3,"b")]) :: MapC
--                                                                                      TL.Natural
--                                                                                      TL.Symbol
-- = 'MapC '[ '(5, "xa"), '(3, "xb")]
-- data Map :: (v -> Exp w) -> MapC k v -> Exp (MapC k w)
-- type instance Eval (Map f mp) =
--     'MapC (Eval (Fcf.Map (Second f) =<< Assocs mp))


-- | MapWithKey
--
-- === __Example__
--
-- data MapWithKey :: (k -> v -> Exp w) -> MapC k v -> Exp (MapC k w)
-- type instance Eval (MapWithKey f mp) =
--     'MapC (Eval (Fcf.Map (Second (Uncurry f))
--         =<< MWKhelp
--         =<< Assocs mp))

-- data MWKhelp :: [(k,v)] -> Exp [(k,(k,v))]
-- type instance Eval (MWKhelp '[]) = '[]
-- type instance Eval (MWKhelp ('(k,v) ': rst)) = '(k, '(k,v)) : Eval (MWKhelp rst)


-- | Foldr
--
-- Fold the values in the map using the given right-associative binary operator,
-- such that 'foldr f z == foldr f z . elems'.
--
-- Note: the order of values in MapC is not well defined at the moment.
--
-- === __Example__
--
-- > :kind! Eval (Fcf.Data.MapC.Foldr (+) 0  =<< (FromList '[ '(1,1), '(2,2)]))
-- Eval (Fcf.Data.MapC.Foldr (+) 0  =<< (FromList '[ '(1,1), '(2,2)])) :: TL.Natural
-- = 3
-- data Foldr :: (v -> w -> Exp w) -> w -> MapC k v -> Exp w
-- type instance Eval (Foldr f w mp) = Eval (Fcf.Foldr f w =<< Elems mp)


-- | Elems
--
-- === __Example__
--
-- > :kind! Eval (Elems =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Elems =<< FromList '[ '(5,"a"), '(3,"b")]) :: [TL.Symbol]
-- = '["a", "b"]
-- > :kind! Eval (Elems =<< Empty)
-- Eval (Elems =<< Empty) :: [v]
-- = '[]
-- data Elems :: MapC k v -> Exp [v]
-- type instance Eval (Elems ('MapC lst)) = Eval (Fcf.Map Snd lst)

-- | Keys
--
-- === __Example__
--
-- > :kind! Eval (Keys =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Keys =<< FromList '[ '(5,"a"), '(3,"b")]) :: [TL.Natural]
-- = '[5, 3]
-- > :kind! Eval (Keys =<< Empty)
-- Eval (Keys =<< Empty) :: [k]
-- = '[]
-- data Keys :: MapC k v -> Exp [k]
-- type instance Eval (Keys ('MapC lst)) = Eval (Fcf.Map Fst lst)

-- | Assocs
--
-- === __Example__
--
-- > :kind! Eval (Assocs =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Assocs =<< FromList '[ '(5,"a"), '(3,"b")]) :: [(TL.Natural,
--                                                        TL.Symbol)]
-- = '[ '(5, "a"), '(3, "b")]
-- > :kind! Eval (Assocs =<< Empty)
-- Eval (Assocs =<< Empty) :: [(k, v)]
-- = '[]
-- data Assocs :: MapC k v -> Exp [(k,v)]
-- type instance Eval (Assocs ('MapC lst)) = lst

-- | ToList
--
-- === __Example__
--
-- > :kind! Eval (ToList =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (ToList =<< FromList '[ '(5,"a"), '(3,"b")]) :: [(TL.Natural,
--                                                        TL.Symbol)]
-- = '[ '(5, "a"), '(3, "b")]
-- > :kind! Eval (ToList =<< Empty)
-- Eval (ToList =<< Empty) :: [(k, v)]
-- = '[]
-- data ToList :: MapC k v -> Exp [(k,v)]
-- type instance Eval (ToList ('MapC lst)) = lst

-- | Filter
--
-- === __Example__
--
-- > :kind! Eval (Filter ((>=) 35) =<< FromList '[ '(5,50), '(3,30)])
-- Eval (Filter ((>=) 35) =<< FromList '[ '(5,50), '(3,30)]) :: MapC
--                                                                TL.Natural TL.Natural
-- = 'MapC '[ '(3, 30)]
-- data Filter :: (v -> Exp Bool) -> MapC k v -> Exp (MapC k v)
-- type instance Eval (Filter f ('MapC lst)) =
--     'MapC (Eval (Fcf.Filter (f <=< Snd) lst))

-- | FilterWithKey
--
-- === __Example__
--
-- > :kind! Eval (FilterWithKey (>=) =<< FromList '[ '(3,5), '(6,4)])
-- Eval (FilterWithKey (>=) =<< FromList '[ '(3,5), '(6,4)]) :: MapC
--                                                                TL.Natural TL.Natural
-- = 'MapC '[ '(6, 4)]
-- data FilterWithKey :: (k -> v -> Exp Bool) -> MapC k v -> Exp (MapC k v)
-- type instance Eval (FilterWithKey f ('MapC lst)) =
--     'MapC (Eval (Fcf.Filter (Uncurry f) lst))

-- | Partition
--
-- === __Example__
--
-- > :kind! Eval (Partition ((>=) 35) =<< FromList '[ '(5,50), '(3,30)])
-- Eval (Partition ((>=) 35) =<< FromList '[ '(5,50), '(3,30)]) :: (MapC
--                                                                    TL.Natural TL.Natural,
--                                                                  MapC TL.Natural TL.Natural)
-- = '( 'MapC '[ '(3, 30)], 'MapC '[ '(5, 50)])
-- data Partition :: (v -> Exp Bool) -> MapC k v -> Exp (MapC k v, MapC k v)
-- type instance Eval (Partition f ('MapC lst)) =
--     Eval (PartitionHlp (Eval (L.Partition (f <=< Snd) lst)))

-- data PartitionHlp :: ([(k,v)],[(k,v)]) -> Exp (MapC k v, MapC k v)
-- type instance Eval (PartitionHlp '(xs,ys)) = '( 'MapC xs, 'MapC ys)

--------------------------------------------------------------------------------
-- Balance

-- | Type level implementation of balanceR from Data.Map.Strict.Internal
--
-- balanceR :: k -> a -> Map k a -> Map k a -> Map k a
-- balanceR k x l r = case l of
--   Tip -> case r of
--            Tip -> Bin 1 k x Tip Tip
--            (Bin _ _ _ Tip Tip) -> Bin 2 k x Tip r
--            (Bin _ rk rx Tip rr@(Bin _ _ _ _ _)) -> Bin 3 rk rx (Bin 1 k x Tip Tip) rr
--            (Bin _ rk rx (Bin _ rlk rlx _ _) Tip) -> Bin 3 rlk rlx (Bin 1 k x Tip Tip) (Bin 1 rk rx Tip Tip)
--            (Bin rs rk rx rl@(Bin rls rlk rlx rll rlr) rr@(Bin rrs _ _ _ _))
--              | rls < ratio*rrs -> Bin (1+rs) rk rx (Bin (1+rls) k x Tip rl) rr
--              | otherwise -> Bin (1+rs) rlk rlx (Bin (1+size rll) k x Tip rll) (Bin (1+rrs+size rlr) rk rx rlr rr)
--
--   (Bin ls _ _ _ _) -> case r of
--            Tip -> Bin (1+ls) k x l Tip
--
--            (Bin rs rk rx rl rr)
--               | rs > delta*ls  -> case (rl, rr) of
--                    (Bin rls rlk rlx rll rlr, Bin rrs _ _ _ _)
--                      | rls < ratio*rrs -> Bin (1+ls+rs) rk rx (Bin (1+ls+rls) k x l rl) rr
--                      | otherwise -> Bin (1+ls+rs) rlk rlx (Bin (1+ls+size rll) k x l rll) (Bin (1+rrs+size rlr) rk rx rlr rr)
--                    (_, _) -> error "Failure in Data.Map.balanceR"
--               | otherwise -> Bin (1+ls+rs) k x l r 
--
type BalanceR :: 
     k -- ^ key
  -> a -- ^ value
  -> MapC k a -- ^ left Map 
  -> MapC k a -- ^ right Map
  -> MapC k a
type family BalanceR k x l r where
  BalanceR k x 'Tip 'Tip = 'Bin 1 k x 'Tip 'Tip
  BalanceR k x 'Tip ('Bin rs rk rx 'Tip 'Tip) = 'Bin 2 k x 'Tip ('Bin rs rk rx 'Tip 'Tip)
  BalanceR k x 'Tip ('Bin _ rk rx 'Tip ('Bin rrs rrk rrx rrl rrr)) = 'Bin 3 rk rx ('Bin 1 k x 'Tip 'Tip) ('Bin rrs rrk rrx rrl rrr)
  BalanceR k x 'Tip ('Bin _ rk rx ('Bin _ rlk rlx _ _) 'Tip) = 'Bin 3 rlk rlx ('Bin 1 k x 'Tip 'Tip) ('Bin 1 rk rx 'Tip 'Tip)
  BalanceR k x 'Tip ('Bin rs rk rx ('Bin rls rlk rlx rll rlr) ('Bin rrs rrk rrx rrl rrr)) =
    If (Eval (rls < (Ratio Nat.* rrs)))
       ('Bin (1+rs) rk rx ('Bin (1+rls) k x 'Tip ('Bin rls rlk rlx rll rlr)) ('Bin rrs rrk rrx rrl rrr))
       ('Bin (1+rs) rlk rlx ('Bin (1+Size rll) k x 'Tip rll) ('Bin (1+rrs+ Size rlr) rk rx rlr ('Bin rrs rrk rrx rrl rrr)))
  BalanceR k x ('Bin ls lk lx ll lr) 'Tip = 'Bin (1+ls) k x ('Bin ls lk lx ll lr) 'Tip
  BalanceR k x ('Bin ls lk lx ll lr) ('Bin rs rk rx ('Bin rls rlk rlx rll rlr) ('Bin rrs rrk rrx rrl rrr)) =
    If (Eval (rs > (Delta Nat.* ls)))
       (If (Eval (rls < (Ratio Nat.* rrs)))
           ('Bin (1+ls+rs) rk rx ('Bin (1+ls+rls) k x ('Bin ls lk lx ll lr) ('Bin rls rlk rlx rll rlr)) ('Bin rrs rrk rrx rrl rrr))
           ('Bin (1+ls+rs) rlk rlx ('Bin (1+ls+Size rll) k x ('Bin ls lk lx ll lr) rll) ('Bin (1+rrs+Size rlr) rk rx rlr ('Bin rrs rrk rrx rrl rrr))))
       ('Bin (1+ls+rs) k x ('Bin ls lk lx ll lr) ('Bin rs rk rx ('Bin rls rlk rlx rll rlr) ('Bin rrs rrk rrx rrl rrr)))
  BalanceR k x ('Bin ls lk lx ll lr) ('Bin rs rk rx rl rr) =
    If (Eval (rs > (Delta Nat.* ls)))
       (TypeError ('Text "Failure in Fcf.Data.Map.balanceR"))
       ('Bin (1+ls+rs) k x ('Bin ls lk lx ll lr) ('Bin rs rk rx rl rr))

-- | Type level implementation of balanceL from Data.Map.Strict.Internal
--
-- balanceL :: k -> a -> Map k a -> Map k a -> Map k a
-- balanceL k x l r = case r of
--   Tip -> case l of
--            Tip -> Bin 1 k x Tip Tip
--            (Bin _ _ _ Tip Tip) -> Bin 2 k x l Tip
--            (Bin _ lk lx Tip (Bin _ lrk lrx _ _)) -> Bin 3 lrk lrx (Bin 1 lk lx Tip Tip) (Bin 1 k x Tip Tip)
--            (Bin _ lk lx ll@(Bin _ _ _ _ _) Tip) -> Bin 3 lk lx ll (Bin 1 k x Tip Tip)
--            (Bin ls lk lx ll@(Bin lls _ _ _ _) lr@(Bin lrs lrk lrx lrl lrr))
--              | lrs < ratio*lls -> Bin (1+ls) lk lx ll (Bin (1+lrs) k x lr Tip)
--              | otherwise -> Bin (1+ls) lrk lrx (Bin (1+lls+size lrl) lk lx ll lrl) (Bin (1+size lrr) k x lrr Tip)
--
--   (Bin rs _ _ _ _) -> case l of
--            Tip -> Bin (1+rs) k x Tip r
--
--            (Bin ls lk lx ll lr)
--               | ls > delta*rs  -> case (ll, lr) of
--                    (Bin lls _ _ _ _, Bin lrs lrk lrx lrl lrr)
--                      | lrs < ratio*lls -> Bin (1+ls+rs) lk lx ll (Bin (1+rs+lrs) k x lr r)
--                      | otherwise -> Bin (1+ls+rs) lrk lrx (Bin (1+lls+size lrl) lk lx ll lrl) (Bin (1+rs+size lrr) k x lrr r)
--                    (_, _) -> error "Failure in Data.Map.balanceL"
--               | otherwise -> Bin (1+ls+rs) k x l r
--
type BalanceL ::
     k -- ^ key
  -> a -- ^ value
  -> MapC k a -- ^ left map
  -> MapC k a -- ^ left right
  -> MapC k a
type family BalanceL k x l r where
  BalanceL k x 'Tip 'Tip = 'Bin 1 k x 'Tip 'Tip
  BalanceL k x ('Bin ls lk lx 'Tip 'Tip) 'Tip = 'Bin 2 k x ('Bin ls lk lx 'Tip 'Tip) 'Tip
  BalanceL k x ('Bin _ lk lx 'Tip ('Bin _ lrk lrx _ _)) 'Tip = 'Bin 3 lrk lrx ('Bin 1 lk lx 'Tip 'Tip) ('Bin 1 k x 'Tip 'Tip)
  BalanceL k x ('Bin _ lk lx ('Bin lls llk llx lll llr) 'Tip) 'Tip = 'Bin 3 lk lx ('Bin lls llk llx lll llr) ('Bin 1 k x 'Tip 'Tip)
  BalanceL k x ('Bin ls lk lx ('Bin lls llk llx lll llr) ('Bin lrs lrk lrx lrl lrr)) 'Tip =
    If (Eval (lrs < (Ratio Nat.* lls)))
       ('Bin (1+ls) lk lx ('Bin lls llk llx lll llr) ('Bin (1+lrs) k x ('Bin lrs lrk lrx lrl lrr) 'Tip))
       ('Bin (1+ls) lrk lrx ('Bin (1 + lls + Size lrl) lk lx ('Bin lls llk llx lll llr) lrl) ('Bin (1 + Size lrr) k x lrr 'Tip))
  BalanceL k x 'Tip ('Bin rs rk rx rl rr) = 'Bin (1+rs) k x 'Tip ('Bin rs rk rx rl rr)
  BalanceL k x ('Bin ls lk lx ('Bin lls llk llx lll llr) ('Bin lrs lrk lrx lrl lrr)) ('Bin rs rk rx rl rr) = 
    If (Eval (lrs < (Ratio Nat.* lls)))
       (If (Eval (lrs < (Ratio Nat.* lls)))
         ('Bin (1+ls+rs) lk lx ('Bin lls llk llx lll llr) ('Bin (1+rs+lrs) k x ('Bin lrs lrk lrx lrl lrr) ('Bin rs rk rx rl rr)))
         ('Bin (1+ls+rs) lrk lrx ('Bin (1+lls+Size lrl) lk lx ('Bin lls llk llx lll llr) lrl) ('Bin (1+rs+Size lrr) k x lrr ('Bin rs rk rx rl rr))))
       ('Bin (1+ls+rs) k x ('Bin ls lk lx ('Bin lls llk llx lll llr) ('Bin lrs lrk lrx lrl lrr)) ('Bin rs rk rx rl rr))
  BalanceL k x ('Bin ls lk lx ll lr) ('Bin rs rk rx rl rr) = 
    If (Eval (ls > (Delta Nat.* rs)))
       (TypeError ('Text "Failure in Fcf.Data.Map.BalanceL"))
       ('Bin (1+ls+rs) k x ('Bin ls lk lx ll lr) ('Bin rs rk rx rl rr))

data Uncurry1 :: (a -> b -> c -> Exp c) -> (a, b) -> c -> Exp c
type instance Eval (Uncurry1 f '(x, y) c) = Eval (f x y c)

--------------------------------------------------------------------------------
-- Balance

