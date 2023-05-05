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
    -- , InsertWith
    -- , Delete

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
                     , Case, type (-->)
                     , Foldr)
import qualified Fcf (Map, Foldr, Filter)
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
-- >>> import           Fcf (type (>=))
-- >>> import           Fcf.Data.Nat
-- >>> import           Fcf.Data.Symbol

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
-- >>> :kind! (Eval Empty :: MapC Nat TL.Symbol)
-- (Eval Empty :: MapC Nat TL.Symbol) :: MapC TL.Natural TL.Symbol
-- = 'MapC '[]
--
-- >>> :kind! (Eval Empty :: MapC Int String)
-- (Eval Empty :: MapC Int String) :: MapC Int [Char]
-- = 'MapC '[]
--
-- See also the other examples in this module.
type Empty :: MapC k v
type Empty = 'Tip
-- type instance Eval Empty = 'MapC '[]

-- | Singleton
--
-- === __Example__
--
-- >>> :kind! Eval (Singleton 1 "haa")
-- Eval (Singleton 1 "haa") :: MapC TL.Natural TL.Symbol
-- = 'MapC '[ '(1, "haa")]
data Singleton :: k -> v -> Exp (MapC k v)
type instance Eval (Singleton k v) = 'Bin 1 k v 'Tip 'Tip

-- | Use FromList to construct a MapC from type-level list.
--
-- === __Example__
--
-- >>> :kind! Eval (FromList '[ '(1,"haa"), '(2,"hoo")])
-- Eval (FromList '[ '(1,"haa"), '(2,"hoo")]) :: MapC
--                                                 TL.Natural TL.Symbol
-- = 'MapC '[ '(1, "haa"), '(2, "hoo")]
data FromList :: [(k,v)] -> Exp (MapC k v)
type instance Eval (FromList lst) = Foldr (Uncurry1 Insert) Empty @@ lst

-- fromList :: Ord k => [(k,a)] -> Map k a
-- fromList [] = Tip
-- fromList [(kx, x)] = x `seq` Bin 1 kx x Tip Tip
-- fromList ((kx0, x0) : xs0) | not_ordered kx0 xs0 = x0 `seq` fromList' (Bin 1 kx0 x0 Tip Tip) xs0
--                            | otherwise = x0 `seq` go (1::Int) (Bin 1 kx0 x0 Tip Tip) xs0
--   where
--     not_ordered _ [] = False
--     not_ordered kx ((ky,_) : _) = kx >= ky
--     {-# INLINE not_ordered #-}

--     fromList' t0 xs = Foldable.foldl' ins t0 xs
--       where ins t (k,x) = insert k x t

--     go !_ t [] = t
--     go _ t [(kx, x)] = x `seq` insertMax kx x t
--     go s l xs@((kx, x) : xss) | not_ordered kx xss = fromList' l xs
--                               | otherwise = case create s xss of
--                                   (r, ys, []) -> x `seq` go (s `shiftL` 1) (link kx x l r) ys
--                                   (r, _,  ys) -> x `seq` fromList' (link kx x l r) ys

--     -- The create is returning a triple (tree, xs, ys). Both xs and ys
--     -- represent not yet processed elements and only one of them can be nonempty.
--     -- If ys is nonempty, the keys in ys are not ordered with respect to tree
--     -- and must be inserted using fromList'. Otherwise the keys have been
--     -- ordered so far.
--     create !_ [] = (Tip, [], [])
--     create s xs@(xp : xss)
--       | s == 1 = case xp of (kx, x) | not_ordered kx xss -> x `seq` (Bin 1 kx x Tip Tip, [], xss)
--                                     | otherwise -> x `seq` (Bin 1 kx x Tip Tip, xss, [])
--       | otherwise = case create (s `shiftR` 1) xs of
--                       res@(_, [], _) -> res
--                       (l, [(ky, y)], zs) -> y `seq` (insertMax ky y l, [], zs)
--                       (l, ys@((ky, y):yss), _) | not_ordered ky yss -> (l, [], ys)
--                                                | otherwise -> case create (s `shiftR` 1) yss of
--                                                    (r, zs, ws) -> y `seq` (link ky y l r, zs, ws)

-- type instance Eval (FromList lst) = Foldr (Uncurry1 Insert) Empty @@ lst

-- | Insert
--
-- === __Example__
--
-- >>> :kind! Eval (Insert 3 "hih" =<< FromList '[ '(1,"haa"), '(2,"hoo")])
-- Eval (Insert 3 "hih" =<< FromList '[ '(1,"haa"), '(2,"hoo")]) :: MapC
--                                                                    TL.Natural TL.Symbol
-- = 'MapC '[ '(3, "hih"), '(1, "haa"), '(2, "hoo")]
data Insert :: k -> v -> MapC k v -> Exp (MapC k v)
type instance Eval (Insert k v map) = InsertGo k v map

type InsertGo :: k -> v -> MapC k v -> MapC k v
type family InsertGo kx x map where
  InsertGo kx x 'Tip = Eval (Singleton kx x)
  InsertGo kx x ('Bin sz ky y l r) =
    Case [ 'LT --> BalanceL ky y (InsertGo kx x l) r
         , 'GT --> BalanceR ky y l (InsertGo kx x r)
         , 'EQ --> 'Bin sz kx x l r
         ] @@ Eval (Compare kx ky)


--------------------------------------------------------------------------------


-- | InsertWith
-- if old there, map
-- if no old, add
--
-- === __Example__
--
-- >>> :kind! Eval (InsertWith AppendSymbol 5 "xxx" =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (InsertWith AppendSymbol 5 "xxx" =<< FromList '[ '(5,"a"), '(3,"b")]) :: MapC
--                                                                                 TL.Natural TL.Symbol
-- = 'MapC '[ '(5, "xxxa"), '(3, "b")]
--
-- >>> :kind! Eval (InsertWith AppendSymbol 7 "xxx" =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (InsertWith AppendSymbol 7 "xxx" =<< FromList '[ '(5,"a"), '(3,"b")]) :: MapC
--                                                                                 TL.Natural TL.Symbol
-- = 'MapC '[ '(5, "a"), '(3, "b"), '(7, "xxx")]
-- >>> :kind! Eval (InsertWith AppendSymbol 7 "xxx" =<< Empty)
-- Eval (InsertWith AppendSymbol 7 "xxx" =<< Empty) :: MapC
--                                                       TL.Natural TL.Symbol
-- = 'MapC '[ '(7, "xxx")]
-- data InsertWith :: (v -> v -> Exp v) -> k -> v -> MapC k v -> Exp (MapC k v)
-- type instance Eval (InsertWith f k v ('MapC lst)) =
--     If (Eval (L.Elem k =<< Fcf.Map Fst lst))
--         ('MapC (Eval (Fcf.Map (InsWithHelp f k v) lst)))
--         ('MapC (Eval (lst ++ '[ '(k,v)])))

-- helper
-- data InsWithHelp :: (v -> v -> Exp v) -> k -> v -> (k,v) -> Exp (k,v)
-- type instance Eval (InsWithHelp f k1 vNew '(k2,vOld)) =
--     If (Eval (TyEq k1 k2))
--         '(k1, Eval (f vNew vOld))
--         '(k2, vOld)


-- | Delete
--
-- === __Example__
--
-- >>> :kind! Eval (Delete 5 =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Delete 5 =<< FromList '[ '(5,"a"), '(3,"b")]) :: MapC
--                                                          TL.Natural TL.Symbol
-- = 'MapC '[ '(3, "b")]
--
-- >>> :kind! Eval (Delete 7 =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Delete 7 =<< FromList '[ '(5,"a"), '(3,"b")]) :: MapC
--                                                          TL.Natural TL.Symbol
-- = 'MapC '[ '(5, "a"), '(3, "b")]
--
-- >>> :kind! Eval (Delete 7 =<< Empty)
-- Eval (Delete 7 =<< Empty) :: MapC TL.Natural v
-- = 'MapC '[]
-- data Delete :: k -> MapC k v -> Exp (MapC k v)
-- type instance Eval (Delete k ('MapC lst)) =
--     'MapC (Eval (Fcf.Filter (Not <=< TyEq k <=< Fst) lst))

-- | Adjust
--
-- === __Example__
--
-- >>> :kind! Eval (Adjust (AppendSymbol "new ") 5 =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Adjust (AppendSymbol "new ") 5 =<< FromList '[ '(5,"a"), '(3,"b")]) :: MapC
--                                                                                TL.Natural TL.Symbol
-- = 'MapC '[ '(5, "new a"), '(3, "b")]
--
-- >>> :kind! Eval (Adjust (AppendSymbol "new ") 7 =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Adjust (AppendSymbol "new ") 7 =<< FromList '[ '(5,"a"), '(3,"b")]) :: MapC
--                                                                                TL.Natural TL.Symbol
-- = 'MapC '[ '(5, "a"), '(3, "b")]
--
-- >>> :kind! Eval (Adjust (AppendSymbol "new ") 7 =<< Empty)
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
-- >>> :kind! Eval (Lookup 5 =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Lookup 5 =<< FromList '[ '(5,"a"), '(3,"b")]) :: Maybe
--                                                          TL.Symbol
-- = 'Just "a"
--
-- >>> :kind! Eval (Lookup 7 =<< FromList '[ '(5,"a"), '(3,"b")])
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
-- >>> :kind! Eval (Member 5 =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Member 5 =<< FromList '[ '(5,"a"), '(3,"b")]) :: Bool
-- = 'True
-- >>> :kind! Eval (Member 7 =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Member 7 =<< FromList '[ '(5,"a"), '(3,"b")]) :: Bool
-- = 'False
-- data Member :: k -> MapC k v -> Exp Bool
-- type instance Eval (Member k mp) =
--     Eval (L.Elem k =<< Keys mp)

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
-- data NotMember :: k -> MapC k v -> Exp Bool
-- type instance Eval (NotMember k mp) =
--     Eval (Not =<< L.Elem k =<< Keys mp)

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
-- data Null :: MapC k v -> Exp Bool
-- type instance Eval (Null ('MapC '[])) = 'True
-- type instance Eval (Null ('MapC (_ ': _))) = 'False

-- | Size
--
-- === __Example__
--
-- >>> :kind! Eval (Size =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Size =<< FromList '[ '(5,"a"), '(3,"b")]) :: TL.Natural
-- = 2
type Size :: MapC k v -> Nat
type family Size map where
  Size 'Tip = 0
  Size ('Bin sz _ _ _ _) = sz

-- | Union
--
-- === __Example__
--
-- >>> :kind! Eval (Union (Eval (FromList '[ '(5,"a"), '(3,"b")])) (Eval (FromList '[ '(5,"A"), '(7,"c")])) )
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
-- >>> :kind! Eval (Difference (Eval (FromList '[ '(3,"a"), '(5,"b")])) (Eval (FromList '[ '(5,"B"), '(7,"C")])))
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
-- >>> :kind! Eval (Intersection (Eval (FromList '[ '(3,"a"), '(5,"b")])) (Eval (FromList '[ '(5,"B"), '(7,"C")])))
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
-- data Disjoint :: MapC k v -> MapC k v -> Exp Bool
-- type instance Eval (Disjoint mp1 mp2) =
--     Eval (Null =<< Intersection mp1 mp2)


-- | Map
--
-- === __Example__
--
-- >>> :kind! Eval (Fcf.Data.MapC.Map (AppendSymbol "x") =<< FromList '[ '(5,"a"), '(3,"b")])
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
-- >>> :kind! Eval (Fcf.Data.MapC.Foldr (+) 0  =<< (FromList '[ '(1,1), '(2,2)]))
-- Eval (Fcf.Data.MapC.Foldr (+) 0  =<< (FromList '[ '(1,1), '(2,2)])) :: TL.Natural
-- = 3
-- data Foldr :: (v -> w -> Exp w) -> w -> MapC k v -> Exp w
-- type instance Eval (Foldr f w mp) = Eval (Fcf.Foldr f w =<< Elems mp)


-- | Elems
--
-- === __Example__
--
-- >>> :kind! Eval (Elems =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Elems =<< FromList '[ '(5,"a"), '(3,"b")]) :: [TL.Symbol]
-- = '["a", "b"]
-- >>> :kind! Eval (Elems =<< Empty)
-- Eval (Elems =<< Empty) :: [v]
-- = '[]
-- data Elems :: MapC k v -> Exp [v]
-- type instance Eval (Elems ('MapC lst)) = Eval (Fcf.Map Snd lst)

-- | Keys
--
-- === __Example__
--
-- >>> :kind! Eval (Keys =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Keys =<< FromList '[ '(5,"a"), '(3,"b")]) :: [TL.Natural]
-- = '[5, 3]
-- >>> :kind! Eval (Keys =<< Empty)
-- Eval (Keys =<< Empty) :: [k]
-- = '[]
-- data Keys :: MapC k v -> Exp [k]
-- type instance Eval (Keys ('MapC lst)) = Eval (Fcf.Map Fst lst)

-- | Assocs
--
-- === __Example__
--
-- >>> :kind! Eval (Assocs =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (Assocs =<< FromList '[ '(5,"a"), '(3,"b")]) :: [(TL.Natural,
--                                                        TL.Symbol)]
-- = '[ '(5, "a"), '(3, "b")]
-- >>> :kind! Eval (Assocs =<< Empty)
-- Eval (Assocs =<< Empty) :: [(k, v)]
-- = '[]
-- data Assocs :: MapC k v -> Exp [(k,v)]
-- type instance Eval (Assocs ('MapC lst)) = lst

-- | ToList
--
-- === __Example__
--
-- >>> :kind! Eval (ToList =<< FromList '[ '(5,"a"), '(3,"b")])
-- Eval (ToList =<< FromList '[ '(5,"a"), '(3,"b")]) :: [(TL.Natural,
--                                                        TL.Symbol)]
-- = '[ '(5, "a"), '(3, "b")]
-- >>> :kind! Eval (ToList =<< Empty)
-- Eval (ToList =<< Empty) :: [(k, v)]
-- = '[]
-- data ToList :: MapC k v -> Exp [(k,v)]
-- type instance Eval (ToList ('MapC lst)) = lst

-- | Filter
--
-- === __Example__
--
-- >>> :kind! Eval (Filter ((>=) 35) =<< FromList '[ '(5,50), '(3,30)])
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
-- >>> :kind! Eval (FilterWithKey (>=) =<< FromList '[ '(3,5), '(6,4)])
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
-- >>> :kind! Eval (Partition ((>=) 35) =<< FromList '[ '(5,50), '(3,30)])
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

