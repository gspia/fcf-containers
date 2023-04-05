{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

{-|
Module      : Fcf.Data.Reflect
Description : List helpers / utils
Copyright   : (c) gspia 2023-
License     : BSD
Maintainer  : gspia

= Fcf.Data.Reflect

Helpers to get results from type-level computations into the fromType-level.

-}

--------------------------------------------------------------------------------

module Fcf.Data.Reflect where

import qualified GHC.TypeLits as TL
import           GHC.TypeLits (Nat, Symbol, KnownNat, KnownSymbol)
import           Data.Proxy
import qualified Data.Map.Strict as MS
import qualified Data.IntMap.Strict as IMS
import qualified Data.Set as S
#if __GLASGOW_HASKELL__ >= 902
import qualified Data.Text as Txt
#endif
import qualified Data.Tree as T

-- import qualified Fcf.Core as C (Eval)
import qualified Fcf.Data.MapC as MC
import qualified Fcf.Data.NatMap as NM
import qualified Fcf.Data.Set as FS
#if __GLASGOW_HASKELL__ >= 902
import qualified Fcf.Data.NewText as FTxt
#endif
import qualified Fcf.Data.Tree as FT

--------------------------------------------------------------------------------

-- For the doctests:

-- $setup
-- >>> import qualified GHC.TypeLits as TL
-- >>> import           Fcf.Data.Nat

--------------------------------------------------------------------------------


-- | Reflect a list of Nats
--
-- Note that you may also use the KnownVal methods given below.
--
-- This method is taken from
-- https://hackage.haskell.org/package/numhask-array-0.10.1/docs/src/NumHask.Array.Shape.html#natVals
--
-- === __Example__
--
-- >>> :{
-- afun :: forall n. (n ~ '[1,2,3,4]) => [Int]
-- afun = natVals @n Proxy
-- :}
--
-- >>> afun
-- [1,2,3,4]
class KnownNats (ns :: [Nat]) where
  natVals :: Proxy ns -> [Int]

instance KnownNats '[] where
  natVals _ = []

instance (TL.KnownNat n, KnownNats ns) => KnownNats (n : ns) where
  natVals _ = fromInteger (TL.natVal (Proxy @n)) : natVals (Proxy @ns)


--------------------------------------------------------------------------------

class KnownVal typeval val where
    fromType :: Proxy typeval -> val

instance KnownNat n => KnownVal (n :: Nat) Integer where
    fromType _ = TL.natVal (Proxy @n)

instance KnownNat n => KnownVal (n :: Nat) Int where
    fromType _ = fromInteger $ TL.natVal (Proxy @n)

instance KnownSymbol s => KnownVal (s :: Symbol) String where
    fromType _ = TL.symbolVal (Proxy @s)

--------------------------------------------------------------------------------

-- List instances

instance KnownVal ('[] :: [Nat]) [Integer] where
    fromType _ = []

instance (KnownNat n, KnownVal ns [Integer]) => KnownVal (n : ns :: [Nat]) [Integer] where
    fromType _ = TL.natVal (Proxy @n) : fromType (Proxy @ns)


instance KnownVal ('[] :: [Nat]) [Int] where
    fromType _ = []

instance (KnownNat n, KnownVal ns [Int]) => KnownVal (n : ns :: [Nat]) [Int] where
    fromType _ = fromInteger (TL.natVal (Proxy @n)) : fromType (Proxy @ns)


instance KnownVal ('[] :: [Symbol]) [String] where
    fromType _ = []

instance (KnownSymbol sym, KnownVal syms [String])
    => KnownVal (sym : syms :: [Symbol]) [String]
  where
    fromType _ = TL.symbolVal (Proxy @sym) : fromType (Proxy @syms)


instance KnownVal ('[] :: [(Nat,Nat)]) [(Int,Int)] where
    fromType _ = []
-- This helps with NatMap instances

instance (KnownNat n, KnownNat m, KnownVal nms [(Int,Int)])
    => KnownVal ( '(n,m) : nms :: [(Nat,Nat)]) [(Int,Int)]
  where
    fromType _ =
        (fromInteger (TL.natVal (Proxy @n)), fromInteger (TL.natVal (Proxy @m)))
        : fromType (Proxy @nms)
-- This helps with NatMap instances

instance KnownVal ('[] :: [(Nat,Symbol)]) [(Int,String)] where
    fromType _ = []
-- This helps with NatMap instances

instance (KnownNat n, KnownSymbol m, KnownVal nms [(Int,String)])
    => KnownVal ( '(n,m) : nms :: [(Nat,Symbol)]) [(Int,String)]
  where
    fromType _ =
        (fromInteger (TL.natVal (Proxy @n)), TL.symbolVal (Proxy @m))
        : fromType (Proxy @nms)
-- This helps with NatMap instances

--------------------------------------------------------------------------------

-- Trees

-- instances for Forests, that is, lists of Trees.
instance KnownVal '[] [T.Tree Int] where fromType _ = []

-- instances for Forests
instance (KnownVal t (T.Tree Int), KnownVal trees [T.Tree Int])
    => KnownVal (t : trees) [T.Tree Int]
  where
    fromType _ = fromType @t Proxy : fromType @trees Proxy

-- instance for Trees (using forest definition).
instance (KnownNat n, KnownVal trees [T.Tree Int])
    => KnownVal ('FT.Node (n :: Nat) trees) (T.Tree Int)
  where
    fromType _ = T.Node (fromType @n Proxy) (fromType @trees Proxy)


instance KnownVal '[] [T.Tree Integer] where fromType _ = []

instance (KnownVal t (T.Tree Integer), KnownVal trees [T.Tree Integer])
    => KnownVal (t : trees) [T.Tree Integer]
  where
    fromType _ = fromType @t Proxy : fromType @trees Proxy

instance (KnownNat n, KnownVal trees [T.Tree Integer])
    => KnownVal ('FT.Node (n :: Nat) trees) (T.Tree Integer)
  where
    fromType _ = T.Node (fromType @n Proxy) (fromType @trees Proxy)


instance KnownVal '[] [T.Tree String] where fromType _ = []

instance (KnownVal t (T.Tree String), KnownVal trees [T.Tree String])
    => KnownVal (t : trees) [T.Tree String]
  where
    fromType _ = fromType @t Proxy : fromType @trees Proxy

instance (KnownSymbol n, KnownVal trees [T.Tree String])
    => KnownVal ('FT.Node (n :: Symbol) trees) (T.Tree String)
  where
    fromType _ = T.Node (fromType @n Proxy) (fromType @trees Proxy)


--------------------------------------------------------------------------------

-- NatMaps / IntMaps

instance (KnownVal (pairs :: [(Nat,Nat)]) [(Int,Int)])
    => KnownVal ('NM.NatMap pairs) (IMS.IntMap Int)
  where
    fromType _ = IMS.fromList (fromType @pairs Proxy)

instance (KnownVal (pairs :: [(Nat,Integer)]) [(Int,Integer)])
    => KnownVal ('NM.NatMap pairs) (IMS.IntMap Integer)
  where
    fromType _ = IMS.fromList (fromType @pairs Proxy)

instance (KnownVal (pairs :: [(Nat,Symbol)]) [(Int,String)])
    => KnownVal ('NM.NatMap pairs) (IMS.IntMap String)
  where
    fromType _ = IMS.fromList (fromType @pairs Proxy)


--------------------------------------------------------------------------------

-- Maps

instance (KnownVal (pairs :: [(Nat,Nat)]) [(Int,Int)])
    => KnownVal ('MC.MapC pairs) (MS.Map Int Int)
  where
    fromType _ = MS.fromList (fromType @pairs Proxy)

instance (KnownVal (pairs :: [(Nat,Integer)]) [(Int,Integer)])
    => KnownVal ('MC.MapC pairs) (MS.Map Int Integer)
  where
    fromType _ = MS.fromList (fromType @pairs Proxy)

instance (KnownVal (pairs :: [(Nat,Symbol)]) [(Int,String)])
    => KnownVal ('MC.MapC pairs) (MS.Map Int String)
  where
    fromType _ = MS.fromList (fromType @pairs Proxy)


instance (KnownVal (pairs :: [(Symbol,Nat)]) [(String,Int)])
    => KnownVal ('MC.MapC pairs) (MS.Map String Int)
  where
    fromType _ = MS.fromList (fromType @pairs Proxy)

instance (KnownVal (pairs :: [(Symbol,Integer)]) [(String,Integer)])
    => KnownVal ('MC.MapC pairs) (MS.Map String Integer)
  where
    fromType _ = MS.fromList (fromType @pairs Proxy)

instance (KnownVal (pairs :: [(Symbol,Symbol)]) [(String,String)])
    => KnownVal ('MC.MapC pairs) (MS.Map String String)
  where
    fromType _ = MS.fromList (fromType @pairs Proxy)


--------------------------------------------------------------------------------

-- Set
 
instance (KnownVal (mems :: [Nat]) [Int]) => KnownVal ('FS.Set mems) (S.Set Int)
  where
    fromType _ = S.fromList (fromType @mems Proxy)

instance (KnownVal (mems :: [Nat]) [Integer]) => KnownVal ('FS.Set mems) (S.Set Integer)
  where
    fromType _ = S.fromList (fromType @mems Proxy)

instance (KnownVal (mems :: [Symbol]) [String]) => KnownVal ('FS.Set mems) (S.Set String)
  where
    fromType _ = S.fromList (fromType @mems Proxy)

--------------------------------------------------------------------------------

-- Either

instance (KnownVal a a1) => KnownVal ('Left a) (Either a1 b1) where
    fromType _ = Left (fromType @a Proxy :: a1)

instance (KnownVal b b1) => KnownVal ('Right b) (Either a1 b1) where
    fromType _ = Right (fromType @b Proxy :: b1)

--------------------------------------------------------------------------------

-- Maybe

instance (KnownVal a a1) => KnownVal ('Just a) (Maybe a1) where
    fromType _ = Just (fromType @a Proxy :: a1)

instance KnownVal 'Nothing (Maybe a1) where
    fromType _ = Nothing

--------------------------------------------------------------------------------

-- Tuples

instance (KnownVal a a1, KnownVal b b1) => KnownVal '(a,b) (a1,b1) where
    fromType _ = (fromType @a Proxy :: a1, fromType @b Proxy :: b1)

instance (KnownVal a a1, KnownVal b b1, KnownVal c c1) => KnownVal '(a,b,c) (a1,b1,c1) where
    fromType _ = (fromType @a Proxy :: a1, fromType @b Proxy :: b1, fromType @c Proxy :: c1)

instance (KnownVal a a1, KnownVal b b1, KnownVal c c1, KnownVal d d1) => KnownVal '(a,b,c,d) (a1,b1,c1,d1) where
    fromType _ = (fromType @a Proxy :: a1, fromType @b Proxy :: b1, fromType @c Proxy :: c1, fromType @d Proxy :: d1)

instance (KnownVal a a1, KnownVal b b1, KnownVal c c1, KnownVal d d1, KnownVal e e1) => KnownVal '(a,b,c,d,e) (a1,b1,c1,d1,e1) where
    fromType _ = (fromType @a Proxy :: a1, fromType @b Proxy :: b1, fromType @c Proxy :: c1, fromType @d Proxy :: d1, fromType @e Proxy :: e1)
--------------------------------------------------------------------------------

-- Tuples

--------------------------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 902

-- Text

-- instance (KnownVal (sym :: Symbol) String) => KnownVal ('FTxt.Text sym) Txt.Text
--   where
--     fromType _ = Txt.pack $ fromType @sym Proxy


-- | Text instance.
--
-- === __Example__
--
-- >>> :{
-- afun :: forall r. (r ~ 'FTxt.Text "hmm") => Txt.Text
-- afun = fromType @r Proxy
-- :}
--
-- >>> afun
-- "hmm"
instance KnownSymbol sym => KnownVal ('FTxt.Text sym) Txt.Text
  where
    fromType _ = Txt.pack $ fromType @sym Proxy



#else

#endif


