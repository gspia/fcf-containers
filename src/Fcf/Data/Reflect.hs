{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
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
import           Data.String (fromString, IsString)
import           Data.Proxy
-- import qualified Data.Map.Strict as MS
import qualified Data.Map as DM
import qualified Data.IntMap.Strict as IMS
import qualified Data.Set as S
-- #if __GLASGOW_HASKELL__ >= 902
-- #endif
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

class KnownVal val kind where
    fromType :: Proxy kind -> val

instance (KnownNat n, Num a) => KnownVal a (n :: Nat) where
    fromType _ = fromInteger $ TL.natVal (Proxy @n)

instance KnownVal Bool 'True where fromType _ = True
instance KnownVal Bool 'False where fromType _ = False

instance (IsString str, KnownSymbol s) => KnownVal str (s :: Symbol )where
    fromType _ = fromString $ TL.symbolVal (Proxy @s)

#if __GLASGOW_HASKELL__ >= 902

-- | Text instance.
--
-- === __Example__
--
-- >>> import qualified Data.Text as Txt
-- >>> :{
-- afun :: forall r. (r ~ 'FTxt.Text "hmm") => Txt.Text
-- afun = fromType (Proxy @r)
-- :}
--
-- >>> afun
-- "hmm"
instance (IsString str, KnownSymbol sym) => KnownVal str ('FTxt.Text sym)
  where
    fromType _ = fromString $ fromType $ Proxy @sym 

#endif

--------------------------------------------------------------------------------

-- List instances

instance KnownVal [a] '[] where
    fromType _ = []

instance (KnownVal typ x, KnownVal [typ] xs) => KnownVal [typ] (x ': xs) where
    fromType _ = fromType (Proxy @x) : fromType (Proxy @xs)

--------------------------------------------------------------------------------

-- Trees
--
instance (KnownVal typ k, KnownVal (T.Forest typ) trees) => KnownVal (T.Tree typ) ('FT.Node k trees)
  where
    fromType _ = T.Node (fromType (Proxy @k)) (fromType (Proxy @trees))

--------------------------------------------------------------------------------

-- NatMaps / IntMaps
--
instance (KnownVal [(Int,val)] pairs) => KnownVal (IMS.IntMap val) ('NM.NatMap pairs)
  where
    fromType _ = IMS.fromList (fromType (Proxy @pairs))

--------------------------------------------------------------------------------

-- Maps

instance (Ord key, KnownVal [(key,val)] pairs) => KnownVal (DM.Map key val) ('MC.MapC pairs)
  where
    fromType _ = DM.fromList (fromType (Proxy @pairs))

--------------------------------------------------------------------------------

-- Set

instance (Ord typ, KnownVal [typ] kind) => KnownVal (S.Set typ) ('FS.Set kind)
  where
    fromType _ = S.fromList (fromType (Proxy @kind))
 
--------------------------------------------------------------------------------

-- Either

instance (KnownVal a1 a) => KnownVal (Either a1 b1) ('Left a) where
    fromType _ = Left (fromType @a1 (Proxy @a))

instance (KnownVal b1 b) => KnownVal (Either a1 b1) ('Right b) where
    fromType _ = Right (fromType @b1 (Proxy @b))

--------------------------------------------------------------------------------

-- Maybe

instance (KnownVal a1 a) => KnownVal (Maybe a1) ('Just a) where
    fromType _ = Just (fromType @a1 (Proxy @a))

instance KnownVal (Maybe a1) 'Nothing where
    fromType _ = Nothing

--------------------------------------------------------------------------------

-- Tuples

instance (KnownVal a1 a, KnownVal b1 b) => KnownVal (a1,b1) '(a,b) where
    fromType _ = (fromType @a1 (Proxy @a), fromType @b1 (Proxy @b))

instance (KnownVal a1 a, KnownVal b1 b, KnownVal c1 c) => KnownVal (a1,b1,c1) '(a,b,c) where
    fromType _ = (fromType @a1 (Proxy @a), fromType @b1 (Proxy @b), fromType @c1 (Proxy @c))

instance (KnownVal a1 a, KnownVal b1 b, KnownVal c1 c, KnownVal d1 d) => KnownVal (a1,b1,c1,d1) '(a,b,c,d) where
    fromType _ = (fromType @a1 (Proxy @a), fromType @b1 (Proxy @b), fromType @c1 (Proxy @c), fromType @d1 (Proxy @d))

instance (KnownVal a1 a, KnownVal b1 b, KnownVal c1 c, KnownVal d1 d, KnownVal e1 e) => KnownVal (a1,b1,c1,d1,e1) '(a,b,c,d,e) where
    fromType _ = (fromType @a1 (Proxy @a), fromType @b1 (Proxy @b), fromType @c1 (Proxy @c), fromType @d1 (Proxy @d), fromType @e1 (Proxy @e))
