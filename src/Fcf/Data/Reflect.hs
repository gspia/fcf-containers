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
import           Data.Typeable (Typeable, typeRep)
import           Data.Kind (Type)
import qualified Data.Map as DM
import qualified Data.IntMap.Strict as IMS
import qualified Data.Set as S
-- #if __GLASGOW_HASKELL__ >= 902
-- #endif
import qualified Data.Tree as T

import qualified Fcf.Data.MapC as MC
import qualified Fcf.Data.NatMap as NM
import qualified Fcf.Data.Set as FS
#if __GLASGOW_HASKELL__ >= 902
import qualified Fcf.Data.NewText as FTxt
#endif
import qualified Fcf.Data.Tree as FT

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
-- > :{
-- afun :: forall n. (n ~ '[1,2,3,4]) => [Int]
-- afun = natVals @n Proxy
-- :}
--
-- >>> afun
-- [1,2,3,4]
class KnownNats (ns :: [Nat]) where
  natVals :: Proxy ns -> [Int]

{-# DEPRECATED KnownNats "Replaced with KnownVal" #-}

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
instance KnownVal () '() where fromType _ = ()

instance (IsString str, KnownSymbol s) => KnownVal str (s :: Symbol) where
    fromType _ = fromString $ TL.symbolVal (Proxy @s)

#if __GLASGOW_HASKELL__ >= 920
instance (TL.KnownChar c) => KnownVal Char c where
    fromType _ = TL.charVal (Proxy @c)
#endif

instance (IsString str, Typeable typ) => KnownVal str (typ :: Type) where
    fromType = fromString . show . typeRep

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
    fromType _ = fromType @str (Proxy @sym) 

#endif

--------------------------------------------------------------------------------

-- List instances

instance KnownVal [a] '[] where
    fromType _ = []

instance (KnownVal val x, KnownVal [val] xs) => KnownVal [val] (x ': xs) where
    fromType _ = fromType (Proxy @x) : fromType (Proxy @xs)

--------------------------------------------------------------------------------

-- Trees
--
instance (KnownVal val k, KnownVal (T.Forest val) trees) => KnownVal (T.Tree val) ('FT.Node k trees)
  where
    fromType _ = T.Node (fromType (Proxy @k)) (fromType (Proxy @trees))

--------------------------------------------------------------------------------

-- NatMaps / IntMaps
--
instance (KnownVal [(Int,val)] pairs) => KnownVal (IMS.IntMap val) ('NM.NatMap pairs)
  where
    fromType _ = IMS.fromList (fromType (Proxy @pairs))

instance (KnownVal [(Int,val)] pairs) => KnownVal (IMS.IntMap val) (pairs :: [(Nat, val')])
  where
    fromType _ = IMS.fromList (fromType (Proxy @pairs))

--------------------------------------------------------------------------------

-- Maps

instance (Ord key, KnownVal [(key,val)] pairs) => KnownVal (DM.Map key val) ('MC.MapC pairs)
  where
    fromType _ = DM.fromList (fromType (Proxy @pairs))

instance (Ord key, KnownVal [(key,val)] pairs) => KnownVal (DM.Map key val) (pairs :: [(key',val')])
  where
    fromType _ = DM.fromList (fromType (Proxy @pairs))

--------------------------------------------------------------------------------

-- Set

instance (Ord val, KnownVal [val] kind) => KnownVal (S.Set val) ('FS.Set kind)
  where
    fromType _ = S.fromList (fromType (Proxy @kind))

instance (Ord val, KnownVal [val] kind) => KnownVal (S.Set val) (kind :: [kind'])
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

--------------------------------------------------------------------------------

-- ErrorMessage from GHC.TypeLits

instance (IsString str, KnownSymbol sym) => KnownVal str ('TL.Text sym) where
  fromType _ = fromType @str (Proxy @sym)

instance (IsString str, Typeable typ) => KnownVal str ('TL.ShowType typ) where
  fromType _ = fromString $ show $ typeRep (Proxy @typ)

instance (IsString str, KnownVal str err1, KnownVal str err2, Semigroup str) => KnownVal str (err1 'TL.:<>: err2) where
  fromType _ = fromType (Proxy @err1) <> fromType (Proxy @err2)

instance (IsString str, KnownVal str err1, KnownVal str err2, Semigroup str) => KnownVal str (err1 'TL.:$$: err2) where
  fromType _ = fromType (Proxy @err1) <> fromString "\n" <> fromType (Proxy @err2)
