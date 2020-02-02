{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

{-|
Module      : Fcf.Data.Symbol
Description : Type level symbols
Copyright   : (c) gspia 2020-
License     : BSD
Maintainer  : gspia

= Fcf.Data.Symbol

Type-level symbols and functions for them.

Note that the operators from this module conflict with "GHC.TypeLits".


-}

--------------------------------------------------------------------------------

module Fcf.Data.Symbol
    ( -- * Reexported type
      -- | From "GHC.TypeList".

      Symbol

      -- * Functions

    , Append
    , Intercalate

     -- * Comparison functions

    , type (<=)
    , type (>=)
    , type (<)
    , type (>)
    , type (==)
    )
  where

--------------------------------------------------------------------------------
        
import           GHC.TypeLits (Symbol)
import qualified GHC.TypeLits as TL

import           Fcf.Core (Eval, Exp)
import           Fcf.Data.List (Foldr)
import           Fcf.Data.Bool (type (||))
import           Fcf.Utils (TyEq)

import           Fcf.Combinators (type (=<<))

--------------------------------------------------------------------------------

-- | Append two type-level symbols.
--
-- === __Example__
-- 
-- >>> :kind! Eval (Append "hmm" " ok")
-- Eval (Append "hmm" " ok") :: Symbol
-- = "hmm ok"
data Append :: Symbol -> Symbol -> Exp Symbol
type instance Eval (Append s1 s2) = TL.AppendSymbol s1 s2


-- | Intercalate type-level symbols.
-- 
-- === __Example__
-- 
-- >>> :kind! Eval (Intercalate "+" '["aa", "bb", "cc"])
-- Eval (Intercalate "+" '["aa", "bb", "cc"]) :: Symbol
-- = "aa+bb+cc"
--
-- >>> :kind! Eval (Intercalate "+" '["aa"])
-- Eval (Intercalate "+" '["aa"]) :: Symbol
-- = "aa"
--
-- >>> :kind! Eval (Intercalate "+" '[])
-- Eval (Intercalate "+" '[]) :: Symbol
-- = ""
data Intercalate :: Symbol -> [Symbol] -> Exp Symbol
type instance Eval (Intercalate s1 '[]) = ""
type instance Eval (Intercalate s1 (s ': sLst)) =
    Eval (Append s =<< Foldr (InterCalHelp s1) "" sLst)

-- helper
data InterCalHelp :: Symbol -> Symbol -> Symbol -> Exp Symbol
type instance Eval (InterCalHelp s s1 s2) = Eval (Append (Eval (Append s s1)) s2)

--------------------------------------------------------------------------------


-- | Less-than-or-equal comparison for symbols.
-- 
-- === __Example__
-- 
-- >>> :kind! Eval ("b" <= "a")
-- Eval ("b" <= "a") :: Bool
-- = 'False
--
data (<=) :: Symbol -> Symbol -> Exp Bool
type instance Eval ((<=) a b) =
    Eval (Eval (TyEq (TL.CmpSymbol a b) 'LT) || Eval (TyEq (TL.CmpSymbol a b) 'EQ))

-- | Larger-than-or-equal comparison for symbols.
-- 
-- === __Example__
-- 
-- >>> :kind! Eval ("b" >= "a")
-- Eval ("b" >= "a") :: Bool
-- = 'True
data (>=) :: Symbol -> Symbol -> Exp Bool
type instance Eval ((>=) a b) =
    Eval (Eval (TyEq (TL.CmpSymbol a b) 'GT) || Eval (TyEq (TL.CmpSymbol a b) 'EQ))

-- | Less-than comparison for symbols.
-- 
-- === __Example__
-- 
-- >>> :kind! Eval ("a" < "b")
-- Eval ("a" < "b") :: Bool
-- = 'True
data (<) :: Symbol -> Symbol -> Exp Bool
type instance Eval ((<) a b) = Eval (TyEq (TL.CmpSymbol a b) 'LT)

-- | Larger-than comparison for symbols.
-- 
-- === __Example__
-- 
-- >>> :kind! Eval ("b" > "a")
-- Eval ("b" > "a") :: Bool
-- = 'True
data (>) :: Symbol -> Symbol -> Exp Bool
type instance Eval ((>) a b) = Eval (TyEq (TL.CmpSymbol a b) 'GT)

-- | Equality of symbols
-- 
-- === __Example__
-- 
-- >>> :kind! Eval ("b" == "a")
-- Eval ("b" == "a") :: Bool
-- = 'False
data (==) :: Symbol -> Symbol -> Exp Bool
type instance Eval ((==) a b) = Eval (TyEq (TL.CmpSymbol a b) 'EQ)
