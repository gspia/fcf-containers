{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

{-|
Module      : Fcf.Alg.Symbol
Description : Type level symbols
Copyright   : (c) gspia 2020-
License     : BSD
Maintainer  : gspia

= Fcf.Alg.Symbol

Type-level symbols and functions for them.

Note that the operators from this module conflict with "GHC.TypeLits".


TODO: Would this whole module have a place first-class-families?

-}

--------------------------------------------------------------------------------

module Fcf.Alg.Symbol
    ( -- * Reexported type
      -- | From "Fcf.Data.Symbol" (which is from GHC).

      module X

      -- * Functions

    , Append
    , Intercalate
    , IsSpace
    , IsNewLine
    , IsTab
    , IsSpaceDelim
    , IsDigit

     -- * Comparison functions

    , SymbolOrd
    , type (<=)
    , type (>=)
    , type (<)
    , type (>)
    , type (==)
    )
  where

--------------------------------------------------------------------------------

-- import           GHC.TypeLits (Symbol)
import qualified GHC.TypeLits as TL

import           Fcf.Core (Eval, Exp)
import           Fcf.Data.List (Foldr, Elem)
import           Fcf.Data.Bool (type (||))
import           Fcf.Data.Symbol as X
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


-- | IsSpace
--
-- === __Example__
--
-- >>> :kind! Eval (IsSpace "a")
-- Eval (IsSpace "a") :: Bool
-- = 'False
--
-- >>> :kind! Eval (IsSpace " ")
-- Eval (IsSpace " ") :: Bool
-- = 'True
data IsSpace :: Symbol -> Exp Bool
type instance Eval (IsSpace s) = Eval (s == " ")


-- | IsNewline
--
-- === __Example__
--
-- >>> :kind! Eval (IsNewLine "a")
-- Eval (IsNewLine "a") :: Bool
-- = 'False
--
-- >>> :kind! Eval (IsNewLine "\n")
-- Eval (IsNewLine "\n") :: Bool
-- = 'True
data IsNewLine :: Symbol -> Exp Bool
type instance Eval (IsNewLine s) = Eval (s == "\n")


-- | IsTab
--
-- === __Example__
--
-- >>> :kind! Eval (IsTab "a")
-- Eval (IsTab "a") :: Bool
-- = 'False
--
-- >>> :kind! Eval (IsTab "\t")
-- Eval (IsTab "\t") :: Bool
-- = 'True
data IsTab :: Symbol -> Exp Bool
type instance Eval (IsTab s) = Eval (s == "\t")


-- | IsSpaceDelim
--
-- === __Example__
--
-- >>> :kind! Eval (IsSpaceDelim "a")
-- Eval (IsSpaceDelim "a") :: Bool
-- = 'False
--
-- >>> :kind! Eval (IsSpaceDelim "\n")
-- Eval (IsSpaceDelim "\n") :: Bool
-- = 'True
data IsSpaceDelim :: Symbol -> Exp Bool
type instance Eval (IsSpaceDelim s) =
    Eval (Eval (IsSpace s) || (Eval (Eval (IsNewLine s) || Eval (IsTab s))))


-- | IsDigit
--
-- === __Example__
--
-- >>> :kind! Eval (IsDigit "3")
-- Eval (IsDigit "3") :: Bool
-- = 'True
--
-- >>> :kind! Eval (IsDigit "a")
-- Eval (IsDigit "a") :: Bool
-- = 'False
data IsDigit :: Symbol -> Exp Bool
type instance Eval (IsDigit s)
    = Eval (Elem s '["0","1","2","3","4","5","6","7","8","9"])



--------------------------------------------------------------------------------


-- | SymbolOrd - compare two symbols and give type-level Ordering
-- ( $ 'LT $, $ 'EQ $ or $ 'GT $ ).
--
-- === __Example__
--
-- >>> :kind! Eval (SymbolOrd "a" "b")
-- Eval (SymbolOrd "a" "b") :: Ordering
-- = 'LT
data SymbolOrd :: Symbol -> Symbol -> Exp Ordering
type instance Eval (SymbolOrd a b) = TL.CmpSymbol a b

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
