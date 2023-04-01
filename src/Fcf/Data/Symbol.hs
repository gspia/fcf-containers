{-# LANGUAGE CPP                  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

{-|
Module      : Fcf.Data.Symbol
Description : Type-level Symbol helpers for Fcf libs
Copyright   : (c) gspia 2023-
License     : BSD
Maintainer  : gspia

= Fcf.Data.Symbol

This might should go to first-class-families.

-}

--------------------------------------------------------------------------------

module Fcf.Data.Symbol
  where


import           Fcf.Core 
import qualified GHC.TypeLits as TL


--------------------------------------------------------------------------------

-- Version should be larger than 9.2.x. Note the two digit space for the
-- minor.
#if __GLASGOW_HASKELL__ >= 902

import           Fcf ( type (=<<), Foldr )
import           Fcf.Data.List ( type (++) )


data AppendSymbol :: TL.Symbol -> TL.Symbol -> Exp TL.Symbol
type instance Eval (AppendSymbol a b) = TL.AppendSymbol a b

data CmpSymbol :: TL.Symbol -> TL.Symbol -> Exp Ordering
type instance Eval (CmpSymbol a b) = TL.CmpSymbol a b

data CmpChar :: Char -> Char -> Exp Ordering
type instance Eval (CmpChar a b) = TL.CmpChar a b

data ConsChar :: Char -> TL.Symbol -> Exp TL.Symbol
type instance Eval (ConsChar a b) = TL.ConsSymbol a b

data UnconsSymbol :: TL.Symbol -> Exp (Maybe (Char, TL.Symbol))
type instance Eval (UnconsSymbol a) = TL.UnconsSymbol a

data CharToNat :: Char -> Exp TL.Nat
type instance Eval (CharToNat c) = TL.CharToNat c

data NatToChar :: TL.Nat -> Exp Char
type instance Eval (NatToChar n) = TL.NatToChar n


-- Helper for ToCharList method.
data HandlePair :: Maybe (Char, TL.Symbol) -> Exp [Char]
type instance Eval (HandlePair 'Nothing) = '[]
type instance Eval (HandlePair ('Just '(c, sym))) =
    Eval ('[c] ++ (Eval (ToCharList sym)))

data ToCharList :: TL.Symbol -> Exp [Char]
type instance Eval (ToCharList sym) = Eval (HandlePair =<< UnconsSymbol sym)


data CharToSymbol :: Char -> Exp TL.Symbol
type instance Eval (CharToSymbol c) = TL.ConsSymbol c ""

-- > :k! Eval (Concat '[ "aa", "bb", "cc"])
-- Eval (Concat '[ "aa", "bb", "cc"]) :: TL.Symbol
-- = "aabbcc"
-- > :k! Eval (Concat '[ ])
-- Eval (Concat '[ ]) :: TL.Symbol
-- = ""
data Concat :: [TL.Symbol] -> Exp TL.Symbol
type instance Eval (Concat syms) = Eval (Foldr AppendSymbol "" syms)


data ConcatChars :: [Char] -> Exp TL.Symbol
type instance Eval (ConcatChars chrs) = Eval (Foldr ConsChar "" chrs)


--------------------------------------------------------------------------------

#else

--------------------------------------------------------------------------------


-- These methods are not available on ghc 9.0 or lower (base 4.16 introduced).

data CmpSymbol :: TL.Symbol -> TL.Symbol -> Exp Ordering
type instance Eval (CmpSymbol a b) =
    TL.TypeError ('TL.Text "CmpSymbol in 9.2.x or higher")

data CmpChar :: Char -> Char -> Exp Ordering
type instance Eval (CmpChar a b) =
    TL.TypeError ('TL.Text "CmpChar in 9.2.x or higher")

data UnconsSymbol :: TL.Symbol -> Exp (Maybe (Char, TL.Symbol))
type instance Eval (UnconsSymbol a) =
    TL.TypeError ('TL.Text "UnconsSymbol in 9.2.x or higher")

data CharToNat :: Char -> Exp TL.Nat
type instance Eval (CharToNat c) =
    TL.TypeError ('TL.Text "CharToNat in 9.2.x or higher")

data NatToChar :: TL.Nat -> Exp Char
type instance Eval (NatToChar n) =
    TL.TypeError ('TL.Text "NatToChar in 9.2.x or higher")

data ConsChar :: Char -> TL.Symbol -> Exp TL.Symbol
type instance Eval (ConsChar a b) =
    TL.TypeError ('TL.Text "ConsChar in 9.2.x or higher")

data ToCharList :: TL.Symbol -> Exp [Char]
type instance Eval (ToCharList sym) =
    TL.TypeError ('TL.Text "ToCharList in 9.2.x or higher")

data CharToSymbol :: Char -> Exp TL.Symbol
type instance Eval (CharToSymbol c) =
    TL.TypeError ('TL.Text "CharToSymbol in 9.2.x or higher")

data Concat :: [TL.Symbol] -> Exp TL.Symbol
type instance Eval (Concat lst) =
    TL.TypeError ('TL.Text "Concat in 9.2.x or higher")

data ConcatChars :: [Char] -> Exp TL.Symbol
type instance Eval (ConcatChars lst) =
    TL.TypeError ('TL.Text "ConcatChars in 9.2.x or higher")

#endif

--------------------------------------------------------------------------------
