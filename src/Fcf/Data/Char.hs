{-# LANGUAGE CPP                    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

{-|
Module      : Fcf.Data.Char
Description : Type-level Char data methods
Copyright   : (c) gspia 2020-
License     : BSD
Maintainer  : gspia

= Fcf.Data.Char

-}

--------------------------------------------------------------------------------

module Fcf.Data.Char
  where


-- Version should be larger than 9.2.x. Note the two digit space for the
-- minor.
#if __GLASGOW_HASKELL__ >= 902

import qualified GHC.TypeLits as TL
import           Fcf.Core (Eval, Exp)
import           Fcf.Data.Bool (type (||))
import           Fcf.Data.List (Elem)
import           Fcf.Utils (TyEq)

--------------------------------------------------------------------------------


-- For the doctests:

-- $setup
 
--------------------------------------------------------------------------------

-- | IsSpace
--
-- === __Example__
--
-- >>> :kind! Eval (IsSpace 'a')
-- Eval (IsSpace 'a') :: Bool
-- = 'False
--
-- >>> :kind! Eval (IsSpace ' ')
-- Eval (IsSpace ' ') :: Bool
-- = 'True
data IsSpace :: Char -> Exp Bool
type instance Eval (IsSpace s) = Eval (s == ' ')


-- | IsNewline
--
-- === __Example__
--
-- >>> :kind! Eval (IsNewLine 'a')
-- Eval (IsNewLine 'a') :: Bool
-- = 'False
--
-- >>> :kind! Eval (IsNewLine '\n')
-- Eval (IsNewLine '\n') :: Bool
-- = 'True
data IsNewLine :: Char -> Exp Bool
type instance Eval (IsNewLine s) = Eval (s == '\n')


-- | IsTab
--
-- === __Example__
--
-- >>> :kind! Eval (IsTab 'a')
-- Eval (IsTab 'a') :: Bool
-- = 'False
--
-- >>> :kind! Eval (IsTab '\t')
-- Eval (IsTab '\t') :: Bool
-- = 'True
data IsTab :: Char -> Exp Bool
type instance Eval (IsTab s) = Eval (s == '\t')


-- | IsSpaceDelim
--
-- === __Example__
--
-- >>> :kind! Eval (IsSpaceDelim 'a')
-- Eval (IsSpaceDelim 'a') :: Bool
-- = 'False
--
-- >>> :kind! Eval (IsSpaceDelim '\n')
-- Eval (IsSpaceDelim '\n') :: Bool
-- = 'True
data IsSpaceDelim :: Char -> Exp Bool
type instance Eval (IsSpaceDelim s) =
    Eval (Eval (IsSpace s) || Eval (Eval (IsNewLine s) || Eval (IsTab s)))


-- | IsDigit
--
-- === __Example__
--
-- >>> :kind! Eval (IsDigit '3')
-- Eval (IsDigit '3') :: Bool
-- = 'True
--
-- >>> :kind! Eval (IsDigit 'a')
-- Eval (IsDigit 'a') :: Bool
-- = 'False
data IsDigit :: Char -> Exp Bool
type instance Eval (IsDigit s)
    = Eval (Elem s '[ '0','1','2','3','4','5','6','7','8','9'])


--------------------------------------------------------------------------------


-- | CharOrd - compare two symbols and give type-level Ordering
-- ( $ 'LT $, $ 'EQ $ or $ 'GT $ ).
--
-- === __Example__
--
-- >>> :kind! Eval (CharOrd 'a' 'b')
-- Eval (CharOrd 'a' 'b') :: Ordering
-- = 'LT
data CharOrd :: Char -> Char -> Exp Ordering
type instance Eval (CharOrd a b) = TL.CmpChar a b
    

-- | Less-than-or-equal comparison for symbols.
--
-- === __Example__
--
-- >>> :kind! Eval ('b' <= 'a')
-- Eval ('b' <= 'a') :: Bool
-- = 'False
--
data (<=) :: Char -> Char -> Exp Bool
type instance Eval ((<=) a b) =
    Eval (Eval (TyEq (TL.CmpChar a b) 'LT) || Eval (TyEq (TL.CmpChar a b) 'EQ))

-- | Larger-than-or-equal comparison for symbols.
--
-- === __Example__
--
-- >>> :kind! Eval ('b' >= 'a')
-- Eval ('b' >= 'a') :: Bool
-- = 'True
data (>=) :: Char -> Char -> Exp Bool
type instance Eval ((>=) a b) =
    Eval (Eval (TyEq (TL.CmpChar a b) 'GT) || Eval (TyEq (TL.CmpChar a b) 'EQ))

-- | Less-than comparison for symbols.
--
-- === __Example__
--
-- >>> :kind! Eval ('a' < 'b')
-- Eval ('a' < 'b') :: Bool
-- = 'True
data (<) :: Char -> Char -> Exp Bool
type instance Eval ((<) a b) = Eval (TyEq (TL.CmpChar a b) 'LT)

-- | Larger-than comparison for symbols.
--
-- === __Example__
--
-- >>> :kind! Eval ('b' > 'a')
-- Eval ('b' > 'a') :: Bool
-- = 'True
data (>) :: Char -> Char -> Exp Bool
type instance Eval ((>) a b) = Eval (TyEq (TL.CmpChar a b) 'GT)

-- | Equality of symbols
--
-- === __Example__
--
-- >>> :kind! Eval ('b' == 'a')
-- Eval ('b' == 'a') :: Bool
-- = 'False
data (==) :: Char -> Char -> Exp Bool
type instance Eval ((==) a b) = Eval (TyEq (TL.CmpChar a b) 'EQ)

--------------------------------------------------------------------------------

#else

--------------------------------------------------------------------------------


#endif

--------------------------------------------------------------------------------

