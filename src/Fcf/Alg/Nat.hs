{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

{-|
Module      : Fcf.Alg.Nat
Description : Nat helpers
Copyright   : (c) gspia 2020-
License     : BSD
Maintainer  : gspia

= Fcf.Data.Nat


-}


--------------------------------------------------------------------------------

module Fcf.Alg.Nat where

import qualified GHC.TypeNats as TN
import           Fcf.Core (Eval, Exp)
import           Fcf.Data.Bool
import           Fcf.Data.Nat (Nat)
import qualified Fcf.Data.Nat as FN

--------------------------------------------------------------------------------

-- | Nat equality.
--
-- >>> :kind! Eval (2 == 2)
-- Eval (2 == 2) :: Bool
-- = 'True
--
-- >>> :kind! Eval (2 == 3)
-- Eval (2 == 3) :: Bool
-- = 'False
data (==) :: Nat -> Nat -> Exp Bool
type instance Eval ((==) a b) = Eval ((a TN.<=? b) && (b TN.<=? a))


-- | Nat in-equality.
--
-- >>> :kind! Eval (2 /= 2)
-- Eval (2 /= 2) :: Bool
-- = 'False
--
-- >>> :kind! Eval (2 /= 3)
-- Eval (2 /= 3) :: Bool
-- = 'True
data (/=) :: Nat -> Nat -> Exp Bool
type instance Eval ((/=) a b) = Eval (Eval (a FN.< b) || Eval (b FN.< a))
