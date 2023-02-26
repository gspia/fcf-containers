{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

{-|
Module      : Fcf.Data.Tuple
Description : Type-level tuple functions
Copyright   : (c) gspia 2020-
License     : BSD
Maintainer  : gspia

= Fcf.Data.Tuple


-}

--------------------------------------------------------------------------------

module Fcf.Data.Tuple where

import           Fcf (Eval, Exp)

--------------------------------------------------------------------------------

-- For the doctests:

-- $setup
-- >>> import qualified GHC.TypeLits as TL

--------------------------------------------------------------------------------

-- | Swap
--
-- === __Example__
--
-- >>> :kind! Eval (Swap '(1, 2))
-- Eval (Swap '(1, 2)) :: (TL.Natural, TL.Natural)
-- = '(2, 1)
data Swap :: (a, b) -> Exp (b, a)
type instance Eval (Swap '(a,b)) = '(b,a)
