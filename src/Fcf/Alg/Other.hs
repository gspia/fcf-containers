{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

{-|
Module      : Fcf.Alg.Other
Description : Utility collection (waiting for new place)
Copyright   : (c) gspia 2020-
License     : BSD
Maintainer  : gspia

= Fcf.Alg.Other

To be moved to some other place

-}


--------------------------------------------------------------------------------

module Fcf.Alg.Other where

import           Fcf

--------------------------------------------------------------------------------

-- | Helper.
--
-- === __Example__
--
-- > :kind! Eval (PairMaybeToMaybePair '( 'Just "txt", 'Just 1))
-- Eval (PairMaybeToMaybePair '( 'Just "txt", 'Just 1)) :: Maybe
--                                                           (Symbol, TL.Natural)
-- = 'Just '("txt", 1)
data PairMaybeToMaybePair :: (Maybe a, Maybe b) -> Exp (Maybe (a,b))
type instance Eval (PairMaybeToMaybePair '( 'Nothing, _)) = 'Nothing
type instance Eval (PairMaybeToMaybePair '( _, 'Nothing)) = 'Nothing
type instance Eval (PairMaybeToMaybePair '( 'Just a, 'Just b)) = 'Just '(a,b)


-- | Id function.
--
-- === __Example__
--
-- > :kind! Eval (Id "id")
-- Eval (Id "id") :: Symbol
-- = "id"
data Id :: a -> Exp a
type instance Eval (Id a) = a
