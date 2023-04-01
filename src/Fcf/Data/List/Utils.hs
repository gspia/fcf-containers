{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

{-|
Module      : Fcf.Data.List.Utils
Description : List helpers / utils
Copyright   : (c) gspia 2023-
License     : BSD
Maintainer  : gspia

= Fcf.Data.List.Utils

Some of the following methods would probably have better place at the
first-class-families library.  That is, polish and make a PR.

-}

--------------------------------------------------------------------------------

module Fcf.Data.List.Utils where

import           Fcf
import           Fcf.Data.List as Fcf

--------------------------------------------------------------------------------

-- For the doctests:

-- $setup
-- >>> import qualified GHC.TypeLits as TL
-- >>> import           Fcf.Data.Nat

--------------------------------------------------------------------------------

-- | Foldl in terms of Foldr.
--
-- === __Example__
--
-- >>> :kind! Eval (Foldl (Fcf.-) 10 '[3,2,1])
-- Eval (Foldl (Fcf.-) 10 '[3,2,1]) :: TL.Natural
-- = 4
data Foldl :: (b -> a -> Exp b) -> b -> t a -> Exp b
type instance Eval (Foldl f b ta) = Eval (Foldr (Flip f) b (Eval (Reverse ta)))


-- | Turn Maybe into a list.
--
-- === __Example__
--
-- >>> :kind! Eval (MaybeToList ('Just 1))
-- Eval (MaybeToList ('Just 1)) :: [TL.Natural]
-- = '[1]
data MaybeToList :: Maybe a -> Exp [a]
type instance Eval (MaybeToList 'Nothing) = '[]
type instance Eval (MaybeToList ('Just a)) = '[a]

