{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

{-|
Module      : Fcf.Data.Tree
Description : Tree data-structure for type-level programming
Copyright   : (c) gspia 2020-
License     : BSD
Maintainer  : gspia

= Fcf.Data.Tree

Tree provides an interface which is similar to the that given by the
container-package. If a method is missing here that you need, please do open
up an issue or better, make a PR.

This module provides it's own but (almost) identical definitions of Tree
and Forest. The reason for not using the definitions given in the containers
is that since nothing else is needed from containers, we are able to have
less dependencies.

Many of the examples are from containers-package.

-}


--------------------------------------------------------------------------------

module Fcf.Data.Tree where

import           Fcf as Fcf
-- import           Fcf.Data.List as Fcf -- TODO start using on fcf 0.7
import           Fcf.Alg.List as Fcf (Unfoldr,ConcatMap) -- TODO remove on fcf 0.7

--------------------------------------------------------------------------------

-- For the doctests:

-- $setup
-- >>> import qualified GHC.TypeLits as TL
-- >>> import           Fcf.Data.Nat

--------------------------------------------------------------------------------

-- | Same as in containers, except not used for any term-level computation 
-- in this module.
data Tree a = Node a [Tree a]

-- | Same as in containers, except not used for any term-level computation 
-- in this module.
type Forest a = [Tree a]

-- | Fold a type-level 'Tree'.
data FoldTree :: (a -> [b] -> Exp b) -> Tree a -> Exp b
type instance Eval (FoldTree f ('Node a '[])) = Eval (f a '[])
type instance Eval (FoldTree f ('Node a (x ': xs))) =
    Eval (f a (Eval (Map (FoldTree f) (x ': xs))))



-- | Unfold for a 'Tree'.
--
-- __Example__
-- 
-- >>> data BuildNode :: Nat -> Exp (Nat,[Nat])
-- >>> :{
--   type instance Eval (BuildNode x) =
--       If (Eval ((2 TL.* x TL.+ 1) >= 8))
--           '(x, '[])
--           '(x, '[2 TL.* x, (2 TL.* x) TL.+ 1 ])
-- :}
--
-- >>> :kind! Eval (UnfoldTree BuildNode 1)
-- Eval (UnfoldTree BuildNode 1) :: Tree Nat
-- = 'Node
--     1
--     '[ 'Node 2 '[ 'Node 4 '[], 'Node 5 '[]],
--        'Node 3 '[ 'Node 6 '[], 'Node 7 '[]]]
data UnfoldTree :: (b -> Exp (a, [b])) -> b -> Exp (Tree a)
type instance Eval (UnfoldTree f b) =
    'Node (Eval (Fst Fcf.=<< f b)) (Eval (UnfoldForest f (Eval (Snd =<< f b))))

-- | Unfold for a 'Forest'.
data UnfoldForest :: (b -> Exp (a, [b])) -> [b] -> Exp (Forest a)
type instance Eval (UnfoldForest f bs) = Eval (Map (UnfoldTree f) bs)


-- | Flatten a 'Tree'.
--
-- __Example__
-- 
-- >>> :kind! Eval (Flatten ('Node 1 '[ 'Node 2 '[ 'Node 3 '[ 'Node 4 '[]]], 'Node 5 '[ 'Node 6 '[]]]))
-- Eval (Flatten ('Node 1 '[ 'Node 2 '[ 'Node 3 '[ 'Node 4 '[]]], 'Node 5 '[ 'Node 6 '[]]])) :: [Nat]
-- = '[1, 2, 3, 4, 5, 6]
data Flatten :: Tree a -> Exp [a]
type instance Eval (Flatten ('Node a fs )) = a ': Eval (ConcatMap Flatten fs)

-- | Get the root node from a 'Tree'.
data GetRoot :: Tree a -> Exp a
type instance Eval (GetRoot ('Node a _)) = a

-- | Get the forest from a 'Tree'.
data GetForest :: Tree a -> Exp [Tree a]
type instance Eval (GetForest ('Node _ f)) = f

-- | Get the root nodes from a list of 'Tree's.
data GetRoots :: [Tree a] -> Exp [a]
type instance Eval (GetRoots trs) = Eval (Map GetRoot trs)

-- | Get the forests from a list of 'Tree's.
data GetForests :: [Tree a] -> Exp [Tree a]
type instance Eval (GetForests ts) = Eval (ConcatMap GetForest ts)

-- helper for the Levels-function
data SubFLevels :: [Tree a] -> Exp (Maybe ([a], [Tree a]))
type instance Eval (SubFLevels '[]) = 'Nothing
type instance Eval (SubFLevels (t ': ts)) =
    'Just '( Eval (GetRoots (t ': ts)), Eval (GetForests (t ': ts)))

-- | Get the levels from a 'Tree'.
--
-- __Example__
-- 
-- >>> :kind! Eval (Levels ('Node 1 '[ 'Node 2 '[ 'Node 3 '[ 'Node 4 '[]]], 'Node 5 '[ 'Node 6 '[]]]))
-- Eval (Levels ('Node 1 '[ 'Node 2 '[ 'Node 3 '[ 'Node 4 '[]]], 'Node 5 '[ 'Node 6 '[]]])) :: [[Nat]]
-- = '[ '[1], '[2, 5], '[3, 6], '[4]]
data Levels :: Tree a -> Exp [[a]]
type instance Eval (Levels tr) = Eval (Unfoldr SubFLevels '[tr])


