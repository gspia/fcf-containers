{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

{-|
Module      : Fcf.Data.Bitree
Description : Binary tree data-structure for type-level programming
Copyright   : (c) gspia 2020-
License     : BSD
Maintainer  : gspia

= Fcf.Data.Bitree

Binary trees

-}


--------------------------------------------------------------------------------

module Fcf.Data.Bitree where

import qualified GHC.TypeLits as TL
import           Fcf as Fcf
-- import           Fcf.Data.List as Fcf
import           Fcf.Data.Nat as N

--------------------------------------------------------------------------------

-- For the doctests:

-- $setup
-- >>> import qualified GHC.TypeLits as TL
-- >>> import           Fcf.Data.Nat

--------------------------------------------------------------------------------

-- | Binary tree type.
data Tree a = Leaf a | Node (Tree a) a (Tree a)
    deriving Show

-- type Forest a = (Tree a, Tree a)
-- type Forest a = [Tree a]

-- | Fold a type-level 'Tree'.
data FoldTree :: (a -> [b] -> Exp b) -> Tree a -> Exp b
type instance Eval (FoldTree f ('Leaf a)) = Eval (f a '[])
type instance Eval (FoldTree f ('Node tr1 a tr2)) =
    Eval (f a (Eval (Map (FoldTree f) '[ tr1, tr2])))


data CountSizeHelp :: Nat -> [Nat] -> Exp Nat
type instance Eval (CountSizeHelp tr '[]) = 1
-- type instance Eval (CountSizeHelp tr '[n]) = 1
type instance Eval (CountSizeHelp tr '[n1,n2]) = 1 TL.+ (n1 TL.+ n2)

type ExampleTree1 = 'Leaf 1
type ExampleTree2 = 'Node ('Leaf 2) 1 ('Leaf 3)
type ExampleTree3 = 'Node ('Node ('Leaf 4) 2 ('Leaf 5) ) 1 ('Node ('Leaf 6) 3 ('Leaf 7))
type ExampleTree4 = 'Node ('Node ('Leaf 4) 2 ('Leaf 5) ) 1 ('Leaf 3)


{-

-- | Unfold for a 'Tree'.
--
-- === __Example__
--
-- > >> data BuildNode :: Nat -> Exp (Nat,[Nat])
-- > >> :{
--   type instance Eval (BuildNode x) =
--       If (Eval ((2 TL.* x TL.+ 1) >= 8))
--           '(x, '[])
--           '(x, '[2 TL.* x, (2 TL.* x) TL.+ 1 ])
-- :}
--
-- > >> :kind! Eval (UnfoldTree BuildNode 1)
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
-}

-- | Flatten a 'Tree'.
--
-- === __Example__
--
data Flatten :: Tree a -> Exp [a]
type instance Eval (Flatten ('Leaf a)) = '[a]
type instance Eval (Flatten ('Node tr1 a tr2)) =
    a ': Eval ((Eval (Flatten tr1)) ++ (Eval (Flatten tr2)))

-- | Get the root node from a 'Tree'.
data GetRoot :: Tree a -> Exp a
type instance Eval (GetRoot ('Leaf a )) = a
type instance Eval (GetRoot ('Node _ a _)) = a


-- | Get the root nodes from a list of 'Tree's.
data GetRoots :: [Tree a] -> Exp [a]
type instance Eval (GetRoots trs) = Eval (Map GetRoot trs)

{-
-- | Get the forest from a 'Tree'.
-- data GetForest :: Tree a -> Exp [Tree a]
-- type instance Eval (GetForest ('Node _ f)) = f
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
-- === __Example__
--
-- > >> :kind! Eval (Levels ('Node 1 '[ 'Node 2 '[ 'Node 3 '[ 'Node 4 '[]]], 'Node 5 '[ 'Node 6 '[]]]))
-- Eval (Levels ('Node 1 '[ 'Node 2 '[ 'Node 3 '[ 'Node 4 '[]]], 'Node 5 '[ 'Node 6 '[]]])) :: [[Nat]]
-- = '[ '[1], '[2, 5], '[3, 6], '[4]]
data Levels :: Tree a -> Exp [[a]]
type instance Eval (Levels tr) = Eval (Unfoldr SubFLevels '[tr])


-}
