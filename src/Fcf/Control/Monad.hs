{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

{-|
Module      : Fcf.Control.Monad
Description : Monad definitions
Copyright   : (c) gspia 2020-
License     : BSD
Maintainer  : gspia

= Fcf.Control.Monad


-}

--------------------------------------------------------------------------------

module Fcf.Control.Monad where

import           Control.Monad.Identity
import           GHC.TypeNats as TN

import           Fcf as Fcf hiding (type (<*>))

--------------------------------------------------------------------------------
-- Functor instances

type instance Eval (Map f ('Identity a)) = 'Identity (Eval (f a))



--------------------------------------------------------------------------------
-- Common methods for both Applicative and Monad


-- | Return corresponds to the 'return' at Monad
-- or 'pure' of Applicative.
--
--
-- :kind! Eval (Return 1) :: Maybe Nat
-- :kind! Eval (Return 1) :: Either Symbol Nat
data Return :: a -> Exp (m a)
type instance Eval (Return a) = '[a]
type instance Eval (Return a) = 'Just a
type instance Eval (Return a) = 'Right a
type instance Eval (Return a) = 'Identity a


--------------------------------------------------------------------------------
-- Applicative

-- | (<*>) corresponds to the value level '<*>'. Note that this clashes with
-- the definition given at Fcf.Combinators.((<*>)).
--
-- Applicatives that we define include:
--
--  - Identity
--  - []
--  - Maybe
--  - Either
--
-- === __Example__
--
-- >>> :kind! Eval ('Identity Plus2 <*> 'Identity 5)
-- Eval ('Identity Plus2 <*> 'Identity 5) :: Identity Nat
-- = 'Identity 7
--
-- >>> :kind! Eval ( (<*>) '[ (Fcf.+) 1, (Fcf.*) 10] '[4,5,6,7])
-- Eval ( (<*>) '[ (Fcf.+) 1, (Fcf.*) 10] '[4,5,6,7]) :: [Nat]
-- = '[5, 6, 7, 8, 40, 50, 60, 70]
-- >>> :kind! Eval ( (<*>) '[ (Fcf.+) 1, (Fcf.*) 10] '[])
-- Eval ( (<*>) '[ (Fcf.+) 1, (Fcf.*) 10] '[]) :: [Nat]
-- = '[]
-- >>> :kind! Eval ( (<*>) '[] '[4,5,6,7])
-- Eval ( (<*>) '[] '[4,5,6,7]) :: [b]
-- = '[]
--
--
data (<*>) :: f (a -> Exp b) -> f a -> Exp (f b)

type instance Eval ('Identity f <*> m) = Eval (Map f m)

type instance Eval ('[] <*> _) = '[]
type instance Eval (_ <*> '[]) = '[]
type instance Eval ((f ': fs) <*> (a ': as)) =
    Eval ((++) (Eval (Star_ f (a ': as))) (Eval ((<*>) fs (a ':as))))

-- | Helper for the [] applicative instance.
data Star_ :: (a -> Exp b) -> f a -> Exp (f b)
type instance Eval (Star_ _ '[]) = '[]
type instance Eval (Star_ f (a ': as)) =
    Eval (f a) ': (Eval (Star_ f as))


-- Example
data Plus1 :: Nat -> Exp Nat
type instance Eval (Plus1 n) = n TN.+ 1

-- Example
data Plus2 :: Nat -> Exp Nat
type instance Eval (Plus2 n) = n TN.+ 2


type instance Eval ('Nothing <*> _) = 'Nothing
type instance Eval ('Just f <*> m) = Eval (Map f m)


type instance Eval ('Left e <*> _) = 'Left e
type instance Eval ('Right f <*> m) = Eval (Map f m)


-- | Type level LiftA2.
--
-- === __Example__
--
-- >>> :kind! Eval (LiftA2 (Fcf.+) '[1,2] '[3,4])
-- Eval (LiftA2 (Fcf.+) '[1,2] '[3,4]) :: [Nat]
-- = '[4, 5, 5, 6]
--
--
data LiftA2 :: (a -> b -> Exp c) -> f a -> f b -> Exp (f c)
-- Could a single default implementation work here? Looks like it would need
-- a function that turns (a -> b -> c) to (b -> c).
-- E.g. something like:
-- type instance Eval (LiftA2 f fa fb) = Eval ( (<*>) (Map (f fa)) fb)

type instance Eval (LiftA2 f 'Nothing _) = 'Nothing
type instance Eval (LiftA2 f _ 'Nothing) = 'Nothing
type instance Eval (LiftA2 f ('Just a) ('Just b)) = 'Just (Eval (f a b))

type instance Eval (LiftA2 f ('Left e) _) = 'Left e
type instance Eval (LiftA2 f ('Right _) ('Left e)) = 'Left e
type instance Eval (LiftA2 f ('Right a) ('Right b)) = 'Right (Eval (f a b))


type instance Eval (LiftA2 f '[] _) = '[]
type instance Eval (LiftA2 f (a ': as) '[]) = '[]
type instance Eval (LiftA2 f (a ': as) (b ':bs)) =
    Eval ((++) (Eval (LiftA2_ f a (b ': bs))) (Eval (LiftA2 f as (b ':bs))))

-- Helper for list LiftA2 instance.
data LiftA2_ :: (a -> b -> Exp c) -> a -> f b -> Exp (f c)
type instance Eval (LiftA2_ f a '[]) = '[]
type instance Eval (LiftA2_ f a (b ': bs)) =
    Eval (f a b) ': (Eval (LiftA2_ f a bs))


--------------------------------------------------------------------------------
-- Monad


-- | Type level Bind corresponding to the value level bind '>>=' operator.
--  Note that name (>>=) clashes with the definition given at
--  Fcf.Combinators.(>>=). (It doesn't export it yet, though.)
--
-- Monads that we define include:
--
--  - Identity
--  - []
--  - Maybe
--  - Either
--
-- === __Example__
--
-- Example: double the length of the input list and increase the numbers at
-- the same time.
--
-- >>> :kind! Eval ('[5,6,7] >>= Plus2M)
-- Eval ('[5,6,7] >>= Plus2M) :: [Nat]
-- = '[7, 8, 8, 9, 9, 10]
--
--
-- >>> :kind! Eval (XsPlusYsMonadic '[1,2,3] '[4,5,6])
-- Eval (XsPlusYsMonadic '[1,2,3] '[4,5,6]) :: [Nat]
-- = '[5, 6, 7, 6, 7, 8, 7, 8, 9]
data (>>=) :: m a -> (a -> Exp (m b)) -> Exp (m b)

-- Maybe
type instance Eval ('Nothing >>= f) = 'Nothing
type instance Eval ('Just a >>= f) = Eval (f a)

-- Either
type instance Eval ('Left a >>= _) = 'Left a
type instance Eval ('Right a >>= f) = Eval (f a)

-- Identity
type instance Eval ('Identity a >>= f) = Eval (f a)

-- Lists
type instance Eval ('[] >>= _) = '[]
type instance Eval ((x ': xs) >>= f) = Eval ((f @@ x) ++  Eval (xs >>= f))


-- For the example. Turn an input number to list of two numbers of a bit
-- larger numbers.
data Plus2M :: Nat -> Exp [Nat]
type instance Eval (Plus2M n) = '[n TN.+ 2, n TN.+3]

-- Part of an example
data PureXPlusY :: Nat -> Nat -> Exp [Nat]
type instance Eval (PureXPlusY x y) = Eval (Return ((TN.+) x y))

-- Part of an example
data XPlusYs :: Nat -> [Nat] -> Exp [Nat]
type instance Eval (XPlusYs x ys) = Eval (ys >>= PureXPlusY x)

-- | An example implementing
--
-- sumM xs ys = do
--     x <- xs
--     y <- ys
--     return (x + y)
--
-- or
--
-- sumM xs ys = xs >>= (\x -> ys >>= (\y -> pure (x+y)))
--
-- Note the use of helper functions. This is a bit awkward, a type level
-- lambda would be nice.
data XsPlusYsMonadic :: [Nat] -> [Nat] -> Exp [Nat]
type instance Eval (XsPlusYsMonadic xs ys) = Eval (xs >>= Flip XPlusYs ys)




-- data Sumnd :: [Nat] -> [Nat] -> Exp [Nat]
-- type instance Eval (Sumnd xs ys) = xs >>=

-- data Sum2 :: Nat -> Nat -> Exp Nat
-- type instance Eval (Sum2 x y) = x TN.+ y

--
-- === __Example__
--
--
-- >>> :kind! Eval ( 'Just 1 >> 'Just 2)
-- Eval ( 'Just 1 >> 'Just 2) :: Maybe Nat
-- = 'Just 2
-- >>> :kind! Eval ( 'Nothing >> 'Just 2)
-- Eval ( 'Nothing >> 'Just 2) :: Maybe Nat
-- = 'Nothing
--
--
data (>>) :: m a -> m b -> Exp (m b)

-- Maybe
type instance Eval ('Nothing >> b) = 'Nothing
type instance Eval ('Just a >> b) = b

-- Either
type instance Eval ('Left a >> _) = 'Left a
type instance Eval ('Right _ >> b) = b

-- Lists
-- TODO, are the instances ok?
type instance Eval ('[] >> _) = '[]
type instance Eval ((x ': xs) >> b) = b

--------------------------------------------------------------------------------
-- MapM

-- | MapM
--
-- === __Example__
--
-- >>> :kind! Eval (MapM (ConstFn '[ 'True, 'False]) '["a","b","c"])
-- Eval (MapM (ConstFn '[ 'True, 'False]) '["a","b","c"]) :: [[Bool]]
-- = '[ '[ 'True, 'True, 'True], '[ 'True, 'True, 'False],
--      '[ 'True, 'False, 'True], '[ 'True, 'False, 'False],
--      '[ 'False, 'True, 'True], '[ 'False, 'True, 'False],
--      '[ 'False, 'False, 'True], '[ 'False, 'False, 'False]]
--
--
data MapM :: (a -> Exp (m b)) -> t a -> Exp (m (t b))
type instance Eval (MapM f ta) = Eval (Sequence (Map f @@ ta))
-- Above one is same as:
-- type instance Eval (MapM f ta) = Eval (Sequence (Eval (Map f ta)))


-- | ForM = Flip MapM
data ForM :: t a -> (a -> Exp (m b)) -> Exp (m (t b))
type instance Eval (ForM ta f) = Eval (MapM f ta)


--------------------------------------------------------------------------------
-- FoldlM

-- | FoldlM
--
-- === __Example__
--
-- >>> import GHC.TypeLits as TL (Symbol, type (-))
-- >>> data Lambda :: Nat -> Nat -> Exp (Either Symbol Nat)
-- >>> type instance Eval (Lambda a b) = If (Eval (a >= b)) ('Right (a TL.- b)) ('Left "Nat cannot be negative")
-- >>> :kind! Eval (FoldlM Lambda 5 '[1,1,1])
-- Eval (FoldlM Lambda 5 '[1,1,1]) :: Either Symbol Nat
-- = 'Right 2
-- >>> :kind! Eval (FoldlM Lambda 5 '[1,4,1])
-- Eval (FoldlM Lambda 5 '[1,4,1]) :: Either Symbol Nat
-- = 'Left "Nat cannot be negative"
--
data FoldlM :: (b -> a -> Exp (m b)) -> b -> t a -> Exp (m b)
type instance Eval (FoldlM f z0 xs) = Eval ((Eval (Foldr (FoldlMHelper f) Return xs)) z0)

-- | Helper for 'FoldlM'
data FoldlMHelper :: (b -> a -> Exp (m b)) -> a -> (b -> Exp (m b)) -> Exp (b -> Exp (m b))
type instance Eval (FoldlMHelper f a b) = Flip (>>=) b <=< Flip f a

--------------------------------------------------------------------------------
-- Traversable


-- | Traverse
--
-- === __Example__
--
-- >>> :kind! Eval (Traverse Id '[ '[1,2], '[3,4]])
-- Eval (Traverse Id '[ '[1,2], '[3,4]]) :: [[Nat]]
-- = '[ '[1, 3], '[1, 4], '[2, 3], '[2, 4]]
--
--
data Traverse :: (a -> Exp (f b)) -> t a -> Exp (f (t b))
-- type instance Eval (Traverse f ta) = Eval (SequenceA =<< Map f ta)
-- Could the above one just be made to work? At the moment, the computation
-- diverges (we need to give the Traverse instances).

-- []
type instance Eval (Traverse f lst) =
    Eval (Foldr (Cons_f f) (Eval (Return '[])) lst)

-- | Helper for [] traverse
data Cons_f :: (a -> Exp (f b)) -> a -> f [b] -> Exp (f [b])
type instance Eval (Cons_f f x ys) = Eval (LiftA2 (Pure2 '(:)) (Eval (f x)) ys)
-- The following would need an extra import line:
-- type instance Eval (Cons_f f x ys) = Eval (LiftA2 Cons (Eval (f x)) ys)


-- Maybe
type instance Eval (Traverse f 'Nothing) = Eval (Return 'Nothing)
type instance Eval (Traverse f ('Just x)) = Eval (Map (Pure1 'Just) (Eval (f x)))

-- Either
type instance Eval (Traverse f ('Left e)) = Eval (Return ('Left e))
type instance Eval (Traverse f ('Right x)) = Eval (Map (Pure1 'Right) (Eval (f x)))


-- | Id function correspondes to term level 'id'-function.
data Id :: a -> Exp a
type instance Eval (Id a) = a


-- | Sequence
--
-- === __Example__
--
-- >>> :kind! Eval (Sequence ('Just ('Right 5)))
-- Eval (Sequence ('Just ('Right 5))) :: Either a (Maybe Nat)
-- = 'Right ('Just 5)
--
-- >>> :kind! Eval (Sequence '[ 'Just 3, 'Just 5, 'Just 7])
-- Eval (Sequence '[ 'Just 3, 'Just 5, 'Just 7]) :: Maybe [Nat]
-- = 'Just '[3, 5, 7]
--
-- >>> :kind! Eval (Sequence '[ 'Just 3, 'Nothing, 'Just 7])
-- Eval (Sequence '[ 'Just 3, 'Nothing, 'Just 7]) :: Maybe [Nat]
-- = 'Nothing
--
-- >>> :kind! Eval (Sequence '[ '[1,2], '[3,4]])
-- Eval (Sequence '[ '[1,2], '[3,4]]) :: [[Nat]]
-- = '[ '[1, 3], '[1, 4], '[2, 3], '[2, 4]]
--
--
data Sequence :: t (f a) -> Exp (f (t a))
type instance Eval (Sequence tfa) = Eval (Traverse Id tfa)


