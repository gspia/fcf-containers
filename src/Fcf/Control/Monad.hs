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

import           Data.Functor.Identity
import           GHC.TypeNats as TN

import           Fcf hiding (type (<*>))
import           Fcf.Class.Monoid (type (<>), MEmpty)
import           Fcf.Data.Tuple

--------------------------------------------------------------------------------

-- For the doctests:

-- $setup
-- >>> import qualified GHC.TypeLits as TL
-- >>> import qualified Fcf.Combinators as C

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
type instance Eval (Return a) = '(MEmpty, a)
type instance Eval (Return a) = '(MEmpty, MEmpty, a)
type instance Eval (Return a) = '(MEmpty, MEmpty, MEmpty, a)


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
--  - (,)
--  - (,,)
--  - (,,,)
--
-- === __Example__
--
-- >>> :kind! Eval ('Identity Plus2 <*> 'Identity 5)
-- Eval ('Identity Plus2 <*> 'Identity 5) :: Identity Natural
-- = 'Identity 7
--
-- >>> :kind! Eval ( (<*>) '[ (Fcf.+) 1, (Fcf.*) 10] '[4,5,6,7])
-- Eval ( (<*>) '[ (Fcf.+) 1, (Fcf.*) 10] '[4,5,6,7]) :: [Natural]
-- = '[5, 6, 7, 8, 40, 50, 60, 70]
-- >>> :kind! Eval ( (<*>) '[ (Fcf.+) 1, (Fcf.*) 10] '[])
-- Eval ( (<*>) '[ (Fcf.+) 1, (Fcf.*) 10] '[]) :: [Natural]
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

-- Maybe
type instance Eval ('Nothing <*> _) = 'Nothing
type instance Eval ('Just f <*> m) = Eval (Map f m)

-- Either
type instance Eval ('Left e <*> _) = 'Left e
type instance Eval ('Right f <*> m) = Eval (Map f m)

-- | For tuples, the 'Monoid' constraint determines how the first values merge.
-- For example, 'Symbol's concatenate:
--
-- >>> :kind! Eval ('("hello", (Fcf.+) 15) <*> '("world!", 2002))
-- Eval ('("hello", (Fcf.+) 15) <*> '("world!", 2002)) :: (TL.Symbol,
--                                                         Natural)
-- = '("helloworld!", 2017)
type instance Eval ('(u, f) <*> '(v, x)) = '(u <> v, Eval (f x))

-- ((,,) a b)
type instance Eval ('(a, b, f) <*> '(a', b', x)) = '(a <> a', b <> b', Eval (f x))

-- ((,,,) a b)
type instance Eval ('(a, b, c, f) <*> '(a', b', c', x)) = '(a <> a', b <> b', c <> c', Eval (f x))

-- | Type level LiftA2.
--
-- === __Example__
--
-- >>> :kind! Eval (LiftA2 (Fcf.+) '[1,2] '[3,4])
-- Eval (LiftA2 (Fcf.+) '[1,2] '[3,4]) :: [Natural]
-- = '[4, 5, 5, 6]
--
--
data LiftA2 :: (a -> b -> Exp c) -> f a -> f b -> Exp (f c)
type instance Eval (LiftA2 f fa fb) = Eval (Eval (Map (App2 f) fa) <*> fb)

-- | Type level LiftA3.
--
-- === __Example__
-- 
-- >>> :kind! Eval (LiftA3 Tuple3 '[1,2] '[3,4] '[5,6])
-- Eval (LiftA3 Tuple3 '[1,2] '[3,4] '[5,6]) :: [(Natural, Natural,
--                                                Natural)]
-- = '[ '(1, 3, 5), '(1, 3, 6), '(1, 4, 5), '(1, 4, 6), '(2, 3, 5),
--      '(2, 3, 6), '(2, 4, 5), '(2, 4, 6)]
--
-- >>> :kind! Eval (LiftA3 Tuple3 ('Right 5) ('Right 6) ('Left "fail"))
-- Eval (LiftA3 Tuple3 ('Right 5) ('Right 6) ('Left "fail")) :: Either
--                                                                TL.Symbol (Natural, Natural, c)
-- = 'Left "fail"
--
data LiftA3 :: (a -> b -> c -> Exp d) -> f a -> f b -> f c -> Exp (f d)
type instance Eval (LiftA3 f fa fb fc) = Eval (Eval (Eval (Map (App3 f) fa) <*> fb) <*> fc)

-- | Type level LiftA4.
data LiftA4 :: (a -> b -> c -> d -> Exp e) -> f a -> f b -> f c -> f d -> Exp (f e)
type instance Eval (LiftA4 f fa fb fc fd) = Eval (Eval (Eval (Eval (Map (App4 f) fa) <*> fb) <*> fc) <*> fd)

-- | Type level LiftA5.
data LiftA5 :: (a -> b -> c -> d -> e -> Exp g) -> f a -> f b -> f c -> f d -> f e -> Exp (f g)
type instance Eval (LiftA5 f fa fb fc fd fe) = Eval (Eval (Eval (Eval (Eval (Map (App5 f) fa) <*> fb) <*> fc) <*> fd) <*> fe)

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
--  - (,)
--  - (,,)
--  - (,,,)
--
-- === __Example__
--
-- Example: double the length of the input list and increase the numbers at
-- the same time.
--
-- >>> :kind! Eval ('[5,6,7] >>= Plus2M)
-- Eval ('[5,6,7] >>= Plus2M) :: [Natural]
-- = '[7, 8, 8, 9, 9, 10]
--
--
-- >>> :kind! Eval (XsPlusYsMonadic '[1,2,3] '[4,5,6])
-- Eval (XsPlusYsMonadic '[1,2,3] '[4,5,6]) :: [Natural]
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

-- (,)
type instance Eval ('(u, a) >>= k) = Eval ('(u, Id) <*> Eval (k a))

-- (,,)
type instance Eval ('(u, v, a) >>= k) = Eval ('(u, v, Id) <*> Eval (k a))

-- (,,,)
type instance Eval ('(u, v, w, a) >>= k) = Eval ('(u, v, w, Id) <*> Eval (k a))

-- | Type level >> 
--
-- === __Example__
--
--
-- >>> :kind! Eval ( 'Just 1 >> 'Just 2)
-- Eval ( 'Just 1 >> 'Just 2) :: Maybe Natural
-- = 'Just 2
-- >>> :kind! Eval ( 'Nothing >> 'Just 2)
-- Eval ( 'Nothing >> 'Just 2) :: Maybe Natural
-- = 'Nothing
--
data (>>) :: m a -> m b -> Exp (m b)
type instance Eval (m >> k) = Eval (m >>= ConstFn k)

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
-- >>> data Lambda :: Nat -> Nat -> Exp (Either Symbol Natural)
-- >>> type instance Eval (Lambda a b) = If (Eval (a >= b)) ('Right (a TL.- b)) ('Left "Nat cannot be negative")
-- >>> :kind! Eval (FoldlM Lambda 5 '[1,1,1])
-- Eval (FoldlM Lambda 5 '[1,1,1]) :: Either Symbol Natural
-- = 'Right 2
-- >>> :kind! Eval (FoldlM Lambda 5 '[1,4,1])
-- Eval (FoldlM Lambda 5 '[1,4,1]) :: Either Symbol Natural
-- = 'Left "Nat cannot be negative"
--
data FoldlM :: (b -> a -> Exp (m b)) -> b -> t a -> Exp (m b)
type instance Eval (FoldlM f z0 xs) = Eval ((Eval (Foldr (FoldlMHelper f) Return xs)) z0)

--------------------------------------------------------------------------------
-- Traversable


-- | Traverse
--
-- === __Example__
--
-- >>> :kind! Eval (Traverse Id '[ '[1,2], '[3,4]])
-- Eval (Traverse Id '[ '[1,2], '[3,4]]) :: [[Natural]]
-- = '[ '[1, 3], '[1, 4], '[2, 3], '[2, 4]]
--
--
data Traverse :: (a -> Exp (f b)) -> t a -> Exp (f (t b))
-- type instance Eval (Traverse f ta) = Eval (SequenceA =<< Map f ta)
-- Could the above one just be made to work? At the moment, the computation
-- diverges (we need to give the Traverse instances).

-- []
type instance Eval (Traverse f lst) =
    Eval (Foldr (ConsHelper f) (Eval (Return '[])) lst)

-- Maybe
type instance Eval (Traverse f 'Nothing) = Eval (Return 'Nothing)
type instance Eval (Traverse f ('Just x)) = Eval (Map (Pure1 'Just) (Eval (f x)))

-- Either
type instance Eval (Traverse f ('Left e)) = Eval (Return ('Left e))
type instance Eval (Traverse f ('Right x)) = Eval (Map (Pure1 'Right) (Eval (f x)))

-- ((,) a)
type instance Eval (Traverse f '(x, y)) = Eval (Map (Tuple2 x) (Eval (f y)))


-- | Sequence
--
-- === __Example__
--
-- >>> :kind! Eval (Sequence ('Just ('Right 5)))
-- Eval (Sequence ('Just ('Right 5))) :: Either a (Maybe Natural)
-- = 'Right ('Just 5)
--
-- >>> :kind! Eval (Sequence '[ 'Just 3, 'Just 5, 'Just 7])
-- Eval (Sequence '[ 'Just 3, 'Just 5, 'Just 7]) :: Maybe [Natural]
-- = 'Just '[3, 5, 7]
--
-- >>> :kind! Eval (Sequence '[ 'Just 3, 'Nothing, 'Just 7])
-- Eval (Sequence '[ 'Just 3, 'Nothing, 'Just 7]) :: Maybe [Natural]
-- = 'Nothing
--
-- >>> :kind! Eval (Sequence '[ '[1,2], '[3,4]])
-- Eval (Sequence '[ '[1,2], '[3,4]]) :: [[Natural]]
-- = '[ '[1, 3], '[1, 4], '[2, 3], '[2, 4]]
--
--
data Sequence :: t (f a) -> Exp (f (t a))
type instance Eval (Sequence tfa) = Eval (Traverse Id tfa)

--------------------------------------------------------------------------------
-- Utility

-- | Id function correspondes to term level 'id'-function.
data Id :: a -> Exp a
type instance Eval (Id a) = a

--------------------------------------------------------------------------------
-- Helper Functions

-- | Needed by LiftA2 instance to partially apply function
data App2 :: (a -> b -> c) -> a -> Exp (b -> c)
type instance Eval (App2 f a) = f a

-- | Needed by LiftA3 instance to partially apply function
data App3 :: (a -> b -> c -> d) -> a -> Exp (b -> Exp (c -> d))
type instance Eval (App3 f a) = Pure2 f a

-- | Needed by LiftA4 instance to partially apply function
data App4 :: (a -> b -> c -> d -> e) -> a -> Exp (b -> Exp (c -> Exp (d -> e)))
type instance Eval (App4 f a) = App3 (f a)

-- | Needed by LiftA5 instance to partially apply function
data App5 :: (a -> b -> c -> d -> e -> g) -> a -> Exp (b -> Exp (c -> Exp (d -> Exp (e -> g))))
type instance Eval (App5 f a) = App4 (f a)

-- | Helper for the [] applicative instance.
data Star_ :: (a -> Exp b) -> f a -> Exp (f b)
type instance Eval (Star_ _ '[]) = '[]
type instance Eval (Star_ f (a ': as)) =
    Eval (f a) ': Eval (Star_ f as)

-- | Helper for 'FoldlM'
data FoldlMHelper :: (b -> a -> Exp (m b)) -> a -> (b -> Exp (m b)) -> Exp (b -> Exp (m b))
type instance Eval (FoldlMHelper f a b) = Flip (>>=) b <=< Flip f a

-- | Helper for [] traverse
data ConsHelper :: (a -> Exp (f b)) -> a -> f [b] -> Exp (f [b])
type instance Eval (ConsHelper f x ys) = Eval (LiftA2 (Pure2 '(:)) (Eval (f x)) ys)
-- The following would need an extra import line:
-- type instance Eval (Cons_f f x ys) = Eval (LiftA2 Cons (Eval (f x)) ys)


--------------------------------------------------------------------------------
-- For Examples

-- | For Applicative documentation example
data Plus1 :: Nat -> Exp Nat
type instance Eval (Plus1 n) = n TN.+ 1

-- | For Applicative documentation example
data Plus2 :: Nat -> Exp Nat
type instance Eval (Plus2 n) = n TN.+ 2

-- | For the example. Turn an input number to list of two numbers of a bit
-- larger numbers.
data Plus2M :: Nat -> Exp [Nat]
type instance Eval (Plus2M n) = '[n TN.+ 2, n TN.+3]

-- | Part of an example
data PureXPlusY :: Nat -> Nat -> Exp [Nat]
type instance Eval (PureXPlusY x y) = Eval (Return ((TN.+) x y))

-- | Part of an example
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
