{-# LANGUAGE
  DataKinds,
  PolyKinds,
  RankNTypes,
  ScopedTypeVariables,
  TypeFamilies,
  TypeInType,
  TypeOperators,
  UndecidableInstances
  #-}

module Ex1 where

import           Data.Kind (Type)
import           Data.Proxy
import qualified GHC.TypeLits as TL
import           GHC.TypeLits (Nat, Symbol)

import           Fcf
import           Fcf.Class.Monoid (type (.<>)) -- type level (<>)
import           Fcf.Data.List as L
import           Fcf.Data.Tree

-- Example
type family Fst' (xy :: (a,b)) :: a
type instance Fst' '(a,_) = a

-- Example
data MapFst :: [(a,b)] -> Exp [a]
type instance Eval (MapFst a) = Eval (Fcf.Map Fst a)

-- On writing foldl on type level. We could try to mimick the way foldr has
-- been implemented. But writing foldl in terms of foldr and on type level
-- sounds fun, so let's try it.

-- Could we take this as starting point for writing the type level foldl?
-- We should first write the step function on type level.  For that,
-- what is the type of the step function?
foldl2 :: (b -> a -> b) -> b -> [a] -> b
foldl2 f z xs = foldr step id xs $ z
  where
    step y g x = g (f x y)

-- This is similar to the previous one, except now we use lambda.
-- Unfortunately, we don't have lambda functions on type level.
foldl3 :: (b -> a -> b) -> b -> [a] -> b
foldl3 f y as =
  foldr (\(y :: b) (g :: a -> a) (x :: a) -> g (f x y) :: a) id as y
  -- foldr (\y g x -> g (f x y)) id as y

-- Easy definition for this demonstration. While on value level we loose some
-- properties that the library foldl has, this is sufficient for our purposes.
-- E.g. we don't have that kind of laziness on type level where we could input
-- potentially infinite list for folding.
foldl4 :: (b -> a -> b) -> b -> [a] -> b
foldl4 f init xs = foldr (flip f) init (reverse xs)


parenR :: Int -> String -> String
parenR i paren = show i ++ ",(" ++ paren ++ ")"

parenL :: String -> Int -> String
parenL paren i  = "(" ++ paren ++ ")," ++ show i



data Foldl :: (b -> a -> Exp b) -> b -> t a -> Exp b

-- type instance Eval (Foldl f b ta) = Eval (Foldr (Flip f) b (Reverse ta))
-- Doesn't work:
-- > :kind! Eval (Foldl (Fcf.+) 0 '[1,2,3])
-- Eval (Foldl (Fcf.+) 0 '[1,2,3]) :: Nat
-- = Eval (Foldl (Fcf.+) 0 '[1, 2, 3])

type instance Eval (Foldl f b ta) = Eval (Foldr (Flip f) b (Eval (Reverse ta)))
-- The Eval is easy to miss:
-- > :kind! Eval (Foldl (Fcf.+) 0 '[1,2,3])
-- Eval (Foldl (Fcf.+) 0 '[1,2,3]) :: Nat
-- = 6
--
-- Also, Foldr (Fcf.-) 0 '[3,2,1]
-- returns a result (2): as it starts from the rigth, each substraction returns
-- a positive value and thus, "kinding" it gives a result.  Trying Foldr
-- there naturally doesn't work.



-- Let's repeat the parenL and parenR example on type level.

-- Just for experimenting, bad implementation here:
data ShowNat :: Nat -> Exp Symbol
type instance Eval (ShowNat 0) = "0"
type instance Eval (ShowNat 1) = "1"
type instance Eval (ShowNat 2) = "2"
type instance Eval (ShowNat 3) = "3"
type instance Eval (ShowNat 4) = "4"
type instance Eval (ShowNat 5) = "5"
type instance Eval (ShowNat 6) = "6"
type instance Eval (ShowNat 7) = "7"
type instance Eval (ShowNat 8) = "8"
type instance Eval (ShowNat 9) = "9"
type instance Eval (ShowNat 10) = "10"
-- type instance Eval (ShowNat _) = "No more fingers!" -- conflicting instances

-- These are a bit ugly: might be doable in a more readable way.
data ParenR :: Nat -> Symbol -> Exp Symbol
type instance Eval (ParenR n s) =
  Eval (Eval (ShowNat n) .<> Eval (",(" .<> Eval (s .<> ")")))
data ParenL :: Symbol -> Nat -> Exp Symbol
type instance Eval (ParenL s n) =
  Eval (Eval ("(" .<> Eval (s .<> "),")) .<> Eval (ShowNat n))

-- Now we can try out the foldings on type and value level and see, how the
-- input functions get applied either from the left or right.

-- > :kind! Eval (Foldr ParenR "" '[1,2,3])
-- Eval (Foldr ParenR "" '[1,2,3]) :: Symbol
-- = "1,(2,(3,()))"
-- > :kind! Eval (Foldl ParenL "" '[1,2,3])
-- Eval (Foldl ParenL "" '[1,2,3]) :: Symbol
-- = "(((),1),2),3"
-- > foldr parenR "" [1,2,3]
-- "1,(2,(3,()))"
-- > foldl parenL "" [1,2,3]
-- "(((),1),2),3"
-- > foldl4 parenL "" [1,2,3]
-- "(((),1),2),3"




-- On implementing the breadth-first-search for trees on type-level.
--
-- We use the following value level function as inspiration.
--
-- breadth :: Tree a -> [a]
-- breadth nd =  map rootLabel $ nd : (breadth2 [nd])
--     where
--       breadth2 :: [Tree a] -> [Tree a]
--       breadth2 [] = []
--       breadth2 nds =
--         let cs = foldr ((++) . subForest) [] nds
--          in cs ++ breadth2 cs

data CalcCS :: [Tree a] -> Exp [Tree a]
-- type instance Eval (CalcCS trs) = Eval (Foldr ((L.++) <=< GetForest) '[] trs) -- doesn't work
type instance Eval (CalcCS trs) = Eval (Foldr FoldFun '[] trs) 

data FoldFun :: Tree a -> [Tree a] -> Exp [Tree a]
type instance Eval (FoldFun a b) = Eval ( (Eval (GetForest a)) L.++ b)

data Breadth2 :: [Tree a] -> Exp [Tree a]
type instance Eval (Breadth2 '[]) = '[]
type instance Eval (Breadth2 (t ':ts)) =
  Eval ( (Eval (CalcCS (t ': ts))) L.++ (Eval (Breadth2 (Eval (CalcCS (t ': ts))))))

data FormTreeList :: Tree a -> Exp [Tree a]
type instance Eval (FormTreeList tr) = tr ': (Eval (Breadth2 '[tr]))

-- The whole implementation with the helpers may can be written in a cleaner way
-- (no effort was spent on that).
data Breadth :: Tree a -> Exp [a]
type instance Eval (Breadth tr) = Eval (Map GetRoot =<< FormTreeList tr)
-- type instance Eval (Breadth tr) = Eval (Map GetRoot =<< Eval (tr ': (Eval (Breadth2 '[tr]))))



-- How to get the answer from type level to value level.
-- Please do take a note of the natVals and how it is implemented below.
answer :: forall n.
  (n ~ Eval (Breadth (Node 1 '[Node 2 '[Node 4 '[], Node 5 '[Node 8 '[], Node 9 '[]]]
                              , Node 3 '[Node 6 '[Node 10 '[]], Node 7 '[]]
                              ]))
  ) => [Int]
answer = natVals @n Proxy 


-- From https://hackage.haskell.org/package/numhask-array-0.10.1/docs/src/NumHask.Array.Shape.html#natVals
-- | Reflect a list of Nats
class KnownNats (ns :: [Nat]) where
  natVals :: Proxy ns -> [Int]

instance KnownNats '[] where
  natVals _ = []

instance (TL.KnownNat n, KnownNats ns) => KnownNats (n : ns) where
  natVals _ = fromInteger (TL.natVal (Proxy @n)) : natVals (Proxy @ns)

