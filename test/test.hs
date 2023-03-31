{-# LANGUAGE
    DataKinds,
    KindSignatures,
    TypeOperators #-}

--------------------------------------------------------------------------------

import           Data.Type.Equality ((:~:)(Refl))
import           GHC.TypeLits (Nat, Symbol)
import           Fcf hiding (type (<*>))

--------------------------------------------------------------------------------

import           Fcf.Data.Set
import           Fcf.Control.Monad
import           Fcf.Class.Monoid
import           Fcf.Data.List

--------------------------------------------------------------------------------


-- Compile-time tests

_ = Refl :: Eval (ToList =<< PowerSet  =<< FromList '[1,2])
        :~: '[ 'Set '[], 'Set '[2], 'Set '[1], 'Set '[1,2]]

-- Dummy

main :: IO ()
main = pure ()

--------------------------------------------------------------------------------
-- Applicative

_ = Refl :: Eval ('("hello", (+) 15) <*> '("world!", 2002))
       :~: Eval (LiftA2 (+) '("hello", 15) '("world!", 2002))

_ = Refl :: Eval ('[(+) 1,(+) 2] <*> '[3,4])
       :~: Eval (LiftA2 (+) '[1,2] '[3,4])

_ = Refl :: Eval ( (<*>) '[] '[4,5,6,7])
       :~: Eval ( (<*>) '[] '[4,5,6,7])

--------------------------------------------------------------------------------
-- Monad

_ = Refl :: Eval ('[5,6,7] >>= Plus2M)
       :~: '[7,8,8,9,9,10]

_ = Refl :: Eval ('Just "hello" >>= (Return <=< Flip (.<>) " world"))
       :~: 'Just "hello world"

_ = Refl :: Eval ('Just 5 >> 'Just "hello world")
       :~: 'Just "hello world"

_ = Refl :: Eval ('Nothing >>= (Return <=< Flip (.<>) " world"))
       :~: 'Nothing

_ = Refl :: Eval ('("age", 5) >>= (Return <=< (+) 1))
       :~: '("age", 6)

_ = Refl :: Eval ('("he","wo",19) >>= (Tuple3 "llo" "rld" <=< Flip (-) 1))
       :~: '("hello","world",18)

--------------------------------------------------------------------------------
-- Traversable

data TraversableTest = TraversableTest Symbol [(Symbol, Nat)]

data SafeMinus :: Nat -> Nat -> Exp (Either Symbol Nat)
type instance Eval (SafeMinus a b) = If (Eval (a >= b)) ('Right (Eval (a - b))) ('Left "Nat cannot be negative")

_ = Refl :: Eval 
              (Map 
                (Flip Cons '[] <=< Pure1 ('TraversableTest "test")) 
                (Sequence @@ (Map 
                  (Sequence <=< Map (Flip SafeMinus 1))
                  @@ '[ '("key1",1), '("key2",2)])))
       :~: 'Right '[ 'TraversableTest "test" '[ '("key1",0), '("key2",1)]]

