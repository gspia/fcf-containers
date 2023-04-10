{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module Test.Control.Monad where

import           Data.Functor ((<&>))
import           Data.Proxy
import           Control.Applicative
import           Test.Hspec (describe, it, shouldBe, Spec)
import           GHC.TypeLits (Nat, Symbol)

import           Fcf hiding (type (<*>))
import           Fcf.Data.Tuple
import           Fcf.Data.List (Cons)
import           Fcf.Control.Monad
import           Fcf.Class.Monoid (type (.<>))
import           Fcf.Data.Reflect (fromType)

-- data TraversableTest = TraversableTest Symbol [(Symbol, Nat)]

data SafeMinus :: Nat -> Nat -> Exp (Either Symbol Nat)
type instance Eval (SafeMinus a b) = If (Eval (a >= b)) ('Right (Eval (a - b))) ('Left "Nat cannot be negative")


spec :: Spec
spec = describe "Monad" $ do
  it "liftA2 (,) instance" $ do
    let test :: forall r. (r ~ Eval (LiftA2 Tuple2 '("hello ", 5) '("world", 6))) => (String, (Int, Int))
        test = fromType (Proxy @r)
    test 
      `shouldBe` 
      liftA2 (,) ("hello ", 5) ("world", 6)
  it "<*> (,) instance" $ do
    let test :: forall r. (r ~ Eval ('("hello ", (+) 15) <*> '("world!", 2002))) => (String,Int)
        test = fromType (Proxy @r)
    test 
      `shouldBe` 
      ("hello ", (+) 15) <*> ("world!", 2002)
  it ">>= (,) instance" $ do
    let test :: forall r. (r ~ Eval ('("age", 5) >>= (Return <=< (+) 1))) => (String,Int)
        test = fromType (Proxy @r)
    test 
      `shouldBe` 
      (("age",5) <&> (+) 1)
      -- Above was suggested by hlint for the following:
      -- (("age",5) >>= return . (+) 1)
  it "liftA3 (,,) instance" $ do
    let test :: forall r. (r ~ Eval (LiftA3 Tuple3 '("hello ", "<", 6) '("world", " ", 8) '("!", ">", 10))) => (String, String, (Int, Int, Int))
        test = fromType (Proxy @r)
    test 
      `shouldBe` 
      liftA3 (,,) ("hello ","<",6) ("world"," ",8) ("!",">",10)
  it ">>= (,,) instance" $ do
    let test :: forall r. (r ~ Eval ('("he","wo",19) >>= (Tuple3 "llo" "rld" <=< Flip (-) 1))) => (String,String,Int)
        test = fromType (Proxy @r)
    test 
      `shouldBe` 
      (("he","wo",19) >>= ("llo","rld",) . flip (-) 1)
  it "liftA4 Either instance test Left" $ do
    let test :: forall r. (r ~ Eval (LiftA4 Tuple4 ('Right 5) ('Right 6) ('Right 7) ('Left "fail"))) => Either String (Int,Int,Int,Int)
        test = fromType (Proxy @r)
    test 
      `shouldBe` 
      (,,,) <$> Right 5 <*> Right 6 <*> Right 7 <*> Left "fail"
  it "liftA4 Either instance test Right" $ do
    let test :: forall r. (r ~ Eval (LiftA4 Tuple4 ('Right 5) ('Right 6) ('Right 7) ('Right 0))) => Either String (Int,Int,Int,Int)
        test = fromType (Proxy @r)
    test 
      `shouldBe` 
      (,,,) <$> Right 5 <*> Right 6 <*> Right 7 <*> Right 0
  it "Traversable Either instance, test with MapM" $ do
    let typeLevelFunc :: forall (list :: [(Symbol,Nat)]) r. 
            (r ~ Eval 
              (Map 
                (Flip Cons '[] <=< Tuple2 "test") 
                (MapM
                  (MapM (Flip SafeMinus 1))
                  @@ list
                )
              )
            ) => Proxy r
        typeLevelFunc = Proxy @r
    let safeMinus a b = if a >= b then Right (a - b) else Left "Nat cannot be negative"
    let valTest :: [(String,Int)] -> Either String [(String, [(String, Int)])]
        valTest list = fmap (flip (:) [] . ("test",)) (mapM (mapM (`safeMinus` 1)) list)
    fromType (typeLevelFunc @'[ '("key1",1), '("key2",2)])
      `shouldBe` 
      valTest [("key1",1),("key2",2)]
    fromType (typeLevelFunc @'[ '("key1",0), '("key2",2)])
      `shouldBe` 
      valTest [("key1",0),("key2",2)]
  it "liftA5 [] instance" $ do
    let test :: forall r. (r ~ Eval (LiftA5 Tuple5 '[1,5] '[2,4] '[6,7,8] '[9,11,10] '[0] )) => [(Int,Int,Int,Int,Int)]
        test = fromType (Proxy @r)
    test 
      `shouldBe` 
      (,,,,) <$> [1,5] <*> [2,4] <*> [6,7,8] <*> [9,11,10] <*> [0]
  it "<*> [] instance" $ do
    let test :: forall r. (r ~ Eval ('[(+) 1,(+) 2] <*> '[3,4])) => [Int]
        test = fromType (Proxy @r)
    test 
      `shouldBe` 
      [(+) 1,(+) 2] <*> [3,4]
  it ">>= [] instance" $ do
    let test :: forall r. (r ~ Eval ('[5,6,7] >>= Plus2M)) => [Int]
        test = fromType (Proxy @r)
    test 
      `shouldBe` 
      ([5,6,7] >>= (\n -> [n + 2, n + 3]))
  it ">>= Maybe instance test Just" $ do
    let test :: forall r. (r ~ Eval ('Just "hello" >>= (Return <=< Flip (.<>) " world"))) => Maybe String
        test = fromType (Proxy @r)
    test 
      `shouldBe` 
      (Just "hello" <&> flip (<>) " world")
      -- Above was suggested by hlint for the following:
      -- (Just "hello" >>= (return . flip (<>) " world"))
  it ">>= Maybe instance test Nothing" $ do
    let test :: forall r. (r ~ Eval ('Nothing >>= (Return <=< Flip (.<>) " world"))) => Maybe String
        test = fromType (Proxy @r)
    test 
      `shouldBe` 
      (Nothing <&> flip (<>) " world")
      -- Above was suggested by hlint for the following:
      -- (Nothing >>= (return . flip (<>) " world"))
  it ">> Maybe instance" $ do
    let test :: forall r. (r ~ Eval ('Just 5 >> 'Just "hello world")) => Maybe String
        test = fromType (Proxy @r)
    test 
      `shouldBe` 
      (Just (5 :: Int) >> Just "hello world")
