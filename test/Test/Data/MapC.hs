{-# LANGUAGE CPP                    #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE DataKinds              #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module Test.Data.MapC where

import           Data.Proxy
import           Data.Type.Equality ((:~:)(Refl))
import           Test.Hspec (describe, it, shouldBe, Spec)
import qualified Data.Map.Strict as DM
-- import qualified Data.Set as DS

import           Fcf (Eval, type (=<<), type (@@), Exp)
import qualified Fcf
import           Fcf.Class.Monoid (type (.<>))
import           Fcf.Data.Nat
import           Fcf.Data.MapC
-- import qualified Fcf.Data.Set as FS
import           Fcf.Data.Reflect (fromType)

spec :: Spec
spec = describe "MapC" $ do
  specQuery
  specConstruction

specQuery :: Spec
specQuery = describe "Query" $ do

  -- describe "Null" $ do
  --   it "Non-Empty Map" $ do
  --     let test :: forall r. (r ~ Null @@ (FromList @@ '[ '(1,"H"), '(2,"e"), '(3,"c"), '(5,"o"), '(4,"l"), '(3,"l")])) => Bool
  --         test = fromType (Proxy @r)
  --     test `shouldBe` DM.null (DM.fromList [(1,"H"),(2,"e"),(3,"c"),(5,"o"),(4,"l"),(3,"l")])
  --   it "Empty Map" $ do
  --     let test :: forall r. (r ~ Null @@ (FromList @@ '[])) => Bool
  --         test = fromType (Proxy @r)
  --     test `shouldBe` DM.null DM.empty

  describe "Size" $ do
    it "Map with 5 elem and Nat keys" $ do
      let test :: forall r. (r ~ Size (FromList @@ '[ '(1,"H"), '(2,"e"), '(3,"c"), '(5,"o"), '(4,"l"), '(3,"l")])) => Int
          test = fromType (Proxy @r)
      test `shouldBe` DM.size (DM.fromList [(1,"H"),(2,"e"),(3,"c"),(5,"o"),(4,"l"),(3,"l")])
    it "Map with 5 elem and String keys" $ do
      let test :: forall r. (r ~ Size (FromList @@ '[ '("H",1), '("ep",2), '("ec",3), '("o",5), '("l",4), '("l",3)])) => Int
          test = fromType (Proxy @r)
      test `shouldBe` DM.size (DM.fromList [("H",1),("ep",2),("ec",3),("o",5),("l",4),("l",3)])

  -- describe "Lookup" $ do
  --   it "basic test" $ do
  --     let test :: forall r. (r ~ Lookup 4 @@ (FromList @@ '[ '(1,"H"), '(3,"c"), '(5,"o"), '(4,"l"), '(3,"l")])) => Maybe String
  --         test = fromType (Proxy @r)
  --     test `shouldBe` DM.lookup 4 (DM.fromList [(1,"H"),(3,"c"),(5,"o"),(4,"l"),(3,"l")])
  --   it "duplicate key" $ do
  --     let test :: forall r. (r ~ Lookup "l" @@ (FromList @@ '[ '("H",1), '("ep",2), '("ec",3), '("o",5), '("l",4), '("l",3)])) => Maybe Int
  --         test = fromType (Proxy @r)
  --     test `shouldBe` DM.lookup "l" (DM.fromList [("H",1),("ep",2),("ec",3),("o",5),("l",4),("l",3)])
  --   it "Missing Key" $ do
  --     let test :: forall r. (r ~ Lookup "epl" @@ (FromList @@ '[ '("H",1), '("ep",2), '("ec",3), '("o",5), '("l",4), '("l",3)])) => Maybe Int
  --         test = fromType (Proxy @r)
  --     test `shouldBe` DM.lookup "epl" (DM.fromList [("H",1),("ep",2),("ec",3),("o",5),("l",4),("l",3)])
  --   it "after mutiple inserts" $ do
  --     let test :: forall r. 
  --           (r ~ Lookup "l" @@ (Fcf.Foldr (Uncurry1 Insert) (Eval Empty) @@ '[ '("H",1), '("ep",2), '("ec",3), '("o",5), '("l",4), '("l",3)])) 
  --           => Maybe Int
  --         test = fromType (Proxy @r)
  --     test `shouldBe` DM.lookup "l" (foldr (uncurry DM.insert) DM.empty [("H",1),("ep",2),("ec",3),("o",5),("l",4),("l",3)])

  -- describe "Member" $ do
  --   it "basic test" $ do
  --     let test :: forall r. (r ~ Member 4 @@ (FromList @@ '[ '(1,"H"), '(3,"c"), '(5,"o"), '(4,"l"), '(3,"l")])) => Bool
  --         test = fromType (Proxy @r)
  --     test `shouldBe` DM.member 4 (DM.fromList [(1,"H"),(3,"c"),(5,"o"),(4,"l"),(3,"l")])
  --   it "duplicate key" $ do
  --     let test :: forall r. (r ~ Member "l" @@ (FromList @@ '[ '("H",1), '("ep",2), '("ec",3), '("o",5), '("l",4), '("l",3)])) => Bool
  --         test = fromType (Proxy @r)
  --     test `shouldBe` DM.member "l" (DM.fromList [("H",1),("ep",2),("ec",3),("o",5),("l",4),("l",3)])
  --   it "Missing Key" $ do
  --     let test :: forall r. (r ~ Member "epl" @@ (FromList @@ '[ '("H",1), '("ep",2), '("ec",3), '("o",5), '("l",4), '("l",3)])) => Bool
  --         test = fromType (Proxy @r)
  --     test `shouldBe` DM.member "epl" (DM.fromList [("H",1),("ep",2),("ec",3),("o",5),("l",4),("l",3)])
  --   it "after mutiple inserts" $ do
  --     let test :: forall r. 
  --           (r ~ Member "l" @@ (Fcf.Foldr (Uncurry1 Insert) (Eval Empty) @@ '[ '("H",1), '("ep",2), '("ec",3), '("o",5), '("l",4), '("l",3)])) 
  --           => Bool
  --         test = fromType (Proxy @r)
  --     test `shouldBe` DM.member "l" (foldr (uncurry DM.insert) DM.empty [("H",1),("ep",2),("ec",3),("o",5),("l",4),("l",3)])

  -- describe "Disjoint" $ do
  --   it "Nothing in common maps" $ do
  --     let test :: forall r. (r ~ Disjoint (FromList @@ '[ '(9,"H"), '(8,"c"), '(6,"o"), '(7,"l"), '(8,"l")]) 
  --                                      @@ (FromList @@ '[ '(1,"H"), '(3,"c"), '(5,"o"), '(4,"l"), '(3,"l")])) => Bool
  --         test = fromType (Proxy @r)
  --     test `shouldBe` DM.disjoint (DM.fromList [(9,"H"),(8,"c"),(6,"o"),(7,"l"),(8,"l")]) 
  --                                 (DM.fromList [(1,"H"),(3,"c"),(5,"o"),(4,"l"),(3,"l")])
  --   it "one key in common" $ do
  --     let test :: forall r. (r ~ Disjoint (FromList @@ '[ '(9,"H"), '(8,"c"), '(5,"o"), '(7,"l"), '(8,"l")]) 
  --                                      @@ (FromList @@ '[ '(1,"H"), '(3,"c"), '(5,"o"), '(4,"l"), '(3,"l")])) => Bool
  --         test = fromType (Proxy @r)
  --     test `shouldBe` DM.disjoint (DM.fromList [(9,"H"),(8,"c"),(5,"o"),(7,"l"),(8,"l")]) 
  --                                 (DM.fromList [(1,"H"),(3,"c"),(5,"o"),(4,"l"),(3,"l")])
  --   it "one key in common after insert" $ do
  --     let test :: forall r. (r ~ Disjoint (Insert 5 "o" @@ (FromList @@ '[ '(9,"H"), '(8,"c"), '(6,"o"), '(7,"l"), '(8,"l")])) 
  --                                      @@ (FromList @@ '[ '(1,"H"), '(3,"c"), '(5,"o"), '(4,"l"), '(3,"l")])) => Bool
  --         test = fromType (Proxy @r)
  --     test `shouldBe` DM.disjoint (DM.insert 5 "o" $ DM.fromList [(9,"H"),(8,"c"),(6,"o"),(7,"l"),(8,"l")])
  --                                 (DM.fromList [(1,"H"),(3,"c"),(5,"o"),(4,"l"),(3,"l")])

  -- describe "Elems" $ do
  --   it "basic test" $ do
  --     let test :: forall r. (r ~ Elems @@ (FromList @@ '[ '(1,"H"), '(3,"c"), '(5,"o"), '(4,"l"), '(3,"l")])) => [String]
  --         test = fromType (Proxy @r)
  --     test `shouldBe` DM.elems (DM.fromList [(1,"H"),(3,"c"),(5,"o"),(4,"l"),(3,"l")])
  --   it "duplicate key" $ do
  --     let test :: forall r. (r ~ Elems @@ (FromList @@ '[ '("H",1), '("ep",2), '("ec",3), '("o",5), '("l",4), '("l",3)])) => [Int]
  --         test = fromType (Proxy @r)
  --     test `shouldBe` DM.elems (DM.fromList [("H",1),("ep",2),("ec",3),("o",5),("l",4),("l",3)])
  --   it "Missing Key" $ do
  --     let test :: forall r. (r ~ Elems @@ (FromList @@ '[ '("H",1), '("ep",2), '("ec",3), '("o",5), '("l",4), '("l",3)])) => [Int]
  --         test = fromType (Proxy @r)
  --     test `shouldBe` DM.elems (DM.fromList [("H",1),("ep",2),("ec",3),("o",5),("l",4),("l",3)])
  --   it "after mutiple inserts" $ do
  --     let test :: forall r. 
  --           (r ~ Elems @@ (Fcf.Foldr (Uncurry1 Insert) (Eval Empty) @@ '[ '("H",1), '("ep",2), '("ec",3), '("o",5), '("l",4), '("l",3)])) 
  --           => [Int]
  --         test = fromType (Proxy @r)
  --     test `shouldBe` DM.elems (foldr (uncurry DM.insert) DM.empty [("H",1),("ep",2),("ec",3),("o",5),("l",4),("l",3)])

  -- describe "Keys" $ do
  --   it "basic test" $ do
  --     let test :: forall r. (r ~ Keys @@ (FromList @@ '[ '(1,"H"), '(3,"c"), '(5,"o"), '(4,"l"), '(3,"l")])) => [Int]
  --         test = fromType (Proxy @r)
  --     test `shouldBe` DM.keys (DM.fromList [(1,"H"),(3,"c"),(5,"o"),(4,"l"),(3,"l")])
  --   it "duplicate key" $ do
  --     let test :: forall r. (r ~ Keys @@ (FromList @@ '[ '("H",1), '("ep",2), '("ec",3), '("o",5), '("l",4), '("l",3)])) => [String]
  --         test = fromType (Proxy @r)
  --     test `shouldBe` DM.keys (DM.fromList [("H",1),("ep",2),("ec",3),("o",5),("l",4),("l",3)])
  --   it "Missing Key" $ do
  --     let test :: forall r. (r ~ Keys @@ (FromList @@ '[ '("H",1), '("ep",2), '("ec",3), '("o",5), '("l",4), '("l",3)])) => [String]
  --         test = fromType (Proxy @r)
  --     test `shouldBe` DM.keys (DM.fromList [("H",1),("ep",2),("ec",3),("o",5),("l",4),("l",3)])
  --   it "after mutiple inserts" $ do
  --     let test :: forall r. 
  --           (r ~ Keys @@ (Fcf.Foldr (Uncurry1 Insert) (Eval Empty) @@ '[ '("H",1), '("ep",2), '("ec",3), '("o",5), '("l",4), '("l",3)])) 
  --           => [String]
  --         test = fromType (Proxy @r)
  --     test `shouldBe` DM.keys (foldr (uncurry DM.insert) DM.empty [("H",1),("ep",2),("ec",3),("o",5),("l",4),("l",3)])

  -- describe "Assocs" $ do
  --   it "basic test" $ do
  --     let test :: forall r. (r ~ Assocs @@ (FromList @@ '[ '(1,"H"), '(3,"c"), '(5,"o"), '(4,"l"), '(3,"l")])) => [(Int,String)]
  --         test = fromType (Proxy @r)
  --     test `shouldBe` DM.assocs (DM.fromList [(1,"H"),(3,"c"),(5,"o"),(4,"l"),(3,"l")])
  --   it "duplicate key" $ do
  --     let test :: forall r. (r ~ Assocs @@ (FromList @@ '[ '("H",1), '("ep",2), '("ec",3), '("o",5), '("l",4), '("l",3)])) => [(String,Int)]
  --         test = fromType (Proxy @r)
  --     test `shouldBe` DM.assocs (DM.fromList [("H",1),("ep",2),("ec",3),("o",5),("l",4),("l",3)])
  --   it "Missing Key" $ do
  --     let test :: forall r. (r ~ Assocs @@ (FromList @@ '[ '("H",1), '("ep",2), '("ec",3), '("o",5), '("l",4), '("l",3)])) => [(String,Int)]
  --         test = fromType (Proxy @r)
  --     test `shouldBe` DM.assocs (DM.fromList [("H",1),("ep",2),("ec",3),("o",5),("l",4),("l",3)])
  --   it "after mutiple inserts" $ do
  --     let test :: forall r. 
  --           (r ~ Assocs @@ (Fcf.Foldr (Uncurry1 Insert) (Eval Empty) @@ '[ '("H",1), '("ep",2), '("ec",3), '("o",5), '("l",4), '("l",3)])) 
  --           => [(String,Int)]
  --         test = fromType (Proxy @r)
  --     test `shouldBe` DM.assocs (foldr (uncurry DM.insert) DM.empty [("H",1),("ep",2),("ec",3),("o",5),("l",4),("l",3)])

specConstruction :: Spec
specConstruction = describe "Construction" $ do

  describe "Insert" $ do
    it "basic test" $ do
      let test :: forall r. (r ~ Insert 6 "test" @@ (FromList @@ '[ '(1,"H"), '(3,"c"), '(5,"o"), '(4,"l"), '(3,"l")])) => DM.Map Int String
          test = fromType (Proxy @r)
      test `shouldBe` DM.insert 6 "test" (DM.fromList [(1,"H"),(3,"c"),(5,"o"),(4,"l"),(3,"l")])
    it "duplicate key" $ do
      let test :: forall r. (r ~ Insert "l" 5 @@ (FromList @@ '[ '("H",1), '("ep",2), '("ec",3), '("o",5), '("l",4), '("l",3)])) => DM.Map String Int
          test = fromType (Proxy @r)
      test `shouldBe` DM.insert "l" 5 (DM.fromList [("H",1),("ep",2),("ec",3),("o",5),("l",4),("l",3)])
    it "multiple" $ do
      let test :: forall r. (r ~ Fcf.Foldr (Uncurry1 Insert) Empty @@ '[ '("H",1), '("ep",2), '("ec",3), '("o",5), '("l",4), '("l",3)]) 
            => DM.Map String Int
          test = fromType (Proxy @r)
      test `shouldBe` foldr (uncurry DM.insert) DM.empty [("H",1),("ep",2),("ec",3),("o",5),("l",4),("l",3)]

  -- describe "InsertWith" $ do
  --   it "basic test" $ do
  --     let test :: forall r. (r ~ InsertWith (.<>) 6 "test" @@ (FromList @@ '[ '(1,"H"), '(3,"c"), '(5,"o"), '(4,"l"), '(3,"l")])) => DM.Map Int String
  --         test = fromType (Proxy @r)
  --     test `shouldBe` DM.insertWith (<>) 6 "test" (DM.fromList [(1,"H"),(3,"c"),(5,"o"),(4,"l"),(3,"l")])
  --   it "duplicate key" $ do
  --     let test :: forall r. (r ~ InsertWith (+) "l" 5 @@ (FromList @@ '[ '("H",1), '("ep",2), '("ec",3), '("o",5), '("l",4), '("l",3)])) => DM.Map String Int
  --         test = fromType (Proxy @r)
  --     test `shouldBe` DM.insertWith (+) "l" 5 (DM.fromList [("H",1),("ep",2),("ec",3),("o",5),("l",4),("l",3)])
  --   it "multiple" $ do
  --     let test :: forall r. (r ~ Fcf.Foldr (Uncurry1 (InsertWith (+))) (Eval Empty) @@ '[ '("H",1), '("ep",2), '("ec",3), '("o",5), '("l",4), '("l",3)]) 
  --           => DM.Map String Int
  --         test = fromType (Proxy @r)
  --     test `shouldBe` foldr (uncurry (DM.insertWith (+))) DM.empty [("H",1),("ep",2),("ec",3),("o",5),("l",4),("l",3)]

  -- describe "Delete" $ do
  --   it "missing key" $ do
  --     let test :: forall r. (r ~ Delete 6 @@ (FromList @@ '[ '(1,"H"), '(3,"c"), '(5,"o"), '(4,"l"), '(3,"l")])) => DM.Map Int String
  --         test = fromType (Proxy @r)
  --     test `shouldBe` DM.delete 6 (DM.fromList [(1,"H"),(3,"c"),(5,"o"),(4,"l"),(3,"l")])
  --   it "duplicate key" $ do
  --     let test :: forall r. (r ~ Delete "l" @@ (FromList @@ '[ '("H",1), '("ep",2), '("ec",3), '("o",5), '("l",4), '("l",3)])) => DM.Map String Int
  --         test = fromType (Proxy @r)
  --     test `shouldBe` DM.delete "l" (DM.fromList [("H",1),("ep",2),("ec",3),("o",5),("l",4),("l",3)])
  --   it "after mutiple inserts" $ do
  --     let test :: forall r. 
  --           (r ~ Delete "ep" @@ (Fcf.Foldr (Uncurry1 Insert) (Eval Empty) @@ '[ '("H",1), '("ep",2), '("ec",3), '("o",5), '("l",4), '("l",3)])) 
  --           => DM.Map String Int
  --         test = fromType (Proxy @r)
  --     test `shouldBe` DM.delete "ep" (foldr (uncurry DM.insert) DM.empty [("H",1),("ep",2),("ec",3),("o",5),("l",4),("l",3)])

