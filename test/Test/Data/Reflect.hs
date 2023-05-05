{-# LANGUAGE CPP                    #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE TypeOperators          #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module Test.Data.Reflect where

import qualified Data.IntMap as IM
import qualified Data.Map.Strict as DM
import qualified Data.Set as DS
import qualified Data.Tree as DT
import qualified Data.Text as DTxt
import qualified GHC.TypeLits as TL
import           Data.Proxy
import           Test.Hspec (describe, it, shouldBe, Spec)

import           Fcf (Eval, type (=<<))
import qualified Fcf.Data.Set as FS
import qualified Fcf.Data.NatMap as FNM
import qualified Fcf.Data.MapC as FNMC
#if __GLASGOW_HASKELL__ >= 902
import qualified Fcf.Data.NewText as FTxt
#endif
import qualified Fcf.Data.Tree as FT
import           Fcf.Data.Reflect

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Reflect" $ do
  specTrees
  specMaybe
  specEither
  specMaybeEither
  specStructures
  specShowTypeable
  specErrorMessage
  
specBool :: Spec
specBool = describe "Bool" $ do
  it "Bool, True" $ do
    let test :: forall r. (r ~ 'True) => Bool
        test = fromType (Proxy @r)
    test `shouldBe` True
  it "Bool, False" $ do
    let test :: forall r. (r ~ 'False) => Bool
        test = fromType (Proxy @r)
    test `shouldBe` False

specMaybe :: Spec
specMaybe = describe "Maybe" $ do
  it "Maybe Int, Just" $ do
    let test :: forall r. (r ~ 'Just 5) 
             => Maybe Int
        test = fromType (Proxy @r)
    test `shouldBe` Just 5
  it "Maybe Int, Nothing" $ do
    let test :: forall r. (r ~ 'Nothing) 
             => Maybe Int
        test = fromType (Proxy @r)
    test `shouldBe` Nothing
  it "Maybe String, Just" $ do
    let test :: forall r. (r ~ 'Just "just") 
             => Maybe String
        test = fromType (Proxy @r)
    test `shouldBe` Just "just"
  it "Maybe String, Nothing" $ do
    let test :: forall r. (r ~ 'Nothing) 
             => Maybe String
        test = fromType (Proxy @r)
    test `shouldBe` Nothing

specEither :: Spec
specEither = describe "Either" $ do
  it "Either Int String, Left" $ do
    let test :: forall r. (r ~ 'Left 5) 
             => Either Int String
        test = fromType (Proxy @r)
    test `shouldBe` Left 5
  it "Either Int String, Right" $ do
    let test :: forall r. (r ~ 'Right "right") 
             => Either Int String
        test = fromType (Proxy @r)
    test `shouldBe` Right "right"
  it "Either Int (Either Int String), Right Right" $ do
    let test :: forall r. (r ~ 'Right ('Right "right"))
             => Either Int (Either Int String)
        test = fromType (Proxy @r)
    test `shouldBe` Right (Right "right")
  it "Either (Either Int String) String, Left Left" $ do
    let test :: forall r. (r ~ 'Left ('Left 5)) 
             => Either (Either Int String) String
        test = fromType (Proxy @r)
    test `shouldBe` Left (Left 5)

specMaybeEither :: Spec
specMaybeEither = describe "Maybe Either and Either Maybe" $ do
  it "Either Int (Maybe String), Right Just" $ do
    let test :: forall r. (r ~ 'Right ('Just "right just"))
             => Either Int (Maybe String)
        test = fromType (Proxy @r)
    test `shouldBe` Right (Just "right just")
  it "Either Int (Maybe String), Right Nothing" $ do
    let test :: forall r. (r ~ 'Right 'Nothing)
             => Either Int (Maybe String)
        test = fromType (Proxy @r)
    test `shouldBe` Right Nothing
  it "Maybe (Either Int String), Just Right" $ do
    let test :: forall r. (r ~ 'Just ('Right "just right"))
             => Maybe (Either Int String)
        test = fromType (Proxy @r)
    test `shouldBe` Just (Right "just right")
  it "Maybe (Either Int String), Nothing" $ do
    let test :: forall r. (r ~ 'Nothing)
             => Maybe (Either Int String)
        test = fromType (Proxy @r)
    test `shouldBe` Nothing


specStructures :: Spec
specStructures = describe "Maps and other structures" $ do
  describe "[]" $ do
    it "[]" $ do
      let test :: forall r. (r ~ '[]) => [()]
          test = fromType (Proxy @r)
      test `shouldBe` []
    it "[(Bool,Int)]" $ do
      let test :: forall r. (r ~ '[ '( 'True,5), '( 'False,6)]) => [(Bool,Int)]
          test = fromType (Proxy @r)
      test `shouldBe` [(True,5),(False,6)]
  describe "IntMap" $ do
#if __GLASGOW_HASKELL__ >= 902
    it "IntMap char, from '[ '(Nat,Char) ]" $ do
      let test :: forall r. (r ~ '[ '(1,'H'), '(2,'e'), '(5,'o'), '(3,'b'), '(4,'l'), '(3,'l')]) => IM.IntMap Char
          test = fromType (Proxy @r)
      test `shouldBe` IM.fromList [(1,'H'),(2,'e'),(5,'o'),(4,'l'),(3,'l')]
#endif
    it "IntMap String, from NatMap" $ do
      let test :: forall r. (r ~ Eval (FNM.FromList '[ '(1,"H"), '(4,"b"), '(2,"e"), '(5,"o"), '(4,"l"), '(3,"l")])) => IM.IntMap String
          test = fromType (Proxy @r)
      test `shouldBe` IM.fromList [(2,"e"),(1,"H"),(4,"l"),(3,"l"),(5,"o")]
    it "IntMap String, with insert" $ do
      let test :: forall r. (r ~ Eval (
                    FNM.Insert 3 "hih" =<< FNM.FromList '[ '(1,"haa"), '(2,"hoo")]
                  )) 
              => IM.IntMap String
          test = fromType (Proxy @r)
      test 
        `shouldBe` 
        IM.fromList [ (3, "hih"), (1, "haa"), (2, "hoo")]
  describe "Map" $ do
#if __GLASGOW_HASKELL__ >= 902
    it "Map Int char, from '[ '(Nat,Char) ]" $ do
      let test :: forall r. (r ~ '[ '(1,'H'), '(2,'e'), '(5,'o'), '(4,'l'), '(3,'b'), '(3,'l')]) => DM.Map Int Char
          test = fromType (Proxy @r)
      test `shouldBe` DM.fromList [(1,'H'),(2,'e'),(5,'o'),(4,'l'),(3,'l')]
#endif
    it "Map Int String, from MapC" $ do
      fromType @(DM.Map Int String) (Proxy @(Eval (FNMC.FromList '[ '(1,"H"), '(2,"e"), '(3,"c"), '(5,"o"), '(4,"l"), '(3,"l")])))
        `shouldBe` 
        DM.fromList [(1,"H"),(2,"e"),(3,"c"),(5,"o"),(4,"l"),(3,"l")]
    it "Map Int String, with insert" $ do
      fromType (Proxy @(Eval (FNMC.Insert 3 "hih" =<< FNMC.FromList '[ '(1,"haa"), '(2,"hoo")])))
        `shouldBe` 
        DM.fromList [ (3, "hih"), (1, "haa"), (2, "hoo")]
  describe "Set" $ do
#if __GLASGOW_HASKELL__ >= 902
    it "Set char, from '[Char]" $ do
      let test :: forall r. (r ~ '[ 'H','e','o','l','l' ]) => DS.Set Char
          test = fromType (Proxy @r)
      test `shouldBe` DS.fromList ['H','e','o','l','l']
#endif
    it "Set String, from Set" $ do
      let test :: forall r. (r ~ Eval (FS.FromList '["H","e","o","l","l"])) => DS.Set String
          test = fromType (Proxy @r)
      test `shouldBe` DS.fromList ["e","H","l","l","o"]
    it "Set Int" $ do
      let test :: forall r. (r ~ Eval (FS.FromList '[5, 9, 1, 8, 3, 5])) 
              => DS.Set Int
          test = fromType (Proxy @r)
      test 
        `shouldBe` 
        DS.fromList [1, 3, 5, 8, 9]
#if __GLASGOW_HASKELL__ >= 902
  it "text" $ do
    let test :: forall r. (r ~ 'FTxt.Text "trial") => DTxt.Text
        test = fromType (Proxy @r)
    test 
      `shouldBe` 
      DTxt.pack "trial"
#endif

type ExTr1 =
    'FT.Node 1 
       '[ 'FT.Node 2 
          '[ 'FT.Node 3 '[ 'FT.Node 4 '[]]]
        , 'FT.Node 5 '[ 'FT.Node 6 '[]]
        ]

type ExTr2 =
    'FT.Node ('Just 1)
        '[ 'FT.Node ('Just 2)
            '[ 'FT.Node ('Just 3)
                '[ 'FT.Node ('Just 4) '[]]
             ]
         , 'FT.Node ('Just 5)
            '[ 'FT.Node ('Just 6) '[]
             ]
         ]

type ExTr3 =
    'FT.Node ('Just 1)
        '[ 'FT.Node ('Just 2)
            '[ 'FT.Node ('Just 3)
                '[ 'FT.Node ('Just 4) '[]]
             ]
         , 'FT.Node ('Just 5)
            '[ 'FT.Node 'Nothing '[]
             ]
         ]

type ExTr4 =
    'FT.Node ('Left 1)
        '[ 'FT.Node ('Right "two")
            '[ 'FT.Node ('Left 3)
                '[ 'FT.Node ('Right "four") '[]]
             ]
         , 'FT.Node ('Left 5)
            '[ 'FT.Node ('Right "six") '[]
             ]
         ]
    
specTrees :: Spec
specTrees = describe "Tree structures" $ do
  it "tree 1" $ do
    let test :: forall r. (r ~ ExTr1) 
             => DT.Tree Int
        test = fromType (Proxy @r)
    test 
      `shouldBe` 
      DT.Node 1 [DT.Node 2 [DT.Node 3 [DT.Node 4 []]], DT.Node 5 [DT.Node 6 []]]
  it "tree 2" $ do
    let test :: forall r. (r ~ ExTr2) 
             => DT.Tree (Maybe Int)
        test = fromType (Proxy @r)
    test 
      `shouldBe` 
      DT.Node (Just 1) 
        [ DT.Node (Just 2) 
          [ DT.Node (Just 3) 
            [ DT.Node (Just 4) []
            ]
          ]
        , DT.Node (Just 5) 
          [DT.Node (Just 6) []
          ]
        ]
  it "tree 3" $ do
    let test :: forall r. (r ~ ExTr3) 
             => DT.Tree (Maybe Int)
        test = fromType (Proxy @r)
    test 
      `shouldBe` 
      DT.Node (Just 1) 
        [ DT.Node (Just 2) 
          [ DT.Node (Just 3) 
            [ DT.Node (Just 4) []
            ]
          ]
        , DT.Node (Just 5) 
          [DT.Node Nothing []
          ]
        ]
  it "tree 4" $ do
    let test :: forall r. (r ~ ExTr4) 
             => DT.Tree (Either Int String)
        test = fromType (Proxy @r)
    test 
      `shouldBe` 
      DT.Node (Left 1) 
        [ DT.Node (Right "two") 
          [ DT.Node (Left 3) 
            [ DT.Node (Right "four") []
            ]
          ]
        , DT.Node (Left 5) 
          [DT.Node (Right "six") []
          ]
        ]

specShowTypeable :: Spec
specShowTypeable = describe "Show Type represented at the Kind level" $ do
  it "Show Int" $ do
    fromType @String (Proxy @Int)
      `shouldBe`
      "Int"
  it "Show Set of Types" $ do
    let test :: forall r. (r ~ Eval (FS.FromList '[Int, Maybe String, (), [Integer], IO ()])) 
             => DS.Set String
        test = fromType (Proxy @r)
    test 
      `shouldBe` 
      DS.fromList ["Int", "Maybe [Char]", "()", "[Integer]", "IO ()"]

specErrorMessage :: Spec
specErrorMessage = describe "show GHC.TypeLits.ErrorMessage" $ do
  it "text error" $ do
    (fromType @DTxt.Text $ Proxy @('TL.Text "I am error"))
      `shouldBe`
      DTxt.pack "I am error"
  it "ShowType 'True" $ do
    (fromType @DTxt.Text $ Proxy @('TL.ShowType '()))
      `shouldBe`
      DTxt.pack "'()"
  it "with Kinds" $ do
    (fromType @DTxt.Text $ Proxy @('TL.Text "Kind: " 'TL.:<>: 'TL.ShowType 'True 'TL.:$$: 'TL.Text "Type: " 'TL.:<>: 'TL.ShowType Bool))
      `shouldBe`
      DTxt.pack "Kind: 'True\nType: Bool"
