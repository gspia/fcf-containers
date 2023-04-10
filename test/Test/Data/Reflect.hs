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
import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Data.Tree as DT
#if __GLASGOW_HASKELL__ >= 902
import qualified Data.Text as DTxt
#endif
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

type ExT1r =
    'FT.Node 1 
       '[ 'FT.Node 2 
          '[ 'FT.Node 3 '[ 'FT.Node 4 '[]]]
        , 'FT.Node 5 '[ 'FT.Node 6 '[]]
        ]

-- type ExTr2 =
--     'Node ('Just 1)
--         '[ 'Node ('Just 2)
--             '[ 'Node ('Just 3)
--                 '[ 'Node ('Just 4) '[]]
--              ]
--          , 'Node ('Just 5)
--             '[ 'Node ('Just 6) '[]
--              ]
--          ]

spec :: Spec
spec = describe "Reflect" $ do
  it "NatMap" $ do
    let test :: forall r. (r ~ Eval (FNM.Insert 3 "hih" =<< FNM.FromList '[ '(1,"haa"), '(2,"hoo")])) => IM.IntMap String
        test = fromType (Proxy @r)
    test 
      `shouldBe` 
      IM.fromList [ (3, "hih"), (1, "haa"), (2, "hoo")]
  it "Map" $ do
    let test :: forall r. (r ~ Eval (FNMC.Insert 3 "hih" =<< FNMC.FromList '[ '(1,"haa"), '(2,"hoo")])) => DM.Map Int String
        test = fromType (Proxy @r)
    test 
      `shouldBe` 
      DM.fromList [ (3, "hih"), (1, "haa"), (2, "hoo")]
  it "Set" $ do
    let test :: forall r. (r ~ Eval (FS.FromList '[5, 9, 1, 8, 3, 5])) => DS.Set Int
        test = fromType (Proxy @r)
    test 
      `shouldBe` 
      DS.fromList [1, 3, 5, 8, 9]
  it "tree" $ do
    let test :: forall r. (r ~ ExT1r) => DT.Tree Int
        test = fromType (Proxy @r)
    test 
      `shouldBe` 
      DT.Node 1 [DT.Node 2 [DT.Node 3 [DT.Node 4 []]], DT.Node 5 [DT.Node 6 []]]
#if __GLASGOW_HASKELL__ >= 902
  it "text" $ do
    let test :: forall r. (r ~ 'FTxt.Text "trial") => DTxt.Text
        test = fromType (Proxy @r)
    test 
      `shouldBe` 
      DTxt.pack "trial"
#endif
