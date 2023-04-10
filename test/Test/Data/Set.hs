{-# LANGUAGE CPP                    #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE DataKinds              #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module Test.Data.Set where

import           Data.Proxy
import           Test.Hspec (describe, it, shouldBe, Spec)
import qualified Data.Set as DS

import           Fcf (Eval, type (=<<))
import           Fcf.Data.Set
import           Fcf.Data.Reflect (fromType)

spec :: Spec
spec = describe "Set" $ do
  it "PowerSet" $ do
    let test :: forall r. (r ~ Eval (PowerSet  =<< FromList '[1,2,9,5])) => DS.Set (DS.Set Int)
        test = fromType (Proxy @r)
    test 
      `shouldBe` 
      DS.powerSet (DS.fromList [1,2,9,5])
