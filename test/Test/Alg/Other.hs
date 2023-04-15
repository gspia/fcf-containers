{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

{- 
In this module, we test mainly Fcf.Alg.Other module. In couple of 
places, we use the "spec" and Reflect. 

Many of the tests are such that if the answer is not correct, the
whole test module refuses to compile.  Please do try to change some 
of the values to see. 

Note that in a way, the spec tests are not needed. If testing just 
the type level behaviour, the module would refuse to compile if 
test is not ok (and thus, guard the quality earlier than what the 
test framework can do).  

Anyhow, as these also work as examples, it is probably ok to have different 
ways of reaching the results. (E.g. to see, how to get the type level 
values to value level.)

-}
module Test.Alg.Other where

import           Data.Proxy
import           Data.Type.Equality ((:~:)(Refl))
import           Test.Hspec (describe, it, shouldBe, Spec)

import           Fcf.Core (Eval)
import           Fcf.Alg.Other
import           Fcf.Data.Reflect (fromType)

-- > :kind! Eval (PairMaybeToMaybePair '( 'Just "txt", 'Just 1))
-- Eval (PairMaybeToMaybePair '( 'Just "txt", 'Just 1)) :: Maybe
--                                                           (Symbol, TL.Natural)
-- = 'Just '("txt", 1)
_ = Refl :: Eval (PairMaybeToMaybePair '( 'Just "txt", 'Just 1)) 
  :~: 'Just '("txt", 1)


-- > :kind! Eval (Id "id")
-- Eval (Id "id") :: Symbol
-- = "id"
spec :: Spec
spec = describe "Other" $ do
  it "Id" $ do
    let test :: forall r. (r ~ Eval (Id "id")) => String
        test = fromType (Proxy @r)
    test `shouldBe` "id"
