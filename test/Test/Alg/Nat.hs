{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

{- 
In this module, we test mainly Fcf.Alg.Nat module. In couple of 
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

module Test.Alg.Nat where

import           Data.Proxy
import           Data.Type.Equality ((:~:)(Refl))
import           Test.Hspec (describe, it, shouldBe, Spec)

import           Fcf.Core (Eval)
import           Fcf.Alg.Nat
import           Fcf.Data.Reflect (fromType)

-- > :kind! Eval (2 == 2)
-- Eval (2 == 2) :: Bool
-- = 'True
_ = Refl :: Eval (2==2) :~: 'True

-- > :kind! Eval (2 == 3)
-- Eval (2 == 3) :: Bool
-- = 'False
_ = Refl :: Eval (2==3) :~: 'False


-- > :kind! Eval (2 /= 2)
-- Eval (2 /= 2) :: Bool
-- = 'False
--
-- > :kind! Eval (2 /= 3)
-- Eval (2 /= 3) :: Bool
-- = 'True
spec :: Spec
spec = describe "Nat" $ do
  it "In-equality, false" $ do
    let test :: forall r. (r ~ Eval (2 /= 2)) => Bool
        test = fromType (Proxy @r)
    test `shouldBe` False
  it "In-equality, true" $ do
    let test :: forall r. (r ~ Eval (2 /= 3)) => Bool
        test = fromType (Proxy @r)
    test `shouldBe` True
