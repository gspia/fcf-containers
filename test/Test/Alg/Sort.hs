{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

{- 
In this module, we test mainly Fcf.Alg.Sort module. In couple of 
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

module Test.Alg.Sort where

import           Data.Proxy
import           Data.Type.Equality ((:~:)(Refl))
import           Test.Hspec (describe, it, shouldBe, Spec)

import           Fcf.Core (Eval)
import           Fcf.Alg.Sort
import qualified Fcf.Alg.Symbol as S ( type (<) )
import qualified Fcf.Data.Nat as N ( type (<) )
import           Fcf.Data.Reflect (fromType)

-- > :kind! Eval (Qsort (N.<) '[5,3,1,9,4,6,3])
-- Eval (Qsort (N.<) '[5,3,1,9,4,6,3]) :: [TL.Natural]
-- = '[1, 3, 3, 4, 5, 6, 9]
spec :: Spec
spec = describe "Sort" $ do
  it "QSort (<)" $ do
    let test :: forall r. (r ~ Eval (Qsort (N.<) '[5,3,1,9,4,6,3])) => [Int]
        test = fromType (Proxy @r)
    test `shouldBe` [1, 3, 3, 4, 5, 6, 9]

-- > :kind! Eval (Qsort (S.<) '[ "bb", "e", "a", "e", "d" ])
-- Eval (Qsort (S.<) '[ "bb", "e", "a", "e", "d" ]) :: [Symbol]
-- = '["a", "bb", "d", "e", "e"]
_ = Refl :: Eval (Qsort (S.<) '[ "bb", "e", "a", "e", "d" ])
  :~: '["a", "bb", "d", "e", "e"]

