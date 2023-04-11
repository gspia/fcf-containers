{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

{- 
In this module, we test mainly Fcf.Alg.List module. In couple of 
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
module Test.Alg.Morphism where

import qualified GHC.TypeLits as TL

import           Data.Proxy
import           Data.Type.Equality ((:~:)(Refl))
import           Test.Hspec (describe, it, shouldBe, Spec)

import           Fcf.Core (Eval)
import           Fcf.Utils (If)
import           Fcf.Alg.List
import           Fcf.Alg.Morphism
import           Fcf.Data.Nat
import           Fcf.Data.Reflect (fromType)

--------------------------------------------------------------------------------

-- For the Ana and Hylo tests made below.
data NToOneCoA :: CoAlgebra (ListF Nat) Nat
type instance Eval (NToOneCoA b) =
  If (Eval (b < 1) )
      'NilF
      ('ConsF b ( b TL.- 1))


-- This module won't compile, if the Refl doesn't give the correct answer.
--
-- > :kind! Eval (Ana NToOneCoA 3)
-- Eval (Ana NToOneCoA 3) :: Fix (ListF TL.Natural)
-- = 'Fix ('ConsF 3 ('Fix ('ConsF 2 ('Fix ('ConsF 1 ('Fix 'NilF))))))
_ = Refl :: Eval (Ana NToOneCoA 3)
  :~: 'Fix ('ConsF 3 ('Fix ('ConsF 2 ('Fix ('ConsF 1 ('Fix 'NilF))))))

-- > :kind! Eval (Second ((+) 1) '("a",3))
-- Eval (Second ((+) 1) '("a",3)) :: (TL.Symbol, TL.Natural)
-- = '("a", 4)
_ = Refl :: Eval (Second ((+) 1) '("a",3))
  :~: '("a", 4)


-- > :kind! Eval (Hylo SumAlg NToOneCoA 5)
-- Eval (Hylo SumAlg NToOneCoA 5) :: TL.Natural
-- = 15

-- > :kind! Eval (First ((+) 1) '(3,"a"))
-- Eval (First ((+) 1) '(3,"a")) :: (TL.Natural, TL.Symbol)
-- = '(4, "a")

spec :: Spec
spec = describe "Morphism" $ do
  it "Hylo SumAlg" $ do
    let test :: forall r. (r ~ Eval (Hylo SumAlg NToOneCoA 5)) => Int
        test = fromType (Proxy @r)
    test `shouldBe` 15
  it "First" $ do
    let test :: forall r. (r ~ Eval (First ((+) 1) '(3,"a"))) => (Int, String)
        test = fromType (Proxy @r)
    test `shouldBe` (4,"a")
