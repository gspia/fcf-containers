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
module Test.Alg.List where

import           Data.Proxy
import           Data.Type.Equality ((:~:)(Refl))
import           Test.Hspec (describe, it, shouldBe, Spec)

import           Fcf.Core (Eval)
import           Fcf.Combinators (type (=<<))
import           Fcf.Alg.List
import           Fcf.Alg.Morphism
import           Fcf.Data.Reflect (fromType)


-- This module won't compile, if the Refl doesn't give the correct answer.
--
-- > :kind! Eval (ListToFix '[1,2,3])
-- Eval (ListToFix '[1,2,3]) :: Fix (ListF TL.Natural)
-- = 'Fix ('ConsF 1 ('Fix ('ConsF 2 ('Fix ('ConsF 3 ('Fix 'NilF))))))
_ = Refl :: Eval (ListToFix '[1,2,3])
  :~: 'Fix ('ConsF 1 ('Fix ('ConsF 2 ('Fix ('ConsF 3 ('Fix 'NilF))))))


-- This module won't compile, if the Refl doesn't give the correct answer.
--  
-- > :kind! Eval (ListToParaFix '[1,2,3])
-- Eval (ListToParaFix '[1,2,3]) :: Fix
--                                    (ListF (TL.Natural, [TL.Natural]))
-- = 'Fix
--     ('ConsF
--        '(1, '[2, 3])
--        ('Fix ('ConsF '(2, '[3]) ('Fix ('ConsF '(3, '[]) ('Fix 'NilF))))))
_ = Refl :: Eval (ListToParaFix '[1,2,3])
  :~: 'Fix ('ConsF
       '(1, '[2, 3])
       ('Fix ('ConsF '(2, '[3]) ('Fix ('ConsF '(3, '[]) ('Fix 'NilF))))))


-- This module won't compile, if the Refl doesn't give the correct answer.
--  
-- > :kind! Eval (Para DedupAlg =<< ListToParaFix '[1,1,3,2,5,1,3,2])
-- Eval (Para DedupAlg =<< ListToParaFix '[1,1,3,2,5,1,3,2]) :: [TL.Natural]
-- = '[5, 1, 3, 2]
_ = Refl :: Eval (Para DedupAlg =<< ListToParaFix '[1,1,3,2,5,1,3,2])
  :~: '[5, 1, 3, 2]


-- This module won't compile, if the Refl doesn't give the correct answer.
--  
-- > :kind! Eval (Sliding 3 '[1,2,3,4,5,6])
-- Eval (Sliding 3 '[1,2,3,4,5,6]) :: [[TL.Natural]]
-- = '[ '[1, 2, 3], '[2, 3, 4], '[3, 4, 5], '[4, 5, 6], '[5, 6], '[6]]
_ = Refl :: Eval (Sliding 3 '[1,2,3,4,5,6])
  :~: '[ '[1, 2, 3], '[2, 3, 4], '[3, 4, 5], '[4, 5, 6], '[5, 6], '[6]]


-- This module won't compile, if the Refl doesn't give the correct answer.
--  
-- > :kind! Eval (Evens =<< RunInc 8)
-- Eval (Evens =<< RunInc 8) :: [TL.Natural]
-- = '[2, 4, 6, 8]
_ = Refl :: Eval (Evens =<< RunInc 8)
  :~: '[2, 4, 6, 8]


-- This module won't compile, if the Refl doesn't give the correct answer.
--  
-- > :kind! Eval (RunInc 8)
-- Eval (RunInc 8) :: [TL.Natural]
-- = '[1, 2, 3, 4, 5, 6, 7, 8]
_ = Refl :: Eval (RunInc 8)
  :~: '[1, 2, 3, 4, 5, 6, 7, 8]


-- This module won't compile, if the Refl doesn't give the correct answer.
--  
-- > :kind! Eval (MToN 1 3)
-- Eval (MToN 1 3) :: [TL.Natural]
-- = '[1, 2, 3]
_ = Refl :: Eval (MToN 1 3)
  :~: '[1, 2, 3]

-- This module won't compile, if the Refl doesn't give the correct answer.
--  
-- > :kind! Eval (ToList 1)
-- Eval (ToList 1) :: [TL.Natural]
-- = '[1]
_ = Refl :: Eval (ToList 1)
  :~: '[1]

  
--------------------------------------------------------------------------------

-- > :kind! Eval (Cata LenAlg =<< ListToFix '[1,2,3])
-- Eval (Cata LenAlg =<< ListToFix '[1,2,3]) :: TL.Natural
-- = 3

-- > :kind! Eval (Cata SumAlg =<< ListToFix '[1,2,3,4])
-- Eval (Cata SumAlg =<< ListToFix '[1,2,3,4]) :: TL.Natural
-- = 10

-- > :kind! Eval (Cata ProdAlg =<< ListToFix '[1,2,3,4])
-- Eval (Cata ProdAlg =<< ListToFix '[1,2,3,4]) :: TL.Natural
-- = 24

-- > :kind! Eval (Sum '[1,2,3])
-- Eval (Sum '[1,2,3]) :: TL.Natural
-- = 6

-- > :kind! Eval (Equal '[1,2,3] '[1,2,3])
-- Eval (Equal '[1,2,3] '[1,2,3]) :: Bool
-- = 'True

-- > :kind! Eval (Equal '[1,2,3] '[1,3,2])
-- Eval (Equal '[1,2,3] '[1,3,2]) :: Bool
-- = 'False

spec :: Spec
spec = describe "List" $ do
  it "Cata LenAlg" $ do
    let test :: forall r. (r ~ Eval (Cata LenAlg =<< ListToFix '[1,2,3])) => Int
        test = fromType (Proxy @r)
    test `shouldBe` 3
  it "Cata SumAlg" $ do
    let test :: forall r. (r ~ Eval (Cata SumAlg =<< ListToFix '[1,2,3,4])) => Int
        test = fromType (Proxy @r)
    test `shouldBe` 10
  it "Cata ProdAlg" $ do
    let test :: forall r. (r ~ Eval (Cata ProdAlg =<< ListToFix '[1,2,3,4])) => Int
        test = fromType (Proxy @r)
    test `shouldBe` 24
  it "Sum" $ do
    let test :: forall r. (r ~ Eval (Sum '[1,2,3])) => Int
        test = fromType (Proxy @r)
    test `shouldBe` 6
  it "Equal, True" $ do
    let test :: forall r. (r ~ Eval (Equal '[1,2,3] '[1,2,3])) => Bool
        test = fromType (Proxy @r)
    test `shouldBe` True
  it "Equal, False" $ do
    let test :: forall r. (r ~ Eval (Equal '[1,2,3] '[1,3,2])) => Bool
        test = fromType (Proxy @r)
    test `shouldBe` False

