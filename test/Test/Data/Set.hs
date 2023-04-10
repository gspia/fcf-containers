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
import           Data.Type.Equality ((:~:)(Refl))
import           Test.Hspec (describe, it, shouldBe, Spec)
import qualified Data.Set as DS

import           Fcf (Eval, type (=<<), Map)
import           Fcf.Data.Nat
import           Fcf.Data.Set
import           Fcf.Data.Reflect (fromType)
import           Fcf.Alg.Sort

--------------------------------------------------------------------------------

-- > :kind! Eval (Size =<< FromList '[5, 3])
-- Eval (Size =<< FromList '[5, 3]) :: TL.Natural
-- = 2

spec :: Spec
spec = describe "Set" $ do
  it "PowerSet" $ do
    let test :: forall r. (r ~ Eval (PowerSet  =<< FromList '[1,2,9,5])) => DS.Set (DS.Set Int)
        test = fromType (Proxy @r)
    test 
      `shouldBe` 
      DS.powerSet (DS.fromList [1,2,9,5])
  it "Size" $ do
    let test :: forall r. (r ~ Eval (Size  =<< FromList '[5,3])) => Int
        test = fromType (Proxy @r)
    test `shouldBe` 2

--------------------------------------------------------------------------------

-- This module won't compile, if the Refl doesn't give the correct answer.
--
-- > :kind! (Eval Empty :: Set Nat)
-- (Eval Empty :: Set Nat) :: Set TL.Natural
-- = 'Set '[]
_ = Refl :: (Eval Empty :: Set Nat)
  :~: 'Set '[]


-- This module won't compile, if the Refl doesn't give the correct answer.
--
-- > :kind! Eval (Singleton 1)
-- Eval (Singleton 1) :: Set TL.Natural
-- = 'Set '[1]
_ = Refl :: Eval (Singleton 1)
  :~: 'Set '[1]


-- > :kind! Eval (Insert 3 =<< FromList '[1, 2])
-- Eval (Insert 3 =<< FromList '[1, 2]) :: Set TL.Natural
-- = 'Set '[3, 1, 2]
_ = Refl :: Eval (Insert 3 =<< FromList '[1, 2])
  :~: 'Set '[3, 1, 2]

-- > :kind! Eval (Insert 2 =<< FromList '[1, 2])
-- Eval (Insert 2 =<< FromList '[1, 2]) :: Set TL.Natural
-- = 'Set '[1, 2]
_ = Refl :: Eval (Insert 2 =<< FromList '[1, 2])
  :~: 'Set '[1, 2]

-- > :kind! Eval (Delete 5 =<< FromList '[5, 3])
-- Eval (Delete 5 =<< FromList '[5, 3]) :: Set TL.Natural
-- = 'Set '[3]
_ = Refl :: Eval (Delete 5 =<< FromList '[5, 3])
  :~: 'Set '[3]

-- > :kind! Eval (Delete 7 =<< FromList '[5, 3])
-- Eval (Delete 7 =<< FromList '[5, 3]) :: Set TL.Natural
-- = 'Set '[5, 3]
_ = Refl :: Eval (Delete 7 =<< FromList '[5, 3])
  :~: 'Set '[5, 3]

-- > :kind! Eval (Member 5 =<< FromList '[5, 3])
-- Eval (Member 5 =<< FromList '[5, 3]) :: Bool
-- = 'True
_ = Refl :: Eval (Member 5 =<< FromList '[5, 3])
  :~: 'True

-- > :kind! Eval (Member 7 =<< FromList '[5, 3])
-- Eval (Member 7 =<< FromList '[5, 3]) :: Bool
-- = 'False
_ = Refl :: Eval (Member 7 =<< FromList '[5, 3])
  :~: 'False

-- > :kind! Eval (NotMember 5 =<< FromList '[5, 3])
-- Eval (NotMember 5 =<< FromList '[5, 3]) :: Bool
-- = 'False
_ = Refl :: Eval (NotMember 5 =<< FromList '[5, 3])
  :~: 'False


-- > :kind! Eval (NotMember 7 =<< FromList '[5, 3])
-- Eval (NotMember 7 =<< FromList '[5, 3]) :: Bool
-- = 'True
_ = Refl :: Eval (NotMember 7 =<< FromList '[5, 3])
  :~: 'True

-- > :kind! Eval (Null =<< FromList '[5, 3])
-- Eval (Null =<< FromList '[5, 3]) :: Bool
-- = 'False
_ = Refl :: Eval (Null =<< FromList '[5, 3])
  :~: 'False

-- > :kind! Eval (Null =<< Empty)
-- Eval (Null =<< Empty) :: Bool
-- = 'True
_ = Refl :: Eval (Null =<< Empty)
  :~: 'True

-- > :kind! Eval (IsSubsetOf ('Set '[]) ('Set '[0,1,2,3,4]))
-- Eval (IsSubsetOf ('Set '[]) ('Set '[0,1,2,3,4])) :: Bool
-- = 'True
_ = Refl :: Eval (IsSubsetOf ('Set '[]) ('Set '[0,1,2,3,4]))
  :~: 'True

-- > :kind! Eval (IsSubsetOf ('Set '[0,1]) ('Set '[0,1,2,3,4]))
-- Eval (IsSubsetOf ('Set '[0,1]) ('Set '[0,1,2,3,4])) :: Bool
-- = 'True
_ = Refl :: Eval (IsSubsetOf ('Set '[0,1]) ('Set '[0,1,2,3,4]))
  :~: 'True

-- > :kind! Eval (IsSubsetOf ('Set '[0,2,1,3,4]) ('Set '[0,1,2,3,4]))
-- Eval (IsSubsetOf ('Set '[0,2,1,3,4]) ('Set '[0,1,2,3,4])) :: Bool
-- = 'True
_ = Refl :: Eval (IsSubsetOf ('Set '[0,1,2,3,4]) ('Set '[0,1,2,3,4]))
  :~: 'True

-- > :kind! Eval (IsSubsetOf ('Set '[0,1,2,3,4,5]) ('Set '[0,1,2,3,4]))
-- Eval (IsSubsetOf ('Set '[0,1,2,3,4,5]) ('Set '[0,1,2,3,4])) :: Bool
-- = 'False
_ = Refl :: Eval (IsSubsetOf ('Set '[0,1,2,3,4,5]) ('Set '[0,1,2,3,4]))
  :~: 'False

-- > :kind! Eval (IsProperSubsetOf ('Set '[0,1,2,3,4]) ('Set '[0,1,2,3,4]))
-- Eval (IsProperSubsetOf ('Set '[0,1,2,3,4]) ('Set '[0,1,2,3,4])) :: Bool
-- = 'False
_ = Refl :: Eval (IsProperSubsetOf ('Set '[0,1,2,3,4]) ('Set '[0,1,2,3,4]))
  :~: 'False

-- > :kind! Eval (IsProperSubsetOf ('Set '[0,2,1,3]) ('Set '[0,1,2,3,4]))
-- Eval (IsProperSubsetOf ('Set '[0,2,1,3]) ('Set '[0,1,2,3,4])) :: Bool
-- = 'True
_ = Refl :: Eval (IsProperSubsetOf ('Set '[0,2,1,3]) ('Set '[0,1,2,3,4]))
  :~: 'True

-- > :kind! Eval (Union (Eval (FromList '[5, 3])) (Eval (FromList '[5, 7])) )
-- Eval (Union (Eval (FromList '[5, 3])) (Eval (FromList '[5, 7])) ) :: Set
--                                                                        TL.Natural
-- = 'Set '[7, 5, 3]
_ = Refl ::  Eval (Union (Eval (FromList '[5, 3])) (Eval (FromList '[5, 7])) )
  :~: 'Set '[7, 5, 3]

-- > :kind! Eval (Difference (Eval (FromList '[3, 5])) (Eval (FromList '[5, 7])))
-- Eval (Difference (Eval (FromList '[3, 5])) (Eval (FromList '[5, 7]))) :: Set
--                                                                            TL.Natural
-- = 'Set '[3]
_ = Refl :: Eval (Difference (Eval (FromList '[3, 5])) (Eval (FromList '[5, 7])))
  :~: 'Set '[3]

-- > :kind! Eval (Intersection (Eval (FromList '[3, 5])) (Eval (FromList '[5, 7])))
-- Eval (Intersection (Eval (FromList '[3, 5])) (Eval (FromList '[5, 7]))) :: Set
--                                                                              TL.Natural
-- = 'Set '[5]
_ = Refl :: Eval (Intersection (Eval (FromList '[3, 5])) (Eval (FromList '[5, 7]))) 
  :~: 'Set '[5]

-- > :kind! Eval (PowerSet =<< FromList '["a", "b", "c"])
-- Eval (PowerSet =<< FromList '["a", "b", "c"]) :: Set
--                                                    (Set TL.Symbol)
-- = 'Set
--     '[ 'Set '[], 'Set '["c"], 'Set '["b"], 'Set '["b", "c"],
--        'Set '["a"], 'Set '["a", "b"], 'Set '["a", "c"],
--        'Set '["a", "b", "c"]]
--
-- > :kind! Eval (PowerSet =<< FromList '[Int, Char, Maybe Int])
-- Eval (PowerSet =<< FromList '[Int, Char, Maybe Int]) :: Set
--                                                           (Set (*))
-- = 'Set
--     '[ 'Set '[], 'Set '[Maybe Int], 'Set '[Char],
--        'Set '[Char, Maybe Int], 'Set '[Int], 'Set '[Int, Char],
--        'Set '[Int, Maybe Int], 'Set '[Int, Char, Maybe Int]]
_ = Refl :: Eval (PowerSet =<< FromList '[Int, Char, Maybe Int])
  :~: 'Set
    '[ 'Set '[], 'Set '[Maybe Int], 'Set '[Char],
       'Set '[Char, Maybe Int], 'Set '[Int], 'Set '[Int, Char],
       'Set '[Int, Maybe Int], 'Set '[Int, Char, Maybe Int]]

-- > :kind! Eval (FromList '[1, 2])
-- Eval (FromList '[1, 2]) :: Set TL.Natural
-- = 'Set '[1, 2]
_ = Refl :: Eval (FromList '[1, 2])
  :~: 'Set '[1, 2]

-- > :kind! Eval (ToList =<< PowerSet =<< FromList '[1,2,3])
-- Eval (ToList =<< PowerSet =<< FromList '[1,2,3]) :: [Set
--                                                        TL.Natural]
-- = '[ 'Set '[], 'Set '[3], 'Set '[2], 'Set '[2, 3], 'Set '[1],
--      'Set '[1, 2], 'Set '[1, 3], 'Set '[1, 2, 3]]
_ = Refl :: Eval (ToList =<< PowerSet =<< FromList '[1,2,3])
  :~: '[ 'Set '[], 'Set '[3], 'Set '[2], 'Set '[2, 3], 'Set '[1],
         'Set '[1, 2], 'Set '[1, 3], 'Set '[1, 2, 3]]

-- > :kind! Eval (Qsort NatListOrd =<< Map ToList =<< ToList =<< PowerSet =<< FromList '[1,2,3])
-- Eval (Qsort NatListOrd =<< Map ToList =<< ToList =<< PowerSet =<< FromList '[1,2,3]) :: [[TL.Natural]]
-- = '[ '[], '[1], '[1, 2], '[1, 2, 3], '[1, 3], '[2], '[2, 3], '[3]]
_ = Refl :: Eval (Qsort NatListOrd =<< Map ToList =<< ToList =<< PowerSet =<< FromList '[1,2,3])
  :~: '[ '[], '[1], '[1, 2], '[1, 2, 3], '[1, 3], '[2], '[2, 3], '[3]]
