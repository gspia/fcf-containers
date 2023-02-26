{-# LANGUAGE
    DataKinds,
    KindSignatures,
    TypeOperators #-}

--------------------------------------------------------------------------------

import           Data.Type.Equality ((:~:)(Refl))
import           GHC.TypeLits (Nat)
import           Fcf

--------------------------------------------------------------------------------

import           Fcf.Data.Set

--------------------------------------------------------------------------------

import           Test.Data (tests)



-- Compile-time tests

_ = Refl :: Eval (ToList =<< PowerSet  =<< FromList '[1,2])
        :~: '[ 'Set '[], 'Set '[2], 'Set '[1], 'Set '[1,2]]

-- Dummy

main :: IO ()
main = 
  if tests 
    then pure ()
    else error "Tests not ok."

