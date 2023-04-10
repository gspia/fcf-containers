{-# LANGUAGE
    DataKinds,
    KindSignatures,
    TypeFamilies,
    UndecidableInstances,
    TypeOperators #-}

--------------------------------------------------------------------------------

import           Data.Type.Equality ((:~:)(Refl))
import           GHC.TypeLits (Nat, Symbol)
import           Fcf hiding (type (<*>))

--------------------------------------------------------------------------------

import           Fcf.Data.Set
import           Fcf.Control.Monad
import           Fcf.Data.Tuple
import           Fcf.Class.Monoid
import           Fcf.Data.List

--------------------------------------------------------------------------------

import           Test.Alg as A (spec)
import           Test.Data as D (spec)
import           Test.Control as C (spec)
import           Test.Hspec (hspec, parallel)

main :: IO ()
main = hspec $ parallel $ do
  D.spec
  C.spec
  A.spec
