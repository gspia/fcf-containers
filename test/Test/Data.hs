
module Test.Data where

import           Test.Data.Reflect as R (spec)
import           Test.Data.Set as S (spec)
import           Test.Hspec (describe, Spec)

spec :: Spec
spec = describe "Data" $ do 
  R.spec
  S.spec

