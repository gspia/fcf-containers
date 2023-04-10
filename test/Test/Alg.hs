
module Test.Alg where

import           Test.Alg.List as L (spec)
import           Test.Hspec (describe, Spec)

spec :: Spec
spec = describe "Data"  
  L.spec

