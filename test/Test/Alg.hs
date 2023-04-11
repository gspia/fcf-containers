
module Test.Alg where

import           Test.Alg.List as L (spec)
import           Test.Alg.Morphism as M (spec)
import           Test.Hspec (describe, Spec)

spec :: Spec
spec = describe "Alg" $ do
  L.spec
  M.spec

