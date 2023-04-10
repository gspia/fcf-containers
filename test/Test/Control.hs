
module Test.Control where

import           Test.Control.Monad as M (spec)
import           Test.Hspec (describe, Spec)

spec :: Spec
spec = describe "Control" $ do
  M.spec
