
module Test.Alg where

import           Test.Alg.List     as L (spec)
import           Test.Alg.Morphism as M (spec)
import           Test.Alg.Nat      as N (spec)
import           Test.Alg.Other    as O (spec)
import           Test.Alg.Sort     as S (spec)
import           Test.Hspec (describe, Spec)

spec :: Spec
spec = describe "Alg" $ do
  L.spec
  M.spec
  N.spec
  O.spec
  S.spec

