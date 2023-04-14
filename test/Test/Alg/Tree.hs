{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

{- 
In this module, we test mainly Fcf.Alg.Tree module. In couple of 
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

module Test.Alg.Tree where

import qualified GHC.TypeLits as TL

import           Data.Proxy
import           Data.Type.Equality ((:~:)(Refl))
import           Test.Hspec (describe, it, shouldBe, Spec)

import           Fcf.Data.Nat

import           Fcf.Alg.List
import           Fcf.Alg.Morphism 
import           Fcf.Alg.Tree
import           Fcf.Combinators (type (=<<))
import           Fcf.Core (Eval, Exp)
import           Fcf.Utils (If)
import           Fcf.Data.Reflect (fromType)
import           Fcf.Data.Tree

data BuildNode :: Nat -> Exp (Nat,[Nat])
type instance Eval (BuildNode x) =
    If (Eval ((2 TL.* x TL.+ 1) >= 8))
        '(x, '[])
        '(x, '[ 2 TL.* x, (2 TL.* x) TL.+ 1 ])

-- > :kind! Eval (Size =<< UnfoldTree BuildNode 1)
-- Eval (Size =<< UnfoldTree BuildNode 1) :: TL.Natural
-- = 7
_ = Refl :: Eval (Size =<< UnfoldTree BuildNode 1) :~: 7


-- :kind! Eval (Ana BuildNodeCoA 1)
-- :kind! Eval (Hylo CountNodesAlg BuildNodeCoA 1)


-- > :kind! Eval (Sizes =<< Ana BuildNodeCoA 1)
-- Eval (Sizes =<< Ana BuildNodeCoA 1) :: Fix
--                                          (AnnF (TreeF TL.Natural) TL.Natural)
-- = 'Fix
--     ('AnnF
--        '( 'NodeF
--             1
--             '[ 'Fix
--                  ('AnnF
--                     '( 'NodeF
--                          2
--                          '[ 'Fix ('AnnF '( 'NodeF 4 '[], 1)),
--                             'Fix ('AnnF '( 'NodeF 5 '[], 1))],
--                        3)),
--                'Fix
--                  ('AnnF
--                     '( 'NodeF
--                          3
--                          '[ 'Fix ('AnnF '( 'NodeF 6 '[], 1)),
--                             'Fix ('AnnF '( 'NodeF 7 '[], 1))],
--                        3))],
--           7))
_ = Refl :: Eval (Sizes =<< Ana BuildNodeCoA 1)
  :~: 'Fix
    ('AnnF
       '( 'NodeF
            1
            '[ 'Fix
                 ('AnnF
                    '( 'NodeF
                         2
                         '[ 'Fix ('AnnF '( 'NodeF 4 '[], 1)),
                            'Fix ('AnnF '( 'NodeF 5 '[], 1))],
                       3)),
               'Fix
                 ('AnnF
                    '( 'NodeF
                         3
                         '[ 'Fix ('AnnF '( 'NodeF 6 '[], 1)),
                            'Fix ('AnnF '( 'NodeF 7 '[], 1))],
                       3))],
          7))

-- Turning this off, because of the slowness. In practice, this would
-- also require turning on ghc-options (-freduction-depth=0). 
-- Please, do try on ghci.
-- > :kind! Eval (FibHisto 100)
-- Eval (FibHisto 100) :: TL.Natural
-- = 354224848179261915075
-- _ = Refl :: Eval (FibHisto 100) :~: 354224848179261915075

-- > :kind! Eval (FibHylo 10)
-- Eval (FibHylo 10) :: TL.Natural
-- = 55
spec :: Spec
spec = describe "Tree" $ do
  it "FibHisto" $ do
    let test :: forall r. (r ~ Eval (FibHylo 10)) => Int
        test = fromType (Proxy @r)
    test `shouldBe` 55
  -- The following is very slow and would require turning on ghc-options
  -- (-freduction-depth=0).  
  -- it "FibHisto" $ do
  --   let test :: forall r. (r ~ Eval (FibHisto 100)) => Integer
  --       test = fromType (Proxy @r)
  --   test `shouldBe` 354224848179261915075

