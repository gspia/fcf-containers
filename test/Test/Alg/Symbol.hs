{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

{- 
In this module, we test mainly Fcf.Alg.Symbol module. In couple of 
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

module Test.Alg.Symbol where

import           Data.Proxy
import           Data.Type.Equality ((:~:)(Refl))
import           Test.Hspec (describe, it, shouldBe, Spec)

import           Fcf.Core (Eval)
import           Fcf.Alg.Symbol
import           Fcf.Data.Reflect (fromType)

-- > :kind! Eval (Append "hmm" " ok")
-- Eval (Append "hmm" " ok") :: Symbol
-- = "hmm ok"
_ = Refl :: Eval (Append "hmm" " ok") :~: "hmm ok"

-- > :kind! Eval (Intercalate "+" '["aa", "bb", "cc"])
-- Eval (Intercalate "+" '["aa", "bb", "cc"]) :: Symbol
-- = "aa+bb+cc"
_ = Refl :: Eval (Intercalate "+" '["aa", "bb", "cc"]) :~: "aa+bb+cc"

-- > :kind! Eval (Intercalate "+" '["aa"])
-- Eval (Intercalate "+" '["aa"]) :: Symbol
-- = "aa"
_ = Refl :: Eval (Intercalate "+" '["aa"]) :~: "aa"

-- > :kind! Eval (Intercalate "+" '[])
-- Eval (Intercalate "+" '[]) :: Symbol
-- = ""
_ = Refl :: Eval (Intercalate "+" '[]) :~: ""

-- > :kind! Eval (IsSpace "a")
-- Eval (IsSpace "a") :: Bool
-- = 'False
_ = Refl :: Eval (IsSpace "a") :~: 'False
--
-- > :kind! Eval (IsSpace " ")
-- Eval (IsSpace " ") :: Bool
-- = 'True
_ = Refl :: Eval (IsSpace " ") :~: 'True

-- > :kind! Eval (IsNewLine "a")
-- Eval (IsNewLine "a") :: Bool
-- = 'False
_ = Refl :: Eval (IsNewLine "a") :~: 'False

-- > :kind! Eval (IsNewLine "\n")
-- Eval (IsNewLine "\n") :: Bool
-- = 'True
_ = Refl :: Eval (IsNewLine "\n") :~: 'True

-- > :kind! Eval (IsTab "a")
-- Eval (IsTab "a") :: Bool
-- = 'False
_ = Refl :: Eval (IsTab "a") :~: 'False
--
-- > :kind! Eval (IsTab "\t")
-- Eval (IsTab "\t") :: Bool
-- = 'True
_ = Refl :: Eval (IsTab "\t") :~: 'True

-- > :kind! Eval (IsSpaceDelim "a")
-- Eval (IsSpaceDelim "a") :: Bool
-- = 'False
_ = Refl :: Eval (IsSpaceDelim "a") :~: 'False
--
-- > :kind! Eval (IsSpaceDelim "\n")
-- Eval (IsSpaceDelim "\n") :: Bool
-- = 'True
_ = Refl :: Eval (IsSpaceDelim "\n") :~: 'True

-- > :kind! Eval (IsDigit "3")
-- Eval (IsDigit "3") :: Bool
-- = 'True
_ = Refl :: Eval (IsDigit "3") :~: 'True
--
-- > :kind! Eval (IsDigit "a")
-- Eval (IsDigit "a") :: Bool
-- = 'False
_ = Refl :: Eval (IsDigit "a") :~: 'False

-- > :kind! Eval (SymbolOrd "a" "b")
-- Eval (SymbolOrd "a" "b") :: Ordering
-- = 'LT
_ = Refl :: Eval (SymbolOrd "a" "b") :~: 'LT

--------------------------------------------------------------------------------

-- > :kind! Eval ("b" <= "a")
-- Eval ("b" <= "a") :: Bool
-- = 'False
--
-- > :kind! Eval ("b" >= "a")
-- Eval ("b" >= "a") :: Bool
-- = 'True
--
-- > :kind! Eval ("a" < "b")
-- Eval ("a" < "b") :: Bool
-- = 'True
--
-- > :kind! Eval ("b" > "a")
-- Eval ("b" > "a") :: Bool
-- = 'True
--
-- > :kind! Eval ("b" == "a")
-- Eval ("b" == "a") :: Bool
-- = 'False
spec :: Spec
spec = describe "Symbol" $ do
  it "<=" $ do
    let test :: forall r. (r ~ Eval ("b" <= "a")) => Bool
        test = fromType (Proxy @r)
    test `shouldBe` False
  it ">=" $ do
    let test :: forall r. (r ~ Eval ("b" >= "a")) => Bool
        test = fromType (Proxy @r)
    test `shouldBe` True
  it "<" $ do
    let test :: forall r. (r ~ Eval ("b" < "a")) => Bool
        test = fromType (Proxy @r)
    test `shouldBe` False
  it ">" $ do
    let test :: forall r. (r ~ Eval ("b" > "a")) => Bool
        test = fromType (Proxy @r)
    test `shouldBe` True
  it "==" $ do
    let test :: forall r. (r ~ Eval ("b" == "a")) => Bool
        test = fromType (Proxy @r)
    test `shouldBe` False
    