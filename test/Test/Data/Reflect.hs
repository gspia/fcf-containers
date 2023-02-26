{-# LANGUAGE CPP                    #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module Test.Data.Reflect where

import qualified Data.IntMap as IM
import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Data.Tree as DT
#if __GLASGOW_HASKELL__ >= 902
import qualified Data.Text as DTxt
#endif
import           Data.Proxy

import           Fcf (Eval, type (=<<))
import qualified Fcf.Data.Set as FS
import qualified Fcf.Data.NatMap as FNM
import qualified Fcf.Data.MapC as FNMC
#if __GLASGOW_HASKELL__ >= 902
import qualified Fcf.Data.NewText as FTxt
#endif
import qualified Fcf.Data.Tree as FT
import           Fcf.Data.Reflect

tests :: Bool
tests = 
     testNatMap
  && testMap
  && testSet
#if __GLASGOW_HASKELL__ >= 902
  && testText
#endif
  && testTree

testNatMap 
  :: forall r. (r ~ Eval (FNM.Insert 3 "hih" =<< FNM.FromList '[ '(1,"haa"), '(2,"hoo")])) 
  => Bool
testNatMap = result == expected
  where
    result   = fromType @r Proxy :: IM.IntMap String
    expected = IM.fromList [ (3, "hih"), (1, "haa"), (2, "hoo")]

testMap 
  :: forall r. (r ~ Eval (FNMC.Insert 3 "hih" =<< FNMC.FromList '[ '(1,"haa"), '(2,"hoo")])) 
  => Bool
testMap = result == expected
  where
    result   = fromType @r Proxy :: DM.Map Int String
    expected = DM.fromList [ (3, "hih"), (1, "haa"), (2, "hoo")]

testSet
  :: forall r. (r ~ Eval (FS.FromList '[5, 9, 1, 8, 3, 5]))
  => Bool
testSet = result == expected
  where
    result   = fromType @r Proxy :: DS.Set Int
    expected = DS.fromList [1, 3, 5, 8, 9]
  

type ExT1r =
    'FT.Node 1 
       '[ 'FT.Node 2 
          '[ 'FT.Node 3 '[ 'FT.Node 4 '[]]]
        , 'FT.Node 5 '[ 'FT.Node 6 '[]]
        ]

-- type ExTr2 =
--     'Node ('Just 1)
--         '[ 'Node ('Just 2)
--             '[ 'Node ('Just 3)
--                 '[ 'Node ('Just 4) '[]]
--              ]
--          , 'Node ('Just 5)
--             '[ 'Node ('Just 6) '[]
--              ]
--          ]


testTree :: forall r. (r ~ ExT1r) => Bool
testTree = result == expected
  where
    result   = fromType @r Proxy :: DT.Tree Int
    expected = DT.Node 1 [DT.Node 2 [DT.Node 3 [DT.Node 4 []]], DT.Node 5 [DT.Node 6 []]]
    
    
#if __GLASGOW_HASKELL__ >= 902
testText :: forall r. (r ~ 'FTxt.Text "trial") => Bool
testText = result == expected
  where
    result   = fromType @r Proxy :: DTxt.Text
    expected = DTxt.pack "trial"
#endif
