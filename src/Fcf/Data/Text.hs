{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

{-|
Module      : Fcf.Data.Text
Description : Type-level Text data structure with methods
Copyright   : (c) gspia 2020-
License     : BSD
Maintainer  : gspia

= Fcf.Data.Text

We mimick Data.Text but on type level. The internal representation is based on
type level lists.

-}

--------------------------------------------------------------------------------

module Fcf.Data.Text
    ( Text (..)

    -- * Creation
    , Empty
    , Singleton
    , FromList
    , ToList
    , ToSymbol

    -- * Basic Interface
    , Null
    , Length
    , Append
    , Cons
    , Snoc
    , Uncons
    , Unsnoc
    , Head
    , Tail
    , Init
    , CompareLength

    -- * Transformation
    , FMap
    , Intercalate
    , Intersperse
    , Reverse
    , Replace

    -- * Special Folds
    , Concat
    , FConcatMap
    , Any
    , All

    -- * Substrings
    , Take
    , TakeEnd
    , Drop
    , DropEnd
    , TakeWhile
    , TakeWhileEnd
    , DropWhile
    , DropWhileEnd
    , DropAround
    , Strip

    -- * Breaking etc
    , SplitOn
    , Split
    , Lines
    , Words
    , Unlines
    , Unwords

    -- * Predicates
    , IsPrefixOf
    , IsSuffixOf
    , IsInfixOf
    )
  where

import qualified GHC.TypeLits as TL

import           Fcf ( If, Eval, Exp, type (=<<), type (@@)
                     , Flip, Pure )
import qualified Fcf.Class.Foldable as F (All, Any)
import           Fcf.Data.List ( type (++) )
import qualified Fcf.Data.List as F
    ( Length, Head, Tail, Init, Reverse, Take, Drop, TakeWhile, DropWhile
    , Foldr, Intercalate, Intersperse, IsPrefixOf, IsSuffixOf, IsInfixOf, Snoc)
import           Fcf.Data.Symbol (Symbol)
-- import qualified Fcf.Data.Symbol as FS

import qualified Fcf.Class.Functor as F ( FMap )


import           Fcf.Data.Nat (Nat)
import           Fcf.Alg.Morphism (First,Second)
import qualified Fcf.Alg.Symbol as S

--------------------------------------------------------------------------------


-- For the doctests:

-- $setup
-- >>> import           Fcf (type (<=<), Not)

--------------------------------------------------------------------------------

-- | 'Text' is a data structure, that is, a list to hold type-level symbols of
-- length one.
data Text = Text [Symbol]

--------------------------------------------------------------------------------

-- | Empty
-- 
-- === __Example__
-- 
-- >>> :kind! (Eval Empty :: Text)
-- (Eval Empty :: Text) :: Text
-- = 'Text '[]
--
-- See also the other examples in this module.
data Empty :: Exp Text
type instance Eval Empty = 'Text '[]

-- | Singleton
-- 
-- === __Example__
-- 
-- >>> :kind! Eval (Singleton "a")
-- Eval (Singleton "a") :: Text
-- = 'Text '["a"]
data Singleton :: Symbol -> Exp Text
type instance Eval (Singleton s) = 'Text '[s]



-- | Use FromList to construct a Text from type-level list.
--
-- === __Example__
-- 
-- :kind! Eval (FromList '["h", "e", "l", "l", "u", "r", "e", "i"])
-- Eval (FromList '["h", "e", "l", "l", "u", "r", "e", "i"]) :: Text
-- = 'Text '["h", "e", "l", "l", "u", "r", "e", "i"]
data FromList :: [Symbol] -> Exp Text
type instance Eval (FromList lst) = 'Text lst

-- | Get the type-level list out of the 'Text'.
--
-- === __Example__
-- 
-- >>> :kind! Eval (ToList =<< FromList '["a", "b"])
-- Eval (ToList =<< FromList '["a", "b"]) :: [Symbol]
-- = '["a", "b"]
data ToList :: Text -> Exp [Symbol]
type instance Eval (ToList ('Text lst)) = lst


-- | ToSymbol
--
-- === __Example__
-- 
-- >>> :kind! Eval (ToSymbol =<< FromList '["w", "o", "r", "d"])
-- Eval (ToSymbol =<< FromList '["w", "o", "r", "d"]) :: Symbol
-- = "word"
data ToSymbol :: Text -> Exp Symbol
type instance Eval (ToSymbol ('Text lst)) = Eval (F.Foldr S.Append "" lst)



-- | Null
--
-- === __Example__
-- 
-- >>> :kind! Eval (Null =<< FromList '["a", "b"])
-- Eval (Null =<< FromList '["a", "b"]) :: Bool
-- = 'False
-- >>> :kind! Eval (Null =<< Empty)
-- Eval (Null =<< Empty) :: Bool
-- = 'True
data Null :: Text -> Exp Bool
type instance Eval (Null ('Text '[])) = 'True
type instance Eval (Null ('Text (_ ': _))) = 'False


-- | Length
--
-- === __Example__
-- 
-- >>> :kind! Eval (Length =<< FromList '["a", "b"])
-- Eval (Length =<< FromList '["a", "b"]) :: Nat
-- = 2
data Length :: Text -> Exp Nat
type instance Eval (Length ('Text lst)) = Eval (F.Length lst)


-- | Add a symbol to the beginning of a type-level text.
--
-- === __Example__
-- 
-- >>> :kind! Eval (Cons "h" ('Text '["a", "a", "m", "u"]))
-- Eval (Cons "h" ('Text '["a", "a", "m", "u"])) :: Text
-- = 'Text '["h", "a", "a", "m", "u"]
data Cons :: Symbol -> Text -> Exp Text
type instance Eval (Cons s ('Text lst)) = 'Text (s ': lst)

-- | Add a symbol to the end of a type-level text.
--
-- === __Example__
-- 
-- >>> :kind! Eval (Snoc ('Text '["a", "a", "m"]) "u")
-- Eval (Snoc ('Text '["a", "a", "m"]) "u") :: Text
-- = 'Text '["a", "a", "m", "u"]
data Snoc :: Text -> Symbol -> Exp Text
type instance Eval (Snoc ('Text lst) s) = 'Text (Eval (lst ++ '[s]))

-- | Append two type-level texts.
--
-- === __Example__
--
-- >>> :kind! Eval (Append ('Text '["a", "a"]) ('Text '["m", "u"]))
-- Eval (Append ('Text '["a", "a"]) ('Text '["m", "u"])) :: Text
-- = 'Text '["a", "a", "m", "u"]
data Append :: Text -> Text -> Exp Text
type instance Eval (Append ('Text l1) ('Text l2)) = 'Text (Eval (l1 ++ l2))


-- | Get the first symbol from type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (Uncons ('Text '["h", "a", "a", "m", "u"]))
-- Eval (Uncons ('Text '["h", "a", "a", "m", "u"])) :: Maybe
--                                                       (Symbol, Text)
-- = 'Just '("h", 'Text '["a", "a", "m", "u"])
--
-- >>> :kind! Eval (Uncons ('Text '[]))
-- Eval (Uncons ('Text '[])) :: Maybe (Symbol, Text)
-- = 'Nothing
data Uncons :: Text -> Exp (Maybe (Symbol, Text))
type instance Eval (Uncons ('Text '[])) = 'Nothing
type instance Eval (Uncons ('Text (t ': txt))) = 'Just '(t, 'Text txt)


-- | Get the last symbol from type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (Unsnoc ('Text '["a", "a", "m", "u", "n"]))
-- Eval (Unsnoc ('Text '["a", "a", "m", "u", "n"])) :: Maybe
--                                                       (Symbol, Text)
-- = 'Just '("n", 'Text '["a", "a", "m", "u"])
--
-- >>> :kind! Eval (Unsnoc ('Text '[]))
-- Eval (Unsnoc ('Text '[])) :: Maybe (Symbol, Text)
-- = 'Nothing
data Unsnoc :: Text -> Exp (Maybe (Symbol, Text))
type instance Eval (Unsnoc txt) =
    Eval (F.FMap (Second Reverse) =<< Uncons =<< Reverse txt)


-- | Get the first symbol of type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (Head ('Text '["a", "a", "m", "u"]))
-- Eval (Head ('Text '["a", "a", "m", "u"])) :: Maybe Symbol
-- = 'Just "a"
-- 
-- >>> :kind! Eval (Head ('Text '[]))
-- Eval (Head ('Text '[])) :: Maybe Symbol
-- = 'Nothing
data Head :: Text -> Exp (Maybe Symbol)
type instance Eval (Head ('Text lst)) = Eval (F.Head lst)

-- | Get the tail of a type-level text.
--
-- === __Example__
-- 
-- >>> :kind! Eval (Tail ('Text '["h", "a", "a", "m", "u"]))
-- Eval (Tail ('Text '["h", "a", "a", "m", "u"])) :: Maybe Text
-- = 'Just ('Text '["a", "a", "m", "u"])
--
-- >>> :kind! Eval (Tail ('Text '[]))
-- Eval (Tail ('Text '[])) :: Maybe Text
-- = 'Nothing
data Tail :: Text -> Exp (Maybe Text)
type instance Eval (Tail ('Text lst)) = Eval (F.FMap FromList =<< F.Tail lst)

-- | Take all except the last symbol from type-level text.
--
-- === __Example__
-- 
-- >>> :kind! Eval (Init ('Text '["a", "a", "m", "u", "n"]))
-- Eval (Init ('Text '["a", "a", "m", "u", "n"])) :: Maybe Text
-- = 'Just ('Text '["a", "a", "m", "u"])
--
-- >>> :kind! Eval (Init ('Text '[]))
-- Eval (Init ('Text '[])) :: Maybe Text
-- = 'Nothing
data Init :: Text -> Exp (Maybe Text)
type instance Eval (Init ('Text lst)) = Eval (F.FMap FromList =<< F.Init lst)


-- | Compare the length of type-level text to given Nat and give
-- the Ordering.
--
-- === __Example__
-- 
-- >>> :kind! Eval (CompareLength ('Text '["a", "a", "m", "u"]) 3)
-- Eval (CompareLength ('Text '["a", "a", "m", "u"]) 3) :: Ordering
-- = 'GT
data CompareLength :: Text -> Nat -> Exp Ordering
type instance Eval (CompareLength txt n) = TL.CmpNat (Length @@ txt) n



-- | FMap for type-level text.
--
-- === __Example__
-- 
-- >>> :{
-- data IsIsymb :: Symbol -> Exp Bool
-- type instance Eval (IsIsymb s) = Eval ("i" S.== s)
-- data Isymb2e :: Symbol -> Exp Symbol
-- type instance Eval (Isymb2e s) = Eval
--     (If (IsIsymb @@ s)
--         (Pure "e")
--         (Pure s)
--     )
-- :}
-- 
-- >>> :kind! Eval (FMap Isymb2e ('Text '["i","m","u"]))
-- Eval (FMap Isymb2e ('Text '["i","m","u"])) :: Text
-- = 'Text '["e", "m", "u"]
data FMap :: (Symbol -> Exp Symbol) -> Text -> Exp Text
type instance Eval (FMap f ('Text lst)) = 'Text (Eval (F.FMap f lst))


-- | Intercalate for type-level text.
-- 
-- === __Example__
-- 
-- >>> :kind! Eval (Intercalate ('Text '[" ", "&", " "]) ('[ 'Text '["a", "a", "m", "u"], 'Text '["v", "a", "l", "o"]]))
-- Eval (Intercalate ('Text '[" ", "&", " "]) ('[ 'Text '["a", "a", "m", "u"], 'Text '["v", "a", "l", "o"]])) :: Text
-- = 'Text '["a", "a", "m", "u", " ", "&", " ", "v", "a", "l", "o"]
data Intercalate :: Text -> [Text] -> Exp Text
type instance Eval (Intercalate ('Text txt) txts) =
    Eval (FromList =<< F.Intercalate txt =<< F.FMap ToList txts)


-- | Intersperse for type-level text.
--
-- === __Example__
-- 
-- >>> :kind! Eval (Intersperse "." ('Text '["a", "a", "m", "u"]))
-- Eval (Intersperse "." ('Text '["a", "a", "m", "u"])) :: Text
-- = 'Text '["a", ".", "a", ".", "m", ".", "u"]
data Intersperse :: Symbol -> Text -> Exp Text
type instance Eval (Intersperse s ('Text txt)) = Eval (FromList =<< F.Intersperse s txt)

-- | Reverse for type-level text.
--
-- === __Example__
-- 
-- >>> :kind! Eval (Reverse ('Text '["a", "a", "m", "u"]))
-- Eval (Reverse ('Text '["a", "a", "m", "u"])) :: Text
-- = 'Text '["u", "m", "a", "a"]
--
-- >>> :kind! Eval (Reverse =<< Reverse ('Text '["a", "a", "m", "u"]))
-- Eval (Reverse =<< Reverse ('Text '["a", "a", "m", "u"])) :: Text
-- = 'Text '["a", "a", "m", "u"]
data Reverse :: Text -> Exp Text
type instance Eval (Reverse ('Text lst)) = 'Text (Eval (F.Reverse lst))

-- | Replace for type-level text.
--
-- === __Example__
-- 
-- >>> :kind! Eval (Replace ('Text '["t","u"]) ('Text '["l","a"]) ('Text '["t","u","u","t","u","t","t","a","a"]))
-- Eval (Replace ('Text '["t","u"]) ('Text '["l","a"]) ('Text '["t","u","u","t","u","t","t","a","a"])) :: Text
-- = 'Text '["l", "a", "u", "l", "a", "t", "t", "a", "a"]
data Replace :: Text -> Text -> Text -> Exp Text
type instance Eval (Replace orig new txt) =
    Eval (Intercalate new =<< SplitOn orig txt)


-- | Concat for type-level text.
--
-- === __Example__
-- 
-- >>> :kind! Eval (Concat '[ 'Text '["l","a"], 'Text '["k","a","n","a"]])
-- Eval (Concat '[ 'Text '["l","a"], 'Text '["k","a","n","a"]]) :: Text
-- = 'Text '["l", "a", "k", "a", "n", "a"]
data Concat :: [Text] -> Exp Text
type instance Eval (Concat lst) = Eval (F.Foldr Append (Eval Empty :: Text) lst)



-- | FConcatMap for type-level text.
--
-- === __Example__
--
-- >>> :{
-- data IsIsymb :: Symbol -> Exp Bool
-- type instance Eval (IsIsymb s) = Eval ("i" S.== s)
-- data Isymb2aa :: Symbol -> Exp Text
-- type instance Eval (Isymb2aa s) = Eval
--     (If (IsIsymb @@ s)
--         (Pure ('Text '["a","a"]))
--         (Pure ('Text '[s]))
--     )
-- :}
--
-- >>> :kind! Eval (FConcatMap Isymb2aa ('Text '["i","m","u"," ","i","h"]))
-- Eval (FConcatMap Isymb2aa ('Text '["i","m","u"," ","i","h"])) :: Text
-- = 'Text '["a", "a", "m", "u", " ", "a", "a", "h"]
data FConcatMap :: (Symbol -> Exp Text) -> Text -> Exp Text
type instance Eval (FConcatMap f ('Text lst)) = Eval (Concat =<< F.FMap f lst)

-- | Any for type-level text.
--
-- === __Example__
-- 
-- >>> :kind! Eval (Any S.IsDigit ('Text '["a","a","m","u","1"]))
-- Eval (Any S.IsDigit ('Text '["a","a","m","u","1"])) :: Bool
-- = 'True
--
-- >>> :kind! Eval (Any S.IsDigit ('Text '["a","a","m","u"]))
-- Eval (Any S.IsDigit ('Text '["a","a","m","u"])) :: Bool
-- = 'False
data Any :: (Symbol -> Exp Bool) -> Text -> Exp Bool
type instance Eval (Any f ('Text lst)) = Eval (F.Any f lst)


-- | All for type-level text.
--
-- === __Example__
-- 
-- >>> :kind! Eval (All S.IsDigit ('Text '["a","a","m","u","1"]))
-- Eval (All S.IsDigit ('Text '["a","a","m","u","1"])) :: Bool
-- = 'False
--
-- >>> :kind! Eval (All S.IsDigit ('Text '["3","2","1"]))
-- Eval (All S.IsDigit ('Text '["3","2","1"])) :: Bool
-- = 'True
data All :: (Symbol -> Exp Bool) -> Text -> Exp Bool
type instance Eval (All f ('Text lst)) = Eval (F.All f lst)





-- | Take for type-level text.
--
-- === __Example__
-- 
-- >>> :kind! Eval (Take 4 ('Text '["a", "a", "m", "u", "n"]))
-- Eval (Take 4 ('Text '["a", "a", "m", "u", "n"])) :: Text
-- = 'Text '["a", "a", "m", "u"]
data Take :: Nat -> Text -> Exp Text
type instance Eval (Take n ('Text lst)) = 'Text (Eval (F.Take n lst))


-- | TakeEnd for type-level text.
--
-- === __Example__
-- 
-- >>> :kind! Eval (TakeEnd 4 ('Text '["h", "a", "a", "m", "u"]))
-- Eval (TakeEnd 4 ('Text '["h", "a", "a", "m", "u"])) :: Text
-- = 'Text '["a", "a", "m", "u"]
data TakeEnd :: Nat -> Text -> Exp Text
type instance Eval (TakeEnd n ('Text lst)) =
    'Text (Eval (F.Reverse =<< F.Take n =<< F.Reverse lst))


-- | Drop for type-level text.
--
-- === __Example__
-- 
-- >>> :kind! Eval (Drop 2 ('Text '["a", "a", "m", "u", "n", "a"]))
-- Eval (Drop 2 ('Text '["a", "a", "m", "u", "n", "a"])) :: Text
-- = 'Text '["m", "u", "n", "a"]
data Drop :: Nat -> Text -> Exp Text
type instance Eval (Drop n ('Text lst)) = 'Text (Eval (F.Drop n lst))

-- | DropEnd for type-level text.
--
-- === __Example__
-- 
-- >>> :kind! Eval (DropEnd 2 ('Text '["a", "a", "m", "u", "n", "a"]))
-- Eval (DropEnd 2 ('Text '["a", "a", "m", "u", "n", "a"])) :: Text
-- = 'Text '["a", "a", "m", "u"]
data DropEnd :: Nat -> Text -> Exp Text
type instance Eval (DropEnd n ('Text lst)) =
    'Text (Eval (F.Reverse =<< F.Drop n =<< F.Reverse lst))


-- | TakeWhile for type-level text.
--
-- === __Example__
-- 
-- >>> :kind! Eval (TakeWhile (Not <=< S.IsDigit) ('Text '["a","a","m","u","1","2"]))
-- Eval (TakeWhile (Not <=< S.IsDigit) ('Text '["a","a","m","u","1","2"])) :: Text
-- = 'Text '["a", "a", "m", "u"]
data TakeWhile :: (Symbol -> Exp Bool) -> Text -> Exp Text
type instance Eval (TakeWhile f ('Text lst)) = 'Text (Eval (F.TakeWhile f lst))


-- | TakeWhileEnd for type-level text.
--
-- === __Example__
-- 
-- >>> :kind! Eval (TakeWhileEnd (Not <=< S.IsDigit) ('Text '["1","2","a","a","m","u"]))
-- Eval (TakeWhileEnd (Not <=< S.IsDigit) ('Text '["1","2","a","a","m","u"])) :: Text
-- = 'Text '["a", "a", "m", "u"]
data TakeWhileEnd :: (Symbol -> Exp Bool) -> Text -> Exp Text
type instance Eval (TakeWhileEnd f ('Text lst)) =
    'Text (Eval (F.Reverse =<< F.TakeWhile f =<< F.Reverse lst))


-- | DropWhile for type-level text.
--
-- === __Example__
-- 
-- >>> :kind! Eval (DropWhile S.IsDigit ('Text '["1","2","a","a","m","u"]))
-- Eval (DropWhile S.IsDigit ('Text '["1","2","a","a","m","u"])) :: Text
-- = 'Text '["a", "a", "m", "u"]
data DropWhile :: (Symbol -> Exp Bool) -> Text -> Exp Text
type instance Eval (DropWhile f ('Text lst)) = 'Text (Eval (F.DropWhile f lst))


-- | DropWhileEnd for type-level text.
-- === __Example__
-- 
-- >>> :kind! Eval (DropWhileEnd S.IsDigit ('Text '["a","a","m","u","1","2"]))
-- Eval (DropWhileEnd S.IsDigit ('Text '["a","a","m","u","1","2"])) :: Text
-- = 'Text '["a", "a", "m", "u"]
data DropWhileEnd :: (Symbol -> Exp Bool) -> Text -> Exp Text
type instance Eval (DropWhileEnd f ('Text lst)) =
    'Text (Eval (F.Reverse =<< F.DropWhile f =<< F.Reverse lst))


-- | DropAround for type-level text.
--
-- === __Example__
-- 
-- >>> :kind! Eval (DropAround S.IsDigit ('Text '["3","4","a","a","m","u","1","2"]))
-- Eval (DropAround S.IsDigit ('Text '["3","4","a","a","m","u","1","2"])) :: Text
-- = 'Text '["a", "a", "m", "u"]
data DropAround :: (Symbol -> Exp Bool) -> Text -> Exp Text
type instance Eval (DropAround f txt) = Eval (DropWhile f =<< DropWhileEnd f txt)



-- | Strip the space, newline and tab -symbols from the beginning and and
-- of type-level text.
--
-- === __Example__
-- 
-- >>> :kind! Eval (Strip ('Text '[" ", " ", "a", "a", "m", "u", " ", "\n"]))
-- Eval (Strip ('Text '[" ", " ", "a", "a", "m", "u", " ", "\n"])) :: Text
-- = 'Text '["a", "a", "m", "u"]
data Strip :: Text -> Exp Text
type instance Eval (Strip txt) = Eval (DropAround S.IsSpaceDelim txt)


-- | SplitOn for type-level text.
--
-- === __Example__
-- 
-- >>> :kind! Eval (SplitOn (Eval (FromList '["a", "b"])) (Eval (FromList '[ "c", "d", "a", "b", "f", "g", "a", "b", "h"])))
-- Eval (SplitOn (Eval (FromList '["a", "b"])) (Eval (FromList '[ "c", "d", "a", "b", "f", "g", "a", "b", "h"]))) :: [Text]
-- = '[ 'Text '["c", "d"], 'Text '["f", "g"], 'Text '["h"]]
data SplitOn :: Text -> Text -> Exp [Text]
type instance Eval (SplitOn ('Text sep) ('Text txt)) =
    Eval (F.FMap FromList =<< SOLoop sep '( '[], txt))


-- | Helper for SplitOn
--
-- >>> :kind! Eval (SOTake '["a", "b"] '[ "c", "d", "a", "b", "f", "g"] '[])
-- Eval (SOTake '["a", "b"] '[ "c", "d", "a", "b", "f", "g"] '[]) :: ([Symbol],
--                                                                    [Symbol])
-- = '( '["c", "d"], '["f", "g"])
data SOTake :: [Symbol] -> [Symbol] -> [Symbol] -> Exp ([Symbol], [Symbol])
type instance Eval (SOTake sep '[] accum) = '(accum, '[])
type instance Eval (SOTake sep (t ': txt) accum) = Eval
    (If (Eval (F.IsPrefixOf sep (t ': txt)))
        (Pure '(accum, Eval (F.Drop (Eval (F.Length sep)) (t ': txt))))
        (SOTake sep txt (Eval (accum ++ '[t])))
    )

-- | Helper for SplitOn
data SOLoop :: [Symbol] -> ([[Symbol]],[Symbol]) -> Exp [[Symbol]]
type instance Eval (SOLoop sep '(acc, '[])) = acc
type instance Eval (SOLoop sep '(acc, (t ': txt))) =
    Eval (SOLoop sep =<< First (F.Snoc acc) =<< SOTake sep (t ': txt) '[])


-- | Split for type-level text.
--
-- === __Example__
-- 
-- >>> :kind! Eval (Split S.IsSpace (Eval (FromList '[ "c", "d", " ", "b", "f", " ", "a", "b", "h"])))
-- Eval (Split S.IsSpace (Eval (FromList '[ "c", "d", " ", "b", "f", " ", "a", "b", "h"]))) :: [Text]
-- = '[ 'Text '["c", "d"], 'Text '["b", "f"], 'Text '["a", "b", "h"]]
data Split :: (Symbol -> Exp Bool) -> Text -> Exp [Text]
type instance Eval (Split p ('Text txt)) =
    Eval (F.FMap FromList =<< SplitLoop p '( '[], txt))

-- | Helper for Split
data SplitTake :: (Symbol -> Exp Bool) -> [Symbol] -> [Symbol] -> Exp ([Symbol], [Symbol])
type instance Eval (SplitTake p '[] accum) = '(accum, '[])
type instance Eval (SplitTake p (t ': txt) accum) = Eval
    (If (Eval (p t))
        (Pure '(accum, txt))
        (SplitTake p txt (Eval (accum ++ '[t])))
    )

-- | Helper for Split
data SplitLoop :: (Symbol -> Exp Bool) -> ([[Symbol]],[Symbol]) -> Exp [[Symbol]]
type instance Eval (SplitLoop p '(acc, '[])) = acc
type instance Eval (SplitLoop p '(acc, (t ': txt))) =
    Eval (SplitLoop p =<< First (F.Snoc acc) =<< SplitTake p (t ': txt) '[])



-- | Lines for type-level text.
--
-- === __Example__
-- 
-- >>> :kind! Eval (Lines =<< FromList '[ "o", "k", "\n", "h", "m", "m ", "\n", "a", "b"])
-- Eval (Lines =<< FromList '[ "o", "k", "\n", "h", "m", "m ", "\n", "a", "b"]) :: [Text]
-- = '[ 'Text '["o", "k"], 'Text '["h", "m", "m "], 'Text '["a", "b"]]
data Lines :: Text -> Exp [Text]
type instance Eval (Lines txt) = Eval (Split S.IsNewLine txt)

-- | Words for type-level text.
--
-- === __Example__
-- 
-- >>> :kind! Eval (Words =<< FromList '[ "o", "k", " ", "h", "m", "m ", "\n", "a", "b"])
-- Eval (Words =<< FromList '[ "o", "k", " ", "h", "m", "m ", "\n", "a", "b"]) :: [Text]
-- = '[ 'Text '["o", "k"], 'Text '["h", "m", "m "], 'Text '["a", "b"]]
data Words :: Text -> Exp [Text]
type instance Eval (Words txt) = Eval (Split S.IsSpaceDelim txt)

-- | Unlines for type-level text. This adds a newline to each Text and then
-- concats them.
--
-- === __Example__
-- 
-- >>> :kind! Eval (Unlines '[ 'Text '["o", "k"], 'Text '["h", "m", "m "], 'Text '["a", "b"]])
-- Eval (Unlines '[ 'Text '["o", "k"], 'Text '["h", "m", "m "], 'Text '["a", "b"]]) :: Text
-- = 'Text '["o", "k", "\n", "h", "m", "m ", "\n", "a", "b", "\n"]
data Unlines :: [Text] -> Exp Text
type instance Eval (Unlines txts) =
    Eval (Concat =<< F.FMap (Flip Append (Singleton @@ "\n")) txts)

-- | Unwords for type-level text. This uses 'Intercalate' to add space-symbol
-- between the given texts.
--
-- === __Example__
-- 
-- >>> :kind! Eval (Unwords '[ 'Text '["o", "k"], 'Text '["h", "m", "m "], 'Text '["a", "b"]])
-- Eval (Unwords '[ 'Text '["o", "k"], 'Text '["h", "m", "m "], 'Text '["a", "b"]]) :: Text
-- = 'Text '["o", "k", " ", "h", "m", "m ", " ", "a", "b"]
data Unwords :: [Text] -> Exp Text
type instance Eval (Unwords txts) = Eval (Intercalate ('Text '[" "]) txts)


-- | IsPrefixOf for type-level text.
--
-- === __Example__
-- 
-- >>> :kind! Eval (IsPrefixOf ('Text '["a", "a"]) ('Text '["a", "a", "m", "i", "a", "i", "n", "e", "n"]))
-- Eval (IsPrefixOf ('Text '["a", "a"]) ('Text '["a", "a", "m", "i", "a", "i", "n", "e", "n"])) :: Bool
-- = 'True
data IsPrefixOf :: Text -> Text -> Exp Bool
type instance Eval (IsPrefixOf ('Text l1) ('Text l2) ) = Eval (F.IsPrefixOf l1 l2)


-- | IsSuffixOf for type-level text.
--
-- === __Example__
-- 
-- >>> :kind! Eval (IsSuffixOf ('Text '["n", "e", "n"]) ('Text '["a", "a", "m", "i", "a", "i", "n", "e", "n"]))
-- Eval (IsSuffixOf ('Text '["n", "e", "n"]) ('Text '["a", "a", "m", "i", "a", "i", "n", "e", "n"])) :: Bool
-- = 'True
data IsSuffixOf :: Text -> Text -> Exp Bool
type instance Eval (IsSuffixOf ('Text l1) ('Text l2) ) = Eval (F.IsSuffixOf l1 l2)


-- | IsInfixOf for type-level text.
--
-- === __Example__
-- 
-- >>> :kind! Eval (IsInfixOf ('Text '["m", "i", "a"]) ('Text '["a", "a", "m", "i", "a", "i", "n", "e", "n"]))
-- Eval (IsInfixOf ('Text '["m", "i", "a"]) ('Text '["a", "a", "m", "i", "a", "i", "n", "e", "n"])) :: Bool
-- = 'True
data IsInfixOf :: Text -> Text -> Exp Bool
type instance Eval (IsInfixOf ('Text l1) ('Text l2) ) = Eval (F.IsInfixOf l1 l2) 

