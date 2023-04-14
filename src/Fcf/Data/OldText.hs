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

= Fcf.Data.OldText

We mimick Data.Text but on type level. The internal representation is based on
type level lists.  This module is for GHC version 9.0.x or less.  The current
(as of early 2023) implementation of Fcf.Data.Text will be deprecated and replaced
with the contents of Fcf.Data.NewText later 2023 as newer version GHC become more
widespread.

-}

--------------------------------------------------------------------------------

module Fcf.Data.OldText
    {-# DEPRECATED "Use Fcf.Data.NewText instead" #-}
    ( Text (..)

    -- * Creation
    , Empty
    , Singleton
    , FromList
    , FromSymbolList
    , ToList
    , ToSymbol
    , ToSymbolList

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

import           GHC.TypeLits (Symbol)
import qualified GHC.TypeLits as TL

import           Fcf ( If, Eval, Exp, type (=<<), type (@@)
                     , Flip, Pure)
import qualified Fcf.Class.Foldable as F (All, Any)
import           Fcf.Data.List ( type (++) )
import qualified Fcf.Data.List as F
    ( Length, Init, Reverse, Take, Drop, TakeWhile, DropWhile
    , Intercalate, Intersperse, IsPrefixOf, IsSuffixOf, IsInfixOf, Snoc)

import qualified Fcf.Class.Functor as F ( FMap )


import           Fcf.Data.Nat (Nat)
import qualified Fcf.Data.Text.Internal as T
import           Fcf.Alg.Other ( PairMaybeToMaybePair )
import           Fcf.Alg.Morphism (First,Second)
import qualified Fcf.Alg.Symbol as S
import           Fcf.Alg.Nat (type (==))

--------------------------------------------------------------------------------


-- For the doctests:

-- $setup
-- >>> import           Fcf (type (<=<), Not)

--------------------------------------------------------------------------------

-- | 'Text' is a data structure, that is, a list to hold type-level symbols of
-- length one.
newtype Text = Text Symbol

--------------------------------------------------------------------------------

-- | Empty
--
-- === __Example__
--
-- >>> :kind! (Eval Empty :: Text)
-- (Eval Empty :: Text) :: Text
-- = 'Text ""
--
-- See also the other examples in this module.
data Empty :: Exp Text
type instance Eval Empty = 'Text ""

-- | Singleton
--
-- === __Example__
--
-- >>> :kind! Eval (Singleton "a")
-- Eval (Singleton "a") :: Text
-- = 'Text "a"
data Singleton :: Symbol -> Exp Text
type instance Eval (Singleton s) = 'Text s



-- | Use FromList to construct a Text from type-level list.
--
-- === __Example__
--
-- :kind! Eval (FromSymbolList '["h", "e", "l", "l", "u", "r", "e", "i"])
-- Eval (FromSymbolList '["h", "e", "l", "l", "u", "r", "e", "i"]) :: Text
-- = 'Text "hellurei"
data FromSymbolList :: [Symbol] -> Exp Text
type instance Eval (FromSymbolList sym) =  'Text (T.ToSymbol2 @@ sym)


-- hmm, this is also a Monoid, so we should be able to use Concat
data FromList :: [Text] -> Exp Text
type instance Eval (FromList txt) =
    'Text (T.ToSymbol2 @@ Eval (F.FMap ToSymbol txt))

-- | Get the type-level list out of the 'Text'.
--
-- === __Example__
--
-- >>> :kind! Eval (ToSymbolList =<< FromSymbolList '["a", "b"])
-- Eval (ToSymbolList =<< FromSymbolList '["a", "b"]) :: [Symbol]
-- = '["a", "b"]
data ToSymbolList :: Text -> Exp [Symbol]
type instance Eval (ToSymbolList ('Text sym)) = T.ToListA @@ sym


-- | Split 'Text' to single character 'Text' list.
--
-- === __Example__
--
-- >>> :kind! Eval (ToList =<< FromSymbolList '["a", "b"])
-- Eval (ToList =<< FromSymbolList '["a", "b"]) :: [Text]
-- = '[ 'Text "a", 'Text "b"]
data ToList :: Text -> Exp [Text]
type instance Eval (ToList txt) = Eval (F.FMap Singleton =<< ToSymbolList txt)


-- | ToSymbol
--
-- === __Example__
--
-- >>> :kind! Eval (ToSymbol =<< FromSymbolList '["w", "o", "r", "d"])
-- Eval (ToSymbol =<< FromSymbolList '["w", "o", "r", "d"]) :: Symbol
-- = "word"
data ToSymbol :: Text -> Exp Symbol
type instance Eval (ToSymbol ('Text sym)) = sym



-- | Null
--
-- === __Example__
--
-- >>> :kind! Eval (Null ('Text "ab"))
-- Eval (Null ('Text "ab")) :: Bool
-- = 'False
--
-- >>> :kind! Eval (Null =<< Empty)
-- Eval (Null =<< Empty) :: Bool
-- = 'True
data Null :: Text -> Exp Bool
type instance Eval (Null txt) = Eval
    (If (Eval (Eval (Length txt) == 0))
        (Pure 'True)
        (Pure 'False)
    )


-- | Length
--
-- === __Example__
--
-- >>> :kind! Eval (Length =<< Singleton "ab")
-- Eval (Length =<< Singleton "ab") :: TL.Natural
-- = 2
data Length :: Text -> Exp Nat
type instance Eval (Length ('Text sym)) = Eval (F.Length (T.ToList sym))


-- | Add a symbol to the beginning of a type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (Cons "h" ('Text "aamu"))
-- Eval (Cons "h" ('Text "aamu")) :: Text
-- = 'Text "haamu"
data Cons :: Symbol -> Text -> Exp Text
type instance Eval (Cons s ('Text sym)) = 'Text (TL.AppendSymbol s sym)

-- | Add a symbol to the end of a type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (Snoc ('Text "aam") "u")
-- Eval (Snoc ('Text "aam") "u") :: Text
-- = 'Text "aamu"
data Snoc :: Text -> Symbol -> Exp Text
type instance Eval (Snoc ('Text sym) s) = 'Text (TL.AppendSymbol sym s)

-- | Append two type-level texts.
--
-- === __Example__
--
-- >>> :kind! Eval (Append ('Text "aa") ('Text "mu"))
-- Eval (Append ('Text "aa") ('Text "mu")) :: Text
-- = 'Text "aamu"
data Append :: Text -> Text -> Exp Text
type instance Eval (Append ('Text s1) ('Text s2)) = 'Text (TL.AppendSymbol s1 s2)


-- | Get the first symbol from type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (Uncons ('Text "haamu"))
-- Eval (Uncons ('Text "haamu")) :: Maybe (Symbol, Text)
-- = 'Just '("h", 'Text "aamu")
--
-- >>> :kind! Eval (Uncons ('Text ""))
-- Eval (Uncons ('Text "")) :: Maybe (Symbol, Text)
-- = 'Nothing
data Uncons :: Text -> Exp (Maybe (TL.Symbol, Text))
type instance Eval (Uncons txt) =
    Eval (PairMaybeToMaybePair '( Eval (Head txt), Eval (Tail txt) ))




-- | Get the last symbol from type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (Unsnoc ('Text "aamun"))
-- Eval (Unsnoc ('Text "aamun")) :: Maybe (Symbol, Text)
-- = 'Just '("n", 'Text "aamu")
--
-- >>> :kind! Eval (Unsnoc ('Text ""))
-- Eval (Unsnoc ('Text "")) :: Maybe (Symbol, Text)
-- = 'Nothing
data Unsnoc :: Text -> Exp (Maybe (Symbol, Text))
type instance Eval (Unsnoc txt) = 
    Eval (F.FMap (Second Reverse) =<< Uncons =<< Reverse txt)


-- | Get the first symbol of type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (Head ('Text "aamu"))
-- Eval (Head ('Text "aamu")) :: Maybe Symbol
-- = 'Just "a"
--
-- >>> :kind! Eval (Head ('Text ""))
-- Eval (Head ('Text "")) :: Maybe Symbol
-- = 'Nothing
data Head :: Text -> Exp (Maybe Symbol)
type instance Eval (Head ('Text sym)) = Eval
    (If (Eval (Eval (Length ('Text sym)) == 0))
        (Pure 'Nothing)
        (Pure ('Just (T.Head1 sym (TL.CmpSymbol sym "\128"))))
    )

-- | Get the tail of a type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (Tail ('Text "haamu"))
-- Eval (Tail ('Text "haamu")) :: Maybe Text
-- = 'Just ('Text "aamu")
--
-- >>> :kind! Eval (Tail ('Text ""))
-- Eval (Tail ('Text "")) :: Maybe Text
-- = 'Nothing
data Tail :: Text -> Exp (Maybe Text)
type instance Eval (Tail ('Text sym)) = Eval (F.FMap Singleton =<< T.Uncons sym)

-- | Take all except the last symbol from type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (Init ('Text "aamun"))
-- Eval (Init ('Text "aamun")) :: Maybe Text
-- = 'Just ('Text "aamu")
--
-- >>> :kind! Eval (Init ('Text ""))
-- Eval (Init ('Text "")) :: Maybe Text
-- = 'Nothing
data Init :: Text -> Exp (Maybe Text)
type instance Eval (Init txt) = Eval (F.FMap FromList =<< F.Init =<< ToList txt)


-- | Compare the length of type-level text to given Nat and give
-- the Ordering.
--
-- === __Example__
--
-- >>> :kind! Eval (CompareLength ('Text "aamu") 3)
-- Eval (CompareLength ('Text "aamu") 3) :: Ordering
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
-- >>> :kind! Eval (FMap Isymb2e ('Text "imu"))
-- Eval (FMap Isymb2e ('Text "imu")) :: Text
-- = 'Text "emu"
data FMap :: (Symbol -> Exp Symbol) -> Text -> Exp Text
type instance Eval (FMap f txt) = Eval (FromSymbolList =<< F.FMap f =<< ToSymbolList txt)

data FMapT :: (Text -> Exp Text) -> Text -> Exp Text
type instance Eval (FMapT f txt) = Eval (FromList =<< F.FMap f =<< ToList txt)


-- | Intercalate for type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (Intercalate ('Text " & ") ('[ 'Text "aamu", 'Text "valo"]))
-- Eval (Intercalate ('Text " & ") ('[ 'Text "aamu", 'Text "valo"])) :: Text
-- = 'Text "aamu & valo"
data Intercalate :: Text -> [Text] -> Exp Text
type instance Eval (Intercalate txt txts) = 
    Eval (FromList =<< F.Intercalate '[txt] =<< F.FMap ToList txts)


-- | Intersperse for type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (Intersperse "." ('Text "aamu"))
-- Eval (Intersperse "." ('Text "aamu")) :: Text
-- = 'Text "a.a.m.u"
data Intersperse :: Symbol -> Text -> Exp Text
type instance Eval (Intersperse s ('Text txt)) =
    Eval (FromSymbolList =<< F.Intersperse s (T.ToList txt))

-- | Reverse for type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (Reverse ('Text "aamu"))
-- Eval (Reverse ('Text "aamu")) :: Text
-- = 'Text "umaa"
--
-- >>> :kind! Eval (Reverse =<< Reverse ('Text "aamu"))
-- Eval (Reverse =<< Reverse ('Text "aamu")) :: Text
-- = 'Text "aamu"
data Reverse :: Text -> Exp Text
type instance Eval (Reverse txt) =  Eval (FromList =<< F.Reverse =<< ToList txt)

-- | Replace for type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (Replace ('Text "tu") ('Text "la") ('Text "tuututtaa"))
-- Eval (Replace ('Text "tu") ('Text "la") ('Text "tuututtaa")) :: Text
-- = 'Text "laulattaa"
data Replace :: Text -> Text -> Text -> Exp Text
type instance Eval (Replace orig new txt) =
    Eval (Intercalate new =<< SplitOn orig txt)


-- | Concat for type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (Concat '[ 'Text "la", 'Text "kana"])
-- Eval (Concat '[ 'Text "la", 'Text "kana"]) :: Text
-- = 'Text "lakana"
data Concat :: [Text] -> Exp Text
type instance Eval (Concat lst) = 
    'Text (T.ToSymbol2 @@ Eval (F.FMap ToSymbol lst))



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
--         (Pure ('Text "aa"))
--         (Pure ('Text s))
--     )
-- :}
--
-- >>> :kind! Eval (FConcatMap Isymb2aa ('Text "imu ih"))
-- Eval (FConcatMap Isymb2aa ('Text "imu ih")) :: Text
-- = 'Text "aamu aah"
data FConcatMap :: (Symbol -> Exp Text) -> Text -> Exp Text
type instance Eval (FConcatMap f ('Text lst)) = Eval (Concat =<< F.FMap f (T.ToList lst))

-- | Any for type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (Any S.IsDigit ('Text "aamu1"))
-- Eval (Any S.IsDigit ('Text "aamu1")) :: Bool
-- = 'True
--
-- >>> :kind! Eval (Any S.IsDigit ('Text "aamu"))
-- Eval (Any S.IsDigit ('Text "aamu")) :: Bool
-- = 'False
data Any :: (Symbol -> Exp Bool) -> Text -> Exp Bool
type instance Eval (Any f ('Text sym)) = Eval (F.Any f (T.ToList sym))


-- | All for type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (All S.IsDigit ('Text "aamu1"))
-- Eval (All S.IsDigit ('Text "aamu1")) :: Bool
-- = 'False
--
-- >>> :kind! Eval (All S.IsDigit ('Text "321"))
-- Eval (All S.IsDigit ('Text "321")) :: Bool
-- = 'True
data All :: (Symbol -> Exp Bool) -> Text -> Exp Bool
type instance Eval (All f ('Text lst)) = Eval (F.All f (T.ToList lst))





-- | Take for type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (Take 4 ('Text "aamun"))
-- Eval (Take 4 ('Text "aamun")) :: Text
-- = 'Text "aamu"
data Take :: Nat -> Text -> Exp Text
type instance Eval (Take n ('Text lst)) = Eval (FromSymbolList =<< F.Take n (T.ToList lst))


-- | TakeEnd for type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (TakeEnd 4 ('Text "haamu"))
-- Eval (TakeEnd 4 ('Text "haamu")) :: Text
-- = 'Text "aamu"
data TakeEnd :: Nat -> Text -> Exp Text
type instance Eval (TakeEnd n ('Text lst)) =
    Eval (FromSymbolList =<< F.Reverse =<< F.Take n =<< F.Reverse (T.ToList lst))


-- | Drop for type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (Drop 2 ('Text "aamuna"))
-- Eval (Drop 2 ('Text "aamuna")) :: Text
-- = 'Text "muna"
data Drop :: Nat -> Text -> Exp Text
type instance Eval (Drop n ('Text lst)) = Eval (FromSymbolList =<< F.Drop n (T.ToList lst))

-- | DropEnd for type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (DropEnd 2 ('Text "aamuna"))
-- Eval (DropEnd 2 ('Text "aamuna")) :: Text
-- = 'Text "aamu"
data DropEnd :: Nat -> Text -> Exp Text
type instance Eval (DropEnd n ('Text lst)) =
    Eval (FromSymbolList =<< F.Reverse =<< F.Drop n =<< F.Reverse (T.ToList lst))


-- | TakeWhile for type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (TakeWhile (Not <=< S.IsDigit) ('Text "aamu12"))
-- Eval (TakeWhile (Not <=< S.IsDigit) ('Text "aamu12")) :: Text
-- = 'Text "aamu"
data TakeWhile :: (Symbol -> Exp Bool) -> Text -> Exp Text
type instance Eval (TakeWhile f ('Text lst)) = Eval (FromSymbolList =<< F.TakeWhile f (T.ToList lst))


-- | TakeWhileEnd for type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (TakeWhileEnd (Not <=< S.IsDigit) ('Text "12aamu"))
-- Eval (TakeWhileEnd (Not <=< S.IsDigit) ('Text "12aamu")) :: Text
-- = 'Text "aamu"
data TakeWhileEnd :: (Symbol -> Exp Bool) -> Text -> Exp Text
type instance Eval (TakeWhileEnd f ('Text lst)) =
    Eval (FromSymbolList =<< F.Reverse =<< F.TakeWhile f =<< F.Reverse (T.ToList lst))


-- | DropWhile for type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (DropWhile S.IsDigit ('Text "12aamu"))
-- Eval (DropWhile S.IsDigit ('Text "12aamu")) :: Text
-- = 'Text "aamu"
data DropWhile :: (Symbol -> Exp Bool) -> Text -> Exp Text
type instance Eval (DropWhile f ('Text lst)) = Eval (FromSymbolList =<< F.DropWhile f (T.ToList lst))


-- | DropWhileEnd for type-level text.
-- === __Example__
--
-- >>> :kind! Eval (DropWhileEnd S.IsDigit ('Text "aamu12"))
-- Eval (DropWhileEnd S.IsDigit ('Text "aamu12")) :: Text
-- = 'Text "aamu"
data DropWhileEnd :: (Symbol -> Exp Bool) -> Text -> Exp Text
type instance Eval (DropWhileEnd f ('Text lst)) =
    Eval (FromSymbolList =<< F.Reverse =<< F.DropWhile f =<< F.Reverse (T.ToList lst))


-- | DropAround for type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (DropAround S.IsDigit ('Text "34aamu12"))
-- Eval (DropAround S.IsDigit ('Text "34aamu12")) :: Text
-- = 'Text "aamu"
data DropAround :: (Symbol -> Exp Bool) -> Text -> Exp Text
type instance Eval (DropAround f txt) = Eval (DropWhile f =<< DropWhileEnd f txt)



-- | Strip the space, newline and tab -symbols from the beginning and and
-- of type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (Strip ('Text "  aamu \n"))
-- Eval (Strip ('Text "  aamu \n")) :: Text
-- = 'Text "aamu"
data Strip :: Text -> Exp Text
type instance Eval (Strip txt) = Eval (DropAround S.IsSpaceDelim txt)


-- | SplitOn for type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (SplitOn ('Text "ab") ('Text "cdabfgabh"))
-- Eval (SplitOn ('Text "ab") ('Text "cdabfgabh")) :: [Text]
-- = '[ 'Text "cd", 'Text "fg", 'Text "h"]
data SplitOn :: Text -> Text -> Exp [Text]
type instance Eval (SplitOn ('Text sep) ('Text txt)) =
    Eval (F.FMap FromSymbolList =<< SOLoop (T.ToList sep) '( '[], T.ToList txt))


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
type instance Eval (SOLoop sep '(acc, t ': txt)) =
    Eval (SOLoop sep =<< First (F.Snoc acc) =<< SOTake sep (t ': txt) '[])



-- | Split for type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (Split S.IsSpace (Eval (Singleton "cd bf abh")))
-- Eval (Split S.IsSpace (Eval (Singleton "cd bf abh"))) :: [Text]
-- = '[ 'Text "cd", 'Text "bf", 'Text "abh"]
data Split :: (Symbol -> Exp Bool) -> Text -> Exp [Text]
type instance Eval (Split p ('Text txt)) =
    Eval (F.FMap FromSymbolList =<< SplitLoop p '( '[], T.ToList txt))

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
type instance Eval (SplitLoop p '(acc, t ': txt)) = 
    Eval (SplitLoop p =<< First (F.Snoc acc) =<< SplitTake p (t ': txt) '[])



-- | Lines for type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (Lines =<< Singleton "ok\nhmm\nab")
-- Eval (Lines =<< Singleton "ok\nhmm\nab") :: [Text]
-- = '[ 'Text "ok", 'Text "hmm", 'Text "ab"]
data Lines :: Text -> Exp [Text]
type instance Eval (Lines txt) = Eval (Split S.IsNewLine txt)

-- | Words for type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (Words =<< Singleton "ok hmm\nab")
-- Eval (Words =<< Singleton "ok hmm\nab") :: [Text]
-- = '[ 'Text "ok", 'Text "hmm", 'Text "ab"]
data Words :: Text -> Exp [Text]
type instance Eval (Words txt) = Eval (Split S.IsSpaceDelim txt)

-- | Unlines for type-level text. This adds a newline to each Text and then
-- concats them.
--
-- === __Example__
--
-- >>> :kind! Eval (Unlines '[ 'Text "ok", 'Text "hmm", 'Text "ab"])
-- Eval (Unlines '[ 'Text "ok", 'Text "hmm", 'Text "ab"]) :: Text
-- = 'Text "ok\nhmm\nab\n"
data Unlines :: [Text] -> Exp Text
type instance Eval (Unlines txts) = 
    Eval (Concat =<< F.FMap (Flip Append (Singleton @@ "\n")) txts)

-- | Unwords for type-level text. This uses 'Intercalate' to add space-symbol
-- between the given texts.
--
-- === __Example__
--
-- >>> :kind! Eval (Unwords '[ 'Text "ok", 'Text "hmm", 'Text "ab"])
-- Eval (Unwords '[ 'Text "ok", 'Text "hmm", 'Text "ab"]) :: Text
-- = 'Text "ok hmm ab"
data Unwords :: [Text] -> Exp Text
type instance Eval (Unwords txts) = Eval (Intercalate ('Text " ") txts)


-- | IsPrefixOf for type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (IsPrefixOf ('Text "aa") ('Text "aamiainen"))
-- Eval (IsPrefixOf ('Text "aa") ('Text "aamiainen")) :: Bool
-- = 'True
data IsPrefixOf :: Text -> Text -> Exp Bool
type instance Eval (IsPrefixOf ('Text l1) ('Text l2) ) =
    Eval (F.IsPrefixOf (T.ToList l1) (T.ToList l2))


-- | IsSuffixOf for type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (IsSuffixOf ('Text "nen") ('Text "aamiainen"))
-- Eval (IsSuffixOf ('Text "nen") ('Text "aamiainen")) :: Bool
-- = 'True
data IsSuffixOf :: Text -> Text -> Exp Bool
type instance Eval (IsSuffixOf ('Text l1) ('Text l2) ) =
    Eval (F.IsSuffixOf (T.ToList l1) (T.ToList l2))


-- | IsInfixOf for type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (IsInfixOf ('Text "mia") ('Text "aamiainen"))
-- Eval (IsInfixOf ('Text "mia") ('Text "aamiainen")) :: Bool
-- = 'True
data IsInfixOf :: Text -> Text -> Exp Bool
type instance Eval (IsInfixOf ('Text l1) ('Text l2) ) =
    Eval (F.IsInfixOf (T.ToList l1) (T.ToList l2))
