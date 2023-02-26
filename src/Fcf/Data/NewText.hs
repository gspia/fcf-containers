{-# LANGUAGE CPP                    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

{-|
Module      : Fcf.Data.NewText
Description : Type-level Text data structure with methods
Copyright   : (c) gspia 2020-
License     : BSD
Maintainer  : gspia

= Fcf.Data.NewText

We mimick Data.Text but on type level. The current internal representation
of Fcf.Data.Text is based
on type level lists.  The current (as of early 2023) implementation of this
Fcf.Data.Text will be deprecated and replaced with the contents of Fcf.Data.NewText
later 2023 as newer version GHC become more widespread.

The old version working with 9.0.x or less will be kept at Fcf.Data.OldText for
some time.  Similarly, the module Fcf.Data.NewText contains the functions
and definitions for better Text type, which will be taken into use after some time.

The Fcf.Data.NewText will replace Fcf.Data.Text eventually.

-}

--------------------------------------------------------------------------------

module Fcf.Data.NewText
#if __GLASGOW_HASKELL__ >= 902
    ( Text (..)

    -- * Creation
    , Empty
    , Singleton
    , FromList
    , FromSymbolList
    , FromSymbol
    , ToList
    , ToSymbolList
    , ToCharList
    , Unpack

    -- * Basic Interface
    , Null
    , Length
    , Append
    , Cons
    , ConsSymbol
    , Snoc
    , SnocSymbol
    , Uncons
    , Unsnoc
    , Head
    , Last
    , Tail
    , Init
    , CompareLength

    -- * Transformation
    , FMap
    , FMapSymbol
    , FMapT
    , Intercalate
    , Intersperse
    , IntersperseSymbol
    , Reverse
    , Replace

    -- * Special Folds
    , Concat
    , ConcatMap
    , ConcatMapSymbol
    , ConcatMapCS
    , Any
    , AnySymbol
    , All
    , AllSymbol

    -- * Substrings
    , Take
    , TakeEnd
    , Drop
    , DropEnd
    , TakeWhile
    , TakeWhileSymbol
    , TakeWhileEnd
    , TakeWhileEndSymbol
    , DropWhile
    , DropWhileEnd
    , DropWhileEndSymbol
    , DropAround
    , DropAroundSymbol
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
#endif
  where

-- Version should be larger than 9.2.x. Note the two digit space for the
-- minor.
#if __GLASGOW_HASKELL__ >= 902

import           GHC.TypeLits (Symbol)
import qualified GHC.TypeLits as TL

import           Fcf ( If, Eval, Exp, type (=<<), type (@@)
                     , Flip, Pure, IsNothing, Fst, Snd)
import qualified Fcf.Class.Foldable as F (All, Any)
import           Fcf.Data.List ( type (++) )
import qualified Fcf.Data.List as F
    ( Length, Init, Reverse, Take, Drop, TakeWhile, DropWhile
    , Intercalate, Intersperse, IsPrefixOf, IsSuffixOf, IsInfixOf, Snoc)

import qualified Fcf.Class.Functor as F ( FMap )


import           Fcf.Data.Nat (Nat)
import qualified Fcf.Data.Symbol as S
import           Fcf.Data.Tuple (Swap)
import           Fcf.Alg.Other ( PairMaybeToMaybePair )
import           Fcf.Alg.Morphism (First,Second)
-- import           Fcf.Alg.Nat (type (==))
import qualified Fcf.Alg.Symbol as Sym

--------------------------------------------------------------------------------


-- For the doctests:

-- $setup
-- >>> import           Fcf (type (<=<), Not)
-- >>> import           Fcf.Data.Char as C

--------------------------------------------------------------------------------

-- | 'Text' is a data structure, that is, a list to hold type-level symbols of
-- length one.
data Text = Text Symbol

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
-- >>> :kind! Eval (Singleton 'a')
-- Eval (Singleton 'a') :: Text
-- = 'Text "a"
data Singleton :: Char -> Exp Text
type instance Eval (Singleton c) = Eval (FromSymbol =<< S.CharToSymbol c)


-- | FromSymbol
--
-- === __Example__
--
-- >>> :kind! Eval (FromSymbol "some text")
-- Eval (FromSymbol "some text") :: Text
-- = 'Text "some text"
data FromSymbol :: Symbol -> Exp Text
type instance Eval (FromSymbol s) = 'Text s


-- | Use FromList to construct a Text from type-level list.
--
-- === __Example__
--
-- :kind! Eval (FromSymbolList '["h", "e", "l", "l", "u", "r", "e", "i"])
-- Eval (FromSymbolList '["h", "e", "l", "l", "u", "r", "e", "i"]) :: Text
-- = 'Text "hellurei"
data FromSymbolList :: [Symbol] -> Exp Text
type instance Eval (FromSymbolList sym) = Eval (FromSymbol =<< S.Concat sym)


data FromList :: [Text] -> Exp Text
type instance Eval (FromList txt) =
    Eval (FromSymbol =<< S.Concat =<< F.FMap Unpack txt)
-- hmm, this is also a Monoid, so we should be able to use Concat
    

-- | Get the type-level list out of the 'Text'.
--
-- === __Example__
--
-- >>> :kind! Eval (ToSymbolList =<< FromSymbolList '["a", "b"])
-- Eval (ToSymbolList =<< FromSymbolList '["a", "b"]) :: [Symbol]
-- = '["a", "b"]
data ToSymbolList :: Text -> Exp [Symbol]
type instance Eval (ToSymbolList ('Text sym)) =
    Eval (F.FMap S.CharToSymbol =<< S.ToCharList sym)




-- | Split text to characters and give them as Char list.
--
-- > :kind! Eval (ToCharList =<< FromSymbol "abc")
-- Eval (ToCharList =<< FromSymbol "abc") :: [Char]
-- = '['a', 'b', 'c']
data ToCharList :: Text -> Exp [Char]
type instance Eval (ToCharList ('Text sym)) = S.ToCharList @@ sym


-- | Split text to characters and give them as Text list.
--
-- > :kind! Eval (ToList =<< FromSymbol "abc")
-- Eval (ToList =<< FromSymbol "abc") :: [Text]
-- = '[ 'Text "a", 'Text "b", 'Text "c"]
data ToList :: Text -> Exp [Text]
type instance Eval (ToList txt) =
    Eval (F.FMap Singleton =<< ToCharList txt)


-- | Unpack
--
-- === __Example__
--
-- >>> :kind! Eval (Unpack =<< FromSymbol "word")
-- Eval (Unpack =<< FromSymbol "word") :: Symbol
-- = "word"
data Unpack :: Text -> Exp Symbol
type instance Eval (Unpack ('Text sym)) = sym



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
    (If (Eval (IsNothing =<< NullHelper txt))
        (Pure 'True)
        (Pure 'False)
    )

-- > :kind! Eval (NullHelper =<< (Singleton "koe"))
-- Eval (NullHelper =<< (Singleton "koe")) :: Maybe
--                                                      (Char, Text)
-- = 'Just '('k', 'Text "oe")
data NullHelper :: Text -> Exp (Maybe (Char, Text))
type instance Eval (NullHelper ('Text symbol)) =
    Eval (F.FMap (Second FromSymbol) =<< S.UnconsSymbol symbol)


-- | Length
--
-- === __Example__
--
-- >>> :kind! Eval (Length =<< FromSymbol "ab")
-- Eval (Length =<< FromSymbol "ab") :: TL.Natural
-- = 2
data Length :: Text -> Exp Nat
type instance Eval (Length txt) = Eval (F.Length =<< ToCharList txt)


-- | Add a Char to the beginning of a type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (Cons 'h' ('Text "aamu"))
-- Eval (Cons 'h' ('Text "aamu")) :: Text
-- = 'Text "haamu"
data Cons :: Char -> Text -> Exp Text
type instance Eval (Cons c ('Text sym)) =
    'Text (TL.AppendSymbol (Eval (S.CharToSymbol c)) sym)


-- | Add a Symbol to the beginning of a type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (ConsSymbol "h" ('Text "aamu"))
-- Eval (ConsSymbol "h" ('Text "aamu")) :: Text
-- = 'Text "haamu"
data ConsSymbol :: Symbol -> Text -> Exp Text
type instance Eval (ConsSymbol s ('Text sym)) = 'Text (TL.AppendSymbol s sym)


-- | Add a Char to the end of a type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (Snoc ('Text "aam") 'u')
-- Eval (Snoc ('Text "aam") 'u') :: Text
-- = 'Text "aamu"
data Snoc :: Text -> Char -> Exp Text
type instance Eval (Snoc ('Text sym) c) =
    'Text (TL.AppendSymbol sym (Eval (S.CharToSymbol c)))

-- | Add a Symbol to the end of a type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (SnocSymbol ('Text "aam") "u")
-- Eval (SnocSymbol ('Text "aam") "u") :: Text
-- = 'Text "aamu"
data SnocSymbol :: Text -> Symbol -> Exp Text
type instance Eval (SnocSymbol ('Text sym) s) = 'Text (TL.AppendSymbol sym s)


-- | Append two type-level texts.
--
-- === __Example__
--
-- >>> :kind! Eval (Append ('Text "aa") ('Text "mu"))
-- Eval (Append ('Text "aa") ('Text "mu")) :: Text
-- = 'Text "aamu"
data Append :: Text -> Text -> Exp Text
type instance Eval (Append ('Text s1) ('Text s2)) = 'Text (TL.AppendSymbol s1 s2)


-- | Get the first Char from type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (Uncons ('Text "haamu"))
-- Eval (Uncons ('Text "haamu")) :: Maybe (Char, Text)
-- = 'Just '('h', 'Text "aamu")
--
-- >>> :kind! Eval (Uncons ('Text ""))
-- Eval (Uncons ('Text "")) :: Maybe (Char, Text)
-- = 'Nothing
data Uncons :: Text -> Exp (Maybe (Char, Text))
type instance Eval (Uncons txt) =
    Eval (PairMaybeToMaybePair '( Eval (Head txt), Eval (Tail txt) ))


-- | Get the last Char from type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (Unsnoc ('Text "aamun"))
-- Eval (Unsnoc ('Text "aamun")) :: Maybe (Text, Char)
-- = 'Just '( 'Text "aamu", 'n')
--
-- >>> :kind! Eval (Unsnoc ('Text ""))
-- Eval (Unsnoc ('Text "")) :: Maybe (Text, Char)
-- = 'Nothing
data Unsnoc :: Text -> Exp (Maybe (Text, Char))
type instance Eval (Unsnoc txt) = 
    Eval (F.FMap Swap =<< F.FMap (Second Reverse) =<< Uncons =<< Reverse txt)


-- | Get the first Char of type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (Head ('Text "aamu"))
-- Eval (Head ('Text "aamu")) :: Maybe Char
-- = 'Just 'a'
--
-- >>> :kind! Eval (Head ('Text ""))
-- Eval (Head ('Text "")) :: Maybe Char
-- = 'Nothing
data Head :: Text -> Exp (Maybe Char)
type instance Eval (Head ('Text sym)) =
    Eval (F.FMap Fst =<< S.UnconsSymbol sym)


-- | Get the last Char of type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (Last ('Text "aamu"))
-- Eval (Last ('Text "aamu")) :: Maybe Char
-- = 'Just 'u'
--
-- >>> :kind! Eval (Last ('Text ""))
-- Eval (Last ('Text "")) :: Maybe Char
-- = 'Nothing
data Last :: Text -> Exp (Maybe Char)
type instance Eval (Last txt) = Eval (F.FMap Snd =<< Unsnoc txt)


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
type instance Eval (Tail ('Text sym)) =
    Eval (F.FMap FromSymbol =<< F.FMap Snd =<< S.UnconsSymbol sym)


-- | Take all except the last Char from type-level text.
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
type instance Eval (Init ('Text sym)) = Eval
    (F.FMap FromSymbol =<< F.FMap S.ConcatChars =<< F.Init =<< S.ToCharList sym)


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



-- | FMapSymbol for type-level text.
--
-- === __Example__
--
-- >>> :{
-- data IsIsymb :: Symbol -> Exp Bool
-- type instance Eval (IsIsymb s) = Eval ("i" Sym.== s)
-- data Isymb2e :: Symbol -> Exp Symbol
-- type instance Eval (Isymb2e s) = Eval
--     (If (IsIsymb @@ s)
--         (Pure "e")
--         (Pure s)
--     )
-- :}
--
-- >>> :kind! Eval (FMapSymbol Isymb2e ('Text "imu"))
-- Eval (FMapSymbol Isymb2e ('Text "imu")) :: Text
-- = 'Text "emu"
data FMapSymbol :: (Symbol -> Exp Symbol) -> Text -> Exp Text
type instance Eval (FMapSymbol f txt) =
    Eval (FromSymbol =<< S.Concat =<< F.FMap f =<< F.FMap Unpack =<< ToList txt)


-- | FMap for type-level text.
--
-- === __Example__
--
-- >>> :{
-- data DigitsToX :: Char -> Exp Char
-- type instance Eval (DigitsToX c) = Eval
--     (If (IsDigit @@ c)
--         (Pure 'X')
--         (Pure c)
--     )
-- :}
--
-- >>> :kind! Eval (FMap DigitsToX ('Text "Some4text5oh9."))
-- Eval (FMap DigitsToX ('Text "Some4text5oh9.")) :: Text
-- = 'Text "SomeXtextXohX."
data FMap :: (Char -> Exp Char) -> Text -> Exp Text
type instance Eval (FMap f ('Text sym)) =
    Eval (FromSymbol =<< S.ConcatChars =<< F.FMap f =<< S.ToCharList sym)


data FMapT :: (Text -> Exp Text) -> Text -> Exp Text
type instance Eval (FMapT f txt) = Eval
    (FromList =<< F.FMap f =<< ToList txt)
-- is this too much


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


-- | Intersperse Char for type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (Intersperse '.' ('Text "aamu"))
-- Eval (Intersperse '.' ('Text "aamu")) :: Text
-- = 'Text "a.a.m.u"
data Intersperse :: Char  -> Text -> Exp Text
type instance Eval (Intersperse c ('Text sym)) = Eval
    (FromSymbolList =<< F.Intersperse (S.CharToSymbol @@c) =<< F.FMap S.CharToSymbol =<< S.ToCharList sym)

-- | Intersperse Symbol for type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (IntersperseSymbol "." ('Text "aamu"))
-- Eval (IntersperseSymbol "." ('Text "aamu")) :: Text
-- = 'Text "a.a.m.u"
data IntersperseSymbol :: Symbol -> Text -> Exp Text
type instance Eval (IntersperseSymbol s ('Text sym)) = Eval
    (FromSymbolList =<< F.Intersperse s =<< F.FMap S.CharToSymbol =<< S.ToCharList sym)


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
type instance Eval (Reverse ('Text sym)) =
    Eval (FromSymbol =<< S.ConcatChars =<< F.Reverse =<< S.ToCharList sym)

    
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
type instance Eval (Concat lst) = Eval (FromSymbol =<< S.Concat =<< F.FMap Unpack lst)


-- | FConcatMapSymbol for type-level text. This takes Symbol to Text function.
--
-- === __Example__
--
-- >>> :{
-- data IsIsymb :: Symbol -> Exp Bool
-- type instance Eval (IsIsymb s) = Eval ("i" Sym.== s)
-- data Isymb2aa :: Symbol -> Exp Text
-- type instance Eval (Isymb2aa s) = Eval
--     (If (IsIsymb @@ s)
--         (Pure ('Text "aa"))
--         (Pure ('Text s))
--     )
-- :}
--
-- >>> :kind! Eval (ConcatMapSymbol Isymb2aa ('Text "imu ih"))
-- Eval (ConcatMapSymbol Isymb2aa ('Text "imu ih")) :: Text
-- = 'Text "aamu aah"
data ConcatMapSymbol :: (Symbol -> Exp Text) -> Text -> Exp Text
type instance Eval (ConcatMapSymbol f ('Text sym)) =
    Eval (Concat =<< F.FMap f =<< F.FMap S.CharToSymbol =<< S.ToCharList sym)


-- | ConcatMap for type-level text.  This takes Char to Text function.
--
-- === __Example__
--
-- >>> :{
-- data DigitsToHoo :: Char -> Exp Text
-- type instance Eval (DigitsToHoo c) = Eval
--     (If (IsDigit @@ c)
--         (Pure ( 'Text "hoo"))
--         (Singleton c)
--     )
-- :}
--
-- >>> :kind! Eval (ConcatMap DigitsToHoo ('Text "haa2hui2"))
-- Eval (ConcatMap DigitsToHoo ('Text "haa2hui2")) :: Text
-- = 'Text "haahoohuihoo"
data ConcatMap :: (Char -> Exp Text) -> Text -> Exp Text
type instance Eval (ConcatMap f ('Text sym)) =
    Eval (Concat =<< F.FMap f =<< S.ToCharList sym)

-- | ConcatMap for type-level text.  This takes Char to Symbol function.
data ConcatMapCS :: (Char -> Exp Symbol) -> Text -> Exp Text
type instance Eval (ConcatMapCS f ('Text sym)) =
    Eval (FromSymbol =<< S.Concat =<< F.FMap f =<< S.ToCharList sym)


-- | Any for type-level text.  This takes Char to Bool function.
--
-- === __Example__
--
-- >>> :kind! Eval (Any IsDigit ('Text "aamu1"))
-- Eval (Any IsDigit ('Text "aamu1")) :: Bool
-- = 'True
--
-- >>> :kind! Eval (Any IsDigit ('Text "aamu"))
-- Eval (Any IsDigit ('Text "aamu")) :: Bool
-- = 'False
data Any :: (Char -> Exp Bool) -> Text -> Exp Bool
type instance Eval (Any f ('Text sym)) = Eval (F.Any f =<< S.ToCharList sym)


-- | AnySymbol for type-level text.  This takes Symbol to Bool function.
-- Note that the given function needs to be compatible... (i.e. operating
-- with symbols of length 1.
--
-- === __Example__
--
-- >>> :kind! Eval (AnySymbol Sym.IsDigit ('Text "aamu1"))
-- Eval (AnySymbol Sym.IsDigit ('Text "aamu1")) :: Bool
-- = 'True
--
-- >>> :kind! Eval (AnySymbol Sym.IsDigit ('Text "aamu"))
-- Eval (AnySymbol Sym.IsDigit ('Text "aamu")) :: Bool
-- = 'False
data AnySymbol :: (Symbol -> Exp Bool) -> Text -> Exp Bool
type instance Eval (AnySymbol f ('Text sym)) =
    Eval (F.Any f =<< F.FMap S.CharToSymbol =<< S.ToCharList sym)


-- | All for type-level text. This takes Char to Bool function.
--
-- === __Example__
--
-- >>> :kind! Eval (All IsDigit ('Text "aamu1"))
-- Eval (All IsDigit ('Text "aamu1")) :: Bool
-- = 'False
--
-- >>> :kind! Eval (All IsDigit ('Text "321"))
-- Eval (All IsDigit ('Text "321")) :: Bool
-- = 'True
data All :: (Char -> Exp Bool) -> Text -> Exp Bool
type instance Eval (All f ('Text sym)) = Eval (F.All f =<< S.ToCharList sym)


-- | AllSymbol for type-level text. This takes Symbol to Bool function.
--
-- === __Example__
--
-- >>> :kind! Eval (AllSymbol Sym.IsDigit ('Text "aamu1"))
-- Eval (AllSymbol Sym.IsDigit ('Text "aamu1")) :: Bool
-- = 'False
--
-- >>> :kind! Eval (AllSymbol Sym.IsDigit ('Text "321"))
-- Eval (AllSymbol Sym.IsDigit ('Text "321")) :: Bool
-- = 'True
data AllSymbol :: (Symbol -> Exp Bool) -> Text -> Exp Bool
type instance Eval (AllSymbol f ('Text sym)) =
    Eval (F.All f =<< F.FMap S.CharToSymbol =<< S.ToCharList sym)


-- | Take for type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (Take 4 ('Text "aamun"))
-- Eval (Take 4 ('Text "aamun")) :: Text
-- = 'Text "aamu"
data Take :: Nat -> Text -> Exp Text
type instance Eval (Take n txt) = Eval (FromList =<< F.Take n =<< ToList txt)

-- | TakeEnd for type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (TakeEnd 4 ('Text "haamu"))
-- Eval (TakeEnd 4 ('Text "haamu")) :: Text
-- = 'Text "aamu"
data TakeEnd :: Nat -> Text -> Exp Text
type instance Eval (TakeEnd n txt) =
    Eval (FromList =<< F.Reverse =<< F.Take n =<< F.Reverse =<< ToList txt)


-- | Drop for type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (Drop 2 ('Text "aamuna"))
-- Eval (Drop 2 ('Text "aamuna")) :: Text
-- = 'Text "muna"
data Drop :: Nat -> Text -> Exp Text
type instance Eval (Drop n txt) = Eval (FromList =<< F.Drop n =<< ToList txt)


-- | DropEnd for type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (DropEnd 2 ('Text "aamuna"))
-- Eval (DropEnd 2 ('Text "aamuna")) :: Text
-- = 'Text "aamu"
data DropEnd :: Nat -> Text -> Exp Text
type instance Eval (DropEnd n txt) =
    Eval (FromList =<< F.Reverse =<< F.Drop n =<< F.Reverse =<< ToList txt)

-- | TakeWhile for type-level text. This takes Char to Bool function.
--
-- === __Example__
--
-- >>> :kind! Eval (TakeWhile (Not <=< IsDigit) ('Text "aamu12"))
-- Eval (TakeWhile (Not <=< IsDigit) ('Text "aamu12")) :: Text
-- = 'Text "aamu"
data TakeWhile :: (Char -> Exp Bool) -> Text -> Exp Text
type instance Eval (TakeWhile f ('Text sym)) =
    Eval (FromSymbol =<< S.ConcatChars =<< F.TakeWhile f =<< S.ToCharList sym)


-- | TakeWhileSymbol for type-level text. This takes Symbol to Bool function.
--
-- === __Example__
--
-- >>> :kind! Eval (TakeWhileSymbol (Not <=< Sym.IsDigit) ('Text "aamu12"))
-- Eval (TakeWhileSymbol (Not <=< Sym.IsDigit) ('Text "aamu12")) :: Text
-- = 'Text "aamu"
data TakeWhileSymbol :: (Symbol -> Exp Bool) -> Text -> Exp Text
type instance Eval (TakeWhileSymbol f txt) = Eval
    (FromSymbolList =<< F.TakeWhile f =<< ToSymbolList txt)


-- | TakeWhileEnd for type-level text. This takes Char to Bool function.
--
-- === __Example__
--
-- >>> :kind! Eval (TakeWhileEnd (Not <=< IsDigit) ('Text "12aamu"))
-- Eval (TakeWhileEnd (Not <=< IsDigit) ('Text "12aamu")) :: Text
-- = 'Text "aamu"
data TakeWhileEnd :: (Char -> Exp Bool) -> Text -> Exp Text
type instance Eval (TakeWhileEnd f ('Text sym)) = Eval
    (FromSymbol
    =<< S.ConcatChars =<< F.Reverse =<< F.TakeWhile f =<< F.Reverse
    =<< S.ToCharList sym)


-- | TakeWhileEndSymbol for type-level text. This takes Symbol to Bool function.
--
-- === __Example__
--
-- >>> :kind! Eval (TakeWhileEndSymbol (Not <=< Sym.IsDigit) ('Text "12aamu"))
-- Eval (TakeWhileEndSymbol (Not <=< Sym.IsDigit) ('Text "12aamu")) :: Text
-- = 'Text "aamu"
data TakeWhileEndSymbol :: (Symbol -> Exp Bool) -> Text -> Exp Text
type instance Eval (TakeWhileEndSymbol f txt) = Eval
    (FromSymbolList =<< F.Reverse =<< F.TakeWhile f =<< F.Reverse =<< ToSymbolList txt)


-- | DropWhile for type-level text. This takes Char to Bool function.
--
-- === __Example__
--
-- >>> :kind! Eval (DropWhile IsDigit ('Text "12aamu"))
-- Eval (DropWhile IsDigit ('Text "12aamu")) :: Text
-- = 'Text "aamu"
data DropWhile :: (Char -> Exp Bool) -> Text -> Exp Text
type instance Eval (DropWhile f ('Text sym)) = Eval
    (FromSymbol =<< S.ConcatChars =<< F.DropWhile f =<< S.ToCharList sym)


-- | DropWhileSymbol for type-level text. This takes Symbol to Bool function.
--
-- === __Example__
--
-- >>> :kind! Eval (DropWhileSymbol Sym.IsDigit ('Text "12aamu"))
-- Eval (DropWhileSymbol Sym.IsDigit ('Text "12aamu")) :: Text
-- = 'Text "aamu"
data DropWhileSymbol :: (Symbol -> Exp Bool) -> Text -> Exp Text
type instance Eval (DropWhileSymbol f txt) = Eval
    (FromSymbolList =<< F.DropWhile f =<< ToSymbolList txt)


-- | DropWhileEnd for type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (DropWhileEnd IsDigit ('Text "aamu12"))
-- Eval (DropWhileEnd IsDigit ('Text "aamu12")) :: Text
-- = 'Text "aamu"
data DropWhileEnd :: (Char -> Exp Bool) -> Text -> Exp Text
type instance Eval (DropWhileEnd f ('Text sym)) = Eval
    (FromSymbol =<< S.ConcatChars  =<< F.Reverse =<< F.DropWhile f
    =<< F.Reverse =<< S.ToCharList sym)


-- | DropWhileEndSymbol for type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (DropWhileEndSymbol Sym.IsDigit ('Text "aamu12"))
-- Eval (DropWhileEndSymbol Sym.IsDigit ('Text "aamu12")) :: Text
-- = 'Text "aamu"
data DropWhileEndSymbol :: (Symbol -> Exp Bool) -> Text -> Exp Text
type instance Eval (DropWhileEndSymbol f txt) = Eval
    (FromSymbolList =<< F.Reverse =<< F.DropWhile f =<< F.Reverse =<< ToSymbolList txt)


-- | DropAround for type-level text. This takes Char to Bool function.
--
-- === __Example__
--
-- >>> :kind! Eval (DropAround IsDigit ('Text "34aamu12"))
-- Eval (DropAround IsDigit ('Text "34aamu12")) :: Text
-- = 'Text "aamu"
data DropAround :: (Char -> Exp Bool) -> Text -> Exp Text
type instance Eval (DropAround f txt) = Eval (DropWhile f =<< DropWhileEnd f txt)


-- | DropAroundSymbol for type-level text. This takes Symbol to Bool function.
--
-- === __Example__
--
-- >>> :kind! Eval (DropAroundSymbol Sym.IsDigit ('Text "34aamu12"))
-- Eval (DropAroundSymbol Sym.IsDigit ('Text "34aamu12")) :: Text
-- = 'Text "aamu"
data DropAroundSymbol :: (Symbol -> Exp Bool) -> Text -> Exp Text
type instance Eval (DropAroundSymbol f txt) =
    Eval (DropWhileSymbol f =<< DropWhileEndSymbol f txt)


-- | Strip the space, newline and tab -symbols from the beginning and and
-- of type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (Strip ('Text "  aamu \n"))
-- Eval (Strip ('Text "  aamu \n")) :: Text
-- = 'Text "aamu"
data Strip :: Text -> Exp Text
type instance Eval (Strip txt) = Eval (DropAroundSymbol Sym.IsSpaceDelim txt)


-- | SplitOn for type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (SplitOn ('Text "ab") ('Text "cdabfgabh"))
-- Eval (SplitOn ('Text "ab") ('Text "cdabfgabh")) :: [Text]
-- = '[ 'Text "cd", 'Text "fg", 'Text "h"]
data SplitOn :: Text -> Text -> Exp [Text]
type instance Eval (SplitOn ('Text sep) ('Text txt)) = Eval
    -- (F.FMap FromSymbolList
    (F.FMap FromSymbol =<< F.FMap S.ConcatChars
    =<< SOLoop (Eval (S.ToCharList sep)) '( '[], Eval (S.ToCharList txt)))


-- | Helper for SplitOn
--
-- >>> :kind! Eval (SOTake '[ 'a', 'b'] '[ 'c', 'd', 'a', 'b', 'f', 'g'] '[])
-- Eval (SOTake '[ 'a', 'b'] '[ 'c', 'd', 'a', 'b', 'f', 'g'] '[]) :: ([Char],
--                                                                     [Char])
-- = '( '['c', 'd'], '['f', 'g'])
data SOTake :: [Char] -> [Char] -> [Char] -> Exp ([Char], [Char])
type instance Eval (SOTake sep '[] accum) = '(accum, '[])
type instance Eval (SOTake sep (t ': txt) accum) = Eval
    (If (Eval (F.IsPrefixOf sep (t ': txt)))
        (Pure '(accum, Eval (F.Drop (Eval (F.Length sep)) (t ': txt))))
        (SOTake sep txt (Eval (accum ++ '[t])))
    )

-- | Helper for SplitOn
data SOLoop :: [Char] -> ([[Char]],[Char]) -> Exp [[Char]]
type instance Eval (SOLoop sep '(acc, '[])) = acc
type instance Eval (SOLoop sep '(acc, (t ': txt))) =
    Eval (SOLoop sep =<< First (F.Snoc acc) =<< SOTake sep (t ': txt) '[])


-- | Split for type-level text. This takes Char to Bool function.
--
-- === __Example__
--
-- >>> :kind! Eval (Split C.IsSpace (Eval (FromSymbol "cd bf abh")))
-- Eval (Split C.IsSpace (Eval (FromSymbol "cd bf abh"))) :: [Text]
-- = '[ 'Text "cd", 'Text "bf", 'Text "abh"]
data Split :: (Char -> Exp Bool) -> Text -> Exp [Text]
type instance Eval (Split p ('Text sym)) = Eval
    (F.FMap FromSymbol =<< F.FMap S.ConcatChars
    =<< SplitLoop p '( '[], Eval (S.ToCharList sym)))


-- | SplitSymbol for type-level text. This takes Symbol to Bool function.
--
-- === __Example__
--
-- >>> :kind! Eval (SplitSymbol Sym.IsSpace (Eval (FromSymbol "cd bf abh")))
-- Eval (SplitSymbol Sym.IsSpace (Eval (FromSymbol "cd bf abh"))) :: [Text]
-- = '[ 'Text "cd", 'Text "bf", 'Text "abh"]
data SplitSymbol :: (Symbol -> Exp Bool) -> Text -> Exp [Text]
type instance Eval (SplitSymbol p txt) =
    Eval (F.FMap FromSymbolList =<< SplitLoop p '( '[], Eval (ToSymbolList txt)))

-- | Helper for Split
data SplitTake :: (a -> Exp Bool) -> [a] -> [a] -> Exp ([a], [a])
type instance Eval (SplitTake p '[] accum) = '(accum, '[])
type instance Eval (SplitTake p (t ': txt) accum) = Eval
    (If (Eval (p t))
        (Pure '(accum, txt))
        (SplitTake p txt (Eval (accum ++ '[t])))
    )

-- | Helper for Split
data SplitLoop :: (a -> Exp Bool) -> ([[a]],[a]) -> Exp [[a]]
type instance Eval (SplitLoop p '(acc, '[])) = acc
type instance Eval (SplitLoop p '(acc, (t ': txt))) = 
    Eval (SplitLoop p =<< First (F.Snoc acc) =<< SplitTake p (t ': txt) '[])


-- | Lines for type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (Lines =<< FromSymbol "ok\nhmm\nab")
-- Eval (Lines =<< FromSymbol "ok\nhmm\nab") :: [Text]
-- = '[ 'Text "ok", 'Text "hmm", 'Text "ab"]
data Lines :: Text -> Exp [Text]
type instance Eval (Lines txt) = Eval (SplitSymbol Sym.IsNewLine txt)


-- | Words for type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (Words =<< FromSymbol "ok hmm\nab")
-- Eval (Words =<< FromSymbol "ok hmm\nab") :: [Text]
-- = '[ 'Text "ok", 'Text "hmm", 'Text "ab"]
data Words :: Text -> Exp [Text]
type instance Eval (Words txt) = Eval (SplitSymbol Sym.IsSpaceDelim txt)

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
    Eval (Concat =<< F.FMap (Flip Append (FromSymbol @@ "\n")) txts)

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
type instance Eval (IsPrefixOf l1 l2) =
    Eval (F.IsPrefixOf (Eval (ToList l1)) (Eval (ToList l2)))


-- | IsSuffixOf for type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (IsSuffixOf ('Text "nen") ('Text "aamiainen"))
-- Eval (IsSuffixOf ('Text "nen") ('Text "aamiainen")) :: Bool
-- = 'True
data IsSuffixOf :: Text -> Text -> Exp Bool
type instance Eval (IsSuffixOf l1 l2) =
    Eval (F.IsSuffixOf (Eval (ToList l1)) (Eval (ToList l2)))


-- | IsInfixOf for type-level text.
--
-- === __Example__
--
-- >>> :kind! Eval (IsInfixOf ('Text "mia") ('Text "aamiainen"))
-- Eval (IsInfixOf ('Text "mia") ('Text "aamiainen")) :: Bool
-- = 'True
data IsInfixOf :: Text -> Text -> Exp Bool
type instance Eval (IsInfixOf l1 l2) =
    Eval (F.IsInfixOf (Eval (ToList l1)) (Eval (ToList l2)))


--------------------------------------------------------------------------------

#else

--------------------------------------------------------------------------------

#endif

--------------------------------------------------------------------------------
