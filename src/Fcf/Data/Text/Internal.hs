{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

{-|
Module      : Fcf.Data.Text.Internal
Description : Type-level Text data structure with methods
Copyright   : (c) gspia 2020-
License     : BSD
Maintainer  : gspia

= Fcf.Data.Text.Internal

This is from https://kcsongor.github.io/symbol-parsing-haskell/

Please do also check the symbols library at Hackage.

-}

--------------------------------------------------------------------------------

module Fcf.Data.Text.Internal where

import Data.Char (chr)

import qualified GHC.TypeLits as TL
import qualified Fcf.Data.List as L
import           Fcf.Data.Bitree
import qualified Fcf.Alg.Symbol as S
import Fcf

--------------------------------------------------------------------------------

type LookupTable = Tree (TL.Symbol, TL.Symbol)


-- :kind! Head2 "hello"
type family Head2 (sym :: TL.Symbol) :: TL.Symbol where
    Head2 ""  = ""
    Head2 sym = Head1 sym (TL.CmpSymbol sym "\128")

-- :kind! ToList "hello"
type family ToList (sym :: TL.Symbol) :: [TL.Symbol] where
    ToList sym = ToList1 sym ""

-- Helper.
data ToSymbol2 :: [TL.Symbol] -> Exp TL.Symbol
type instance Eval (ToSymbol2 lst) = Eval (Foldr S.Append "" lst)

-- :kind! Eval (HeadA "koe")
data HeadA :: TL.Symbol -> Exp TL.Symbol
type instance Eval (HeadA sym) = Head1 sym (TL.CmpSymbol sym "\128")

-- :kind! Eval (ToListA "hello")
-- :kind! Eval (ToListA "")
data ToListA :: TL.Symbol -> Exp [TL.Symbol]
type instance Eval (ToListA sym) = ToList1 sym ""

-- :kind! Eval (Uncons "hello")
-- :kind! Eval (Uncons "")
data Uncons :: TL.Symbol -> Exp (Maybe TL.Symbol)
type instance Eval (Uncons sym) = Eval (Map ToSymbol2 =<< L.Tail =<< ToListA sym)


-------------------------------------------------------------------------------

-- | Helper, from symbols-package.
type family Head1 (x :: TL.Symbol) (o :: Ordering) :: TL.Symbol where
  Head1 x 'GT = TL.TypeError ('TL.Text "Starts with non-ASCII character " 'TL.:<>: 'TL.ShowType x)
  Head1 x _   = LookupA x "" Chars

-- | Helper, from symbols-package.
type family ToList1 (x :: TL.Symbol) (pfx :: TL.Symbol) :: [TL.Symbol] where
  ToList1 x x   = '[]
  ToList1 x pfx = ToList2 x pfx (TL.CmpSymbol x (TL.AppendSymbol pfx "\128"))

-- | Helper, from symbols-package.
type family ToList2 (x :: TL.Symbol) (pfx :: TL.Symbol) (o :: Ordering) :: [TL.Symbol] where
  ToList2 x pfx 'LT = LookupA x pfx Chars ': ToList1 x (TL.AppendSymbol pfx (LookupA x pfx Chars))
  ToList2 x _   _   = TL.TypeError ('TL.Text "Non-AScII character in " 'TL.:<>: 'TL.ShowType x)

-- | Helper, from symbols-package.
type family LookupA (x :: TL.Symbol) (pfx :: TL.Symbol) (xs :: Tree TL.Symbol) :: TL.Symbol where
  LookupA "" _   _             = ""
  LookupA _  _   ('Leaf x)     = x
  LookupA x  ""  ('Node l c r) = Lookup2 x ""  c (TL.CmpSymbol x c)                    l r
  LookupA x  pfx ('Node l c r) = Lookup2 x pfx c (TL.CmpSymbol x (TL.AppendSymbol pfx c)) l r

-- | Helper, from symbols-package.
type family Lookup2 (x :: TL.Symbol) (pfx :: TL.Symbol) (c :: TL.Symbol) (o :: Ordering) (l :: Tree TL.Symbol) (r :: Tree TL.Symbol) :: TL.Symbol where
  Lookup2 _ _   c 'EQ _ _ = c
  Lookup2 x pfx c 'LT l _ = LookupA x pfx l
  Lookup2 x pfx _ 'GT _ r = LookupA x pfx r


-- | Helper, from symbols-package. (Generate the character tree.)
chars :: Tree String
chars = buildTree [ chr c | c <- [0..0x7f] ] where
  buildTree []    = error "panic! buildTree []"
  buildTree [c]   = Leaf [c]
  buildTree pairs = Node (buildTree l) c (buildTree r) where
    n      = length pairs
    (l, r) = splitAt (n `div` 2) pairs
    c      = case r of
      []     -> error "panic! buildTree: r is empty"
      (c':_) -> [c']


-- | Helper, from symbols-package. The character tree that is needed for
-- handling the initial character of a symbol.
type Chars = 'Node
  ('Node
     ('Node
        ('Node
           ('Node
              ('Node
                 ('Node ('Leaf "\NUL") "\SOH" ('Leaf "\SOH"))
                 "\STX"
                 ('Node ('Leaf "\STX") "\ETX" ('Leaf "\ETX")))
              "\EOT"
              ('Node
                 ('Node ('Leaf "\EOT") "\ENQ" ('Leaf "\ENQ"))
                 "\ACK"
                 ('Node ('Leaf "\ACK") "\a" ('Leaf "\a"))))
           "\b"
           ('Node
              ('Node
                 ('Node ('Leaf "\b") "\t" ('Leaf "\t"))
                 "\n"
                 ('Node ('Leaf "\n") "\v" ('Leaf "\v")))
              "\f"
              ('Node
                 ('Node ('Leaf "\f") "\r" ('Leaf "\r"))
                 "\SO"
                 ('Node ('Leaf "\SO") "\SI" ('Leaf "\SI")))))
        "\DLE"
        ('Node
           ('Node
              ('Node
                 ('Node ('Leaf "\DLE") "\DC1" ('Leaf "\DC1"))
                 "\DC2"
                 ('Node ('Leaf "\DC2") "\DC3" ('Leaf "\DC3")))
              "\DC4"
              ('Node
                 ('Node ('Leaf "\DC4") "\NAK" ('Leaf "\NAK"))
                 "\SYN"
                 ('Node ('Leaf "\SYN") "\ETB" ('Leaf "\ETB"))))
           "\CAN"
           ('Node
              ('Node
                 ('Node ('Leaf "\CAN") "\EM" ('Leaf "\EM"))
                 "\SUB"
                 ('Node ('Leaf "\SUB") "\ESC" ('Leaf "\ESC")))
              "\FS"
              ('Node
                 ('Node ('Leaf "\FS") "\GS" ('Leaf "\GS"))
                 "\RS"
                 ('Node ('Leaf "\RS") "\US" ('Leaf "\US"))))))
     " "
     ('Node
        ('Node
           ('Node
              ('Node
                 ('Node ('Leaf " ") "!" ('Leaf "!"))
                 "\""
                 ('Node ('Leaf "\"") "#" ('Leaf "#")))
              "$"
              ('Node
                 ('Node ('Leaf "$") "%" ('Leaf "%"))
                 "&"
                 ('Node ('Leaf "&") "'" ('Leaf "'"))))
           "("
           ('Node
              ('Node
                 ('Node ('Leaf "(") ")" ('Leaf ")"))
                 "*"
                 ('Node ('Leaf "*") "+" ('Leaf "+")))
              ","
              ('Node
                 ('Node ('Leaf ",") "-" ('Leaf "-"))
                 "."
                 ('Node ('Leaf ".") "/" ('Leaf "/")))))
        "0"
        ('Node
           ('Node
              ('Node
                 ('Node ('Leaf "0") "1" ('Leaf "1"))
                 "2"
                 ('Node ('Leaf "2") "3" ('Leaf "3")))
              "4"
              ('Node
                 ('Node ('Leaf "4") "5" ('Leaf "5"))
                 "6"
                 ('Node ('Leaf "6") "7" ('Leaf "7"))))
           "8"
           ('Node
              ('Node
                 ('Node ('Leaf "8") "9" ('Leaf "9"))
                 ":"
                 ('Node ('Leaf ":") ";" ('Leaf ";")))
              "<"
              ('Node
                 ('Node ('Leaf "<") "=" ('Leaf "="))
                 ">"
                 ('Node ('Leaf ">") "?" ('Leaf "?")))))))
  "@"
  ('Node
     ('Node
        ('Node
           ('Node
              ('Node
                 ('Node ('Leaf "@") "A" ('Leaf "A"))
                 "B"
                 ('Node ('Leaf "B") "C" ('Leaf "C")))
              "D"
              ('Node
                 ('Node ('Leaf "D") "E" ('Leaf "E"))
                 "F"
                 ('Node ('Leaf "F") "G" ('Leaf "G"))))
           "H"
           ('Node
              ('Node
                 ('Node ('Leaf "H") "I" ('Leaf "I"))
                 "J"
                 ('Node ('Leaf "J") "K" ('Leaf "K")))
              "L"
              ('Node
                 ('Node ('Leaf "L") "M" ('Leaf "M"))
                 "N"
                 ('Node ('Leaf "N") "O" ('Leaf "O")))))
        "P"
        ('Node
           ('Node
              ('Node
                 ('Node ('Leaf "P") "Q" ('Leaf "Q"))
                 "R"
                 ('Node ('Leaf "R") "S" ('Leaf "S")))
              "T"
              ('Node
                 ('Node ('Leaf "T") "U" ('Leaf "U"))
                 "V"
                 ('Node ('Leaf "V") "W" ('Leaf "W"))))
           "X"
           ('Node
              ('Node
                 ('Node ('Leaf "X") "Y" ('Leaf "Y"))
                 "Z"
                 ('Node ('Leaf "Z") "[" ('Leaf "[")))
              "\\"
              ('Node
                 ('Node ('Leaf "\\") "]" ('Leaf "]"))
                 "^"
                 ('Node ('Leaf "^") "_" ('Leaf "_"))))))
     "`"
     ('Node
        ('Node
           ('Node
              ('Node
                 ('Node ('Leaf "`") "a" ('Leaf "a"))
                 "b"
                 ('Node ('Leaf "b") "c" ('Leaf "c")))
              "d"
              ('Node
                 ('Node ('Leaf "d") "e" ('Leaf "e"))
                 "f"
                 ('Node ('Leaf "f") "g" ('Leaf "g"))))
           "h"
           ('Node
              ('Node
                 ('Node ('Leaf "h") "i" ('Leaf "i"))
                 "j"
                 ('Node ('Leaf "j") "k" ('Leaf "k")))
              "l"
              ('Node
                 ('Node ('Leaf "l") "m" ('Leaf "m"))
                 "n"
                 ('Node ('Leaf "n") "o" ('Leaf "o")))))
        "p"
        ('Node
           ('Node
              ('Node
                 ('Node ('Leaf "p") "q" ('Leaf "q"))
                 "r"
                 ('Node ('Leaf "r") "s" ('Leaf "s")))
              "t"
              ('Node
                 ('Node ('Leaf "t") "u" ('Leaf "u"))
                 "v"
                 ('Node ('Leaf "v") "w" ('Leaf "w"))))
           "x"
           ('Node
              ('Node
                 ('Node ('Leaf "x") "y" ('Leaf "y"))
                 "z"
                 ('Node ('Leaf "z") "{" ('Leaf "{")))
              "|"
              ('Node
                 ('Node ('Leaf "|") "}" ('Leaf "}"))
                 "~"
                 ('Node ('Leaf "~") "\DEL" ('Leaf "\DEL")))))))


