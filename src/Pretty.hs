module Pretty
  ( module Pretty
  , module X
  ) where

import Text.PrettyPrint as X
    ( Doc, ($+$), (<+>), (<>)
    , braces, cat, char, comma, equals, hcat, hsep, lbrace, nest, parens, punctuate, rbrace, render, semi, space, vcat )
import qualified Text.PrettyPrint

import String1 (String1)
import qualified String1

-- | Overload 'text' to also work with 'String1'.
class PrettyString a where
  text :: a -> Doc

instance PrettyString String where
  text = Text.PrettyPrint.text

instance PrettyString String1 where
  text = text . String1.toString
