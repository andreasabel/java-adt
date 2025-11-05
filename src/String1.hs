-- | Non-empty strings.

module String1
  ( String1
  , fromString, toString
  , module X
  ) where

import Data.List.NonEmpty
import Data.List.NonEmpty as X ( pattern (:|), head )

type List1 = NonEmpty
type String1 = List1 Char

-- | Partial function, raises error on empty string.
fromString :: String -> String1
fromString = fromList

toString :: String1 -> String
toString = toList
