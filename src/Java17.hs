{-# LANGUAGE OverloadedStrings #-}

{- |

Print algebraic data types as sealed interfaces with records as constructors.

Example: (only ground inductive types)
@
  data R = R  { f1 :: F1, ...,  fn :: Fn }

  data D = C1 { x1 :: D1, ...,  xn :: Dn }
         | C2 ...
         ...

  data E = E1 | ... | En
@
is printed as
@
  record R (F1 f1, ..., Fn fn) {}

  sealed interface D permits
     C1, C2, ... {}

  record C1 (D1 x1, ..., Dn xn) implements D {}
  record C2 ...                 implements D {}
  ...

  enum E { E1, ..., En };
@
-}

module Java17 ( printRecords ) where

import Prelude hiding ((<>))  -- requires GHC >= 7.6 to not be an error

import Syntax
import Pretty
import Printer
import String1 (String1)

printRecords :: String1 -> [Data] -> Class
printRecords x ds = Class x (any usesList ds) $ render $ vcat $ concat $ concat
  [ [ [ hsep [ "public", "interface", text x, "{" ] ]
    ]
  , map (map (nest ind) . ("" :) . printData) ds
  , [ [ ""
      , "}"
      ]
    ]
  ]

printData :: Data -> [Doc]
printData d@(Data x params cs _)
  -- Case enum: no parameters, no fields
  | null params, Just ys <- mapM (\ (Constructor x fs) -> if null fs then Just x else Nothing) cs =
      [ "enum" <+> text x <+> "{" <+> (hcat $ punctuate ", " $ map text ys) <+> "};" ]
  -- Case record: just one constructor, same name as data type
  | [c@(Constructor y _)] <- cs, x == y =
      [ constructorToRecord x params c ]
  -- Case data
  | otherwise =
      dataToInterface d
-- printData _ = error "printRecords: visitors not supported in Java 17 output"

constructorToRecord :: DataId -> [Param] -> Constructor -> Doc
constructorToRecord super params (Constructor x fs) =
  "record" <+> jApp (text x) (map text params)
  <> parens (printFields fs)
  <+> "implements" <+> text super <+> "{}"

dataToInterface :: Data -> [Doc]
dataToInterface (Data x params cs _) =
  hsep [ "sealed", "interface", jApp (text x) (map text params), "permits" ] :
  nest ind (hcat (punctuate ", " $ map (\ (Constructor y _) -> text y) cs) <+> "{}") :
  "" :
  map (constructorToRecord x params) cs
-- dataToInterface _ = error "dataToInterface: visitors not supported in Java 17 output"
