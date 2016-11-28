module Printer where

import Text.PrettyPrint
import Data.Char (toLower)

import Syntax
import Options

type ClassDef = String

data Class = Class
  { classId       :: ClassId
  , classUsesList :: Bool
  , classDef      :: ClassDef
  }

-- | Indentation level
ind = 4

-- | Entry point 1: construct visitors for data type.

dataToVisitors :: Options -> Data -> [Class]
dataToVisitors opt d@(Data id params cs vs) =
  map (constrToVisitor opt d) vs

constrToVisitor :: Options -> Data -> Visitor -> Class
constrToVisitor opt d@(Data id params cs _) (Visitor name rt) =
  Class id (usesList d) $ render $
    condsep (pubClasses opt) public $
    interface <+> jApp (text name) (genReturnType rt ++ map text params) <+> lbrace
    $+$
    (nest ind $ vcat $
       map (\ (Constructor c _) ->
            public <+> flatReturnType rt <+> visit <+>
            parens (jApp (text c) (map text params) <+> var) <> semi)
       cs)
    $+$ rbrace
  where var = char $ toLower (head id)

-- | Entry point 2: construct abstract parent classes and subclasses for data type.

dataToClasses :: Options -> Data -> [Class]
dataToClasses opt d@(Data id params cs vs) = cl : map (constrToClass opt id params vs) cs
  where
  cl = Class id (usesList d) . render $
    publicOpt $ abstract <+>
        text "class" <+> jApp (text id) (map text params) <+> lbrace
    $+$ (nest ind $ vcat $
         map (\ (Visitor name rt) ->
              public <+> abstract <+> quantReturnType rt <+>
              accept <+> parens (jApp (text name) (genReturnType rt ++ map text params) <+> v) <> semi)
           vs)
    $+$ rbrace
  publicOpt doc | pubClasses opt = public <+> doc
                | otherwise      = doc

constrToClass :: Options -> DataId -> [Param] -> [Visitor] -> Constructor -> Class
constrToClass opt super params vs c@(Constructor id fs) =
  Class id (usesList c) . render $
  (publicOpt $ text "class" <+> jApp (text id) (map text params) <+>
    extends <+> jApp (text super) (map text params) <+> lbrace)
  $+$ (nest ind $
       (vcat (map (\ (Field f t) -> public <+> printType t <+> text f <> semi) fs))
       $+$
       (public <+> text id <+>
        parens (cat (punctuate (comma<>space) (map (\ (Field f t) -> printType t <+> text f) fs))) <+>
        lbrace
        $+$ (nest ind (vcat (map (\ (Field f t) -> this <> dot <> text f <+> equals <+> text f <> semi) fs)))
        $+$ rbrace)
        $+$
         (vcat $ map (\ (Visitor name rt) ->
                    public <+> quantReturnType rt <+> accept <+>
                    parens (jApp (text name) (genReturnType rt ++ map text params) <+> v) <+> lbrace
                    $+$
                    (nest ind $ condsep (rt /= TypeId "void") (text "return")
                      $ v <> dot <> visit <+> (parens this) <> semi)
                    $+$ rbrace) vs)
       )
  $+$ rbrace
  where
  publicOpt doc | pubClasses opt = public <+> doc
                | otherwise      = doc

-- | Java type application @T<A,B>@.
jApp :: Doc -> [Doc] -> Doc
jApp d1 [] = d1
jApp d1 ds = d1 <> text "<" <> cat (punctuate comma ds) <> text ">"

printType :: Type -> Doc
printType t = printType' t []

printType' :: Type -> [Type] -> Doc
printType' (List t)  ts = jApp (text "List") $ map printType $ t:ts
printType' (App r s) ts = printType' r $ s:ts
printType' (Name x)  ts = jApp (text x) $ map printType $ ts

flatReturnType :: TypeId -> Doc
flatReturnType (TypeId s) = text s
flatReturnType (Gen s)    = text s

quantReturnType :: TypeId -> Doc
quantReturnType (TypeId s) = text s
quantReturnType (Gen s) = text "<" <> text s <> text ">" <+> text s

genReturnType :: TypeId -> [Doc]
genReturnType (Gen s) = [text s]
genReturnType (TypeId s) = []

condsep :: Bool -> Doc -> Doc -> Doc
condsep True d1 d2 = d1 <+> d2
condsep False _ d2 = d2

dot       = text "."
public    = text "public"
interface = text "interface"
abstract  = text "abstract"
extends   = text "extends"
this      = text "this"
accept    = text "accept"
v         = text "v"
visit     = text "visit"
