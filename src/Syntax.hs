{-# LANGUAGE DeriveFoldable #-}

{- |

Example: (only ground inductive types)
@
  data D = C1 { x1 :: D1, ...,  xn ::Dn }
         | C2 ...
         ...
@
is printed as
@
  public abstract class D {}

  public class C1 extends D {
    public D1 x1;
    ...
    public Dn xn;
    public C1 (D1 x1, ..., Dn xn) {
      this.x1 = x1;
      ...
      this xn = xn;
    }
  }
@
@
  data D = C1 { x1 :: D1, ...,  xn ::Dn }
         | C2 ...
         ...
  --visitor E1 V1
  --visitor E2 V2
@
is printed as
@
  class D {
      public E1 accept (V1 v) {
          return v.visit (this);
      }
      public E2 accept (V2 v) {
          return v.visit (this);
      }
  }

  class C1 extends D {
    public D1 x1;
    ...
    public Dn xn;
    public C1 (D1 x1, ..., Dn xn) {
      this.x1 = x1;
      ...
      this xn = xn;
    }
  }
@
the visitor interface is created as follows
@
  interface V1 {
      public E1 visit (C1 x);
      public E1 visit (C2 x);
      ...
  }
@
Example:
@
  data List a = Nil | Cons { head :: a, tail :: List a }
@
becomes
@
  abstract class List<a> {}

  class Nil<a> extends List<a> {
    Nil () {}
  }

  class Cons<a> extends List<a> {
    public a head;
    public List<a> tail;
    Cons (a head, List<a> tail) {
      this.head = head;
      this.tail = tail;
    }
  }
@

Grammar:
@
  datadecl :: 'data' uppername '=' constructors visitors

  constructor :: uppername [fields]

  fields :: '{' fieldlist '}'

  fieldlist :: fieldlist ',' field
             | field

  field :: lowername '::' type

  type :: type atom
        | atom

  atom :: name
        | '[' type ']'
        | '(' type ')

  visitor :: '--visitor' name name
@
-}

module Syntax where

import Data.Foldable
import Data.Monoid

type FieldId  = String
type ConstrId = String
type DataId   = String
type Param    = String
type ClassId  = String

data TypeId
  = TypeId String
  | Gen    String  -- ^ Type variable.
  deriving (Eq, Show)

data Visitor = Visitor
  { name       :: ClassId
  , returnType :: TypeId
  } deriving Show

data Type
  = List Type      -- ^ @List<Type>@
  | App Type Type  -- ^ @Type<Type>@
  | Name String    -- ^ @Type@
  deriving (Eq, Show)

data Field' a = Field
  { fieldId   :: FieldId
  , fieldType :: a
  } deriving (Show, Foldable)

data Constructor' a = Constructor
  { constrId     :: ConstrId
  , constrFields :: [Field' a]
  } deriving (Show, Foldable)

data Data' a = Data
  { dataId           :: DataId
  , dataParams       :: [Param]
  , dataConstructors :: [Constructor' a]
  , dataVisitors     :: [Visitor]
  } deriving (Show, Foldable)

type Field       = Field'       Type
type Constructor = Constructor' Type
type Data        = Data'        Type

usesList :: Foldable f => f Type -> Bool
usesList = getAny . foldMap (Any . isList)
  where
  isList :: Type -> Bool
  isList (List _) = True
  isList _        = False
