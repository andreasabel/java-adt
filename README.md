java-adt
========

A tool to create immutable algebraic data structures and visitors for Java
(such as abstract syntax trees).  The input syntax is similar to Haskell data types,
and they will be compiled to Java class hierarchies.

Installation
------------

With a running [Haskell installation](https://www.haskell.org/ghcup/), simply type into your shell
```
cabal install java-adt
```
and make sure your `.cabal/bin/` (or similar) is part of your system PATH.

Example 1: Immutable linked lists with default visitor
------------------------------------------------------

Input: List.hs
```haskell
data List A = Nil | Cons { head :: A, tail :: List A }
```
Invocation `java-adt List.hs` prints to standard output:
```java
abstract class List<A> {
}

class Nil<A> extends List<A> {
    public Nil () {
    }
}

class Cons<A> extends List<A> {
    public A head;
    public List<A> tail;
    public Cons (A head, List<A> tail) {
        this.head = head;
        this.tail = tail;
    }
}
```
Invocation: `java-adt -o List.java List.hs` leaves output in `List.java`.

Invocation: `java-adt -d List.hs` outputs same but with default visitor on standard output:
```java
abstract class List<A> {
    public abstract <R> R accept (ListVisitor<R,A> v);
}

class Nil<A> extends List<A> {
    public Nil () {
    }
    public <R> R accept (ListVisitor<R,A> v) {
        return v.visit (this);
    }
}

class Cons<A> extends List<A> {
    public A head;
    public List<A> tail;
    public Cons (A head, List<A> tail) {
        this.head = head;
        this.tail = tail;
    }
    public <R> R accept (ListVisitor<R,A> v) {
        return v.visit (this);
    }
}

interface ListVisitor<R,A> {
    public R visit (Nil<A> l);
    public R visit (Cons<A> l);
}
```

Example 2: A simple AST with custom visitor
-------------------------------------------

Input file `Exp.hs`:  (Note the use of Haskell lists in `[Exp]`)
```haskell
data Exp
  = EInt  { i :: Integer }
  | EAdd  { e1 :: Exp, e2 :: Exp }
  | ECall { f :: String, es :: [Exp] }
--visitor Integer EvalVisitor
```
Invocation `java-ast -o Exp.java Exp.hs` outputs into `Exp.java`:
```java
import java.util.List;

abstract class Exp {
    public abstract Integer accept (EvalVisitor v);
}

class EInt extends Exp {
    public Integer i;
    public EInt (Integer i) {
        this.i = i;
    }
    public Integer accept (EvalVisitor v) {
        return v.visit (this);
    }
}

class EAdd extends Exp {
    public Exp e1;
    public Exp e2;
    public EAdd (Exp e1, Exp e2) {
        this.e1 = e1;
        this.e2 = e2;
    }
    public Integer accept (EvalVisitor v) {
        return v.visit (this);
    }
}

class ECall extends Exp {
    public String f;
    public List<Exp> es;
    public ECall (String f, List<Exp> es) {
        this.f = f;
        this.es = es;
    }
    public Integer accept (EvalVisitor v) {
        return v.visit (this);
    }
}

interface EvalVisitor {
    public Integer visit (EInt e);
    public Integer visit (EAdd e);
    public Integer visit (ECall e);
}

```

Input file grammar
------------------

The input file format is similar to Haskell data type declarations,
with the special comment `--visitor`.
```
  datadecl    ::= 'data' uppername '=' constructors visitors

  constructor ::= uppername ['{' fieldlist '}']

  fieldlist   ::= fieldlist ',' field
                | field

  field       ::= lowername '::' type

  type        ::= type atom
                | atom

  atom        ::= name
                | '[' type ']'
                | '(' type ')'

  visitor     ::= '--visitor' type name
```

Limitations
-----------

- Visitors do not support mutually recursive data types.
- Record types with same constructor name as record name do not produce valid Java. E.g.
  ```haskell
  data R = R { f :: A }
  ```
  creates two classes with name `R` and subsequent Java compilation errors.
