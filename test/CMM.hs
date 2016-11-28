-- C-- syntax

data Exp
  = EId { x :: String }
  | EAdd { e1 :: Exp, e2 :: Exp }
--visitor void EvalVisitor

data Stm
  = SExp { e :: Exp }
  | SIf  { e :: Exp, s1 :: Stm, s2 :: Stm }
-- --visitor void EvalVisitor -- FAILS
