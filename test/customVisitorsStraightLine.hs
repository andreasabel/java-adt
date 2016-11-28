-- Haskell grammar for StraightLine abs. syn.

data Stm
  = CompoundStm { stm1 :: Stm, stm2 :: Stm }
  | AssignStm   { id :: String, exp :: Exp }
  | PrintStm    { exps :: ExpList }
--visitor void RunVisitor

data Exp
  = IdExp   { id :: String }
  | NumExp  { num :: int }
  | OpExp   { left :: Exp, op :: int, right :: Exp }
  | EseqExp { stm :: Stm, exp :: Exp }
--visitor int EvalVisitor

data ExpList
  = PairExpList { head :: Exp, tail :: ExpList }
  | LastExpList { head :: Exp }
--visitor void PrintVisitor
