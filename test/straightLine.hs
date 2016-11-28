-- Haskell grammar for StraightLine abs. syn.

data Stm
  = CompoundStm { stm1 :: Stm, stm2 :: Stm }
  | AssignStm   { id :: String, exp :: Exp }
  | PrintStm    { exps :: ExpList }

data Exp
  = IdExp   { id :: String }
  | NumExp  { num :: int }
  | OpExp   { left :: Exp, op :: int, right :: Exp }
  | EseqExp { stm :: Stm, exp :: Exp }

data ExpList
  = PairExpList { head :: Exp, tail :: ExpList }
  | LastExpList { head :: Exp }
