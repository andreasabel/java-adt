-- C-- syntax

data Ty = TInt | TDouble

data Exp
  = EId  { x :: String }
  | EAdd { e1 :: Exp, e2 :: Exp }

data Stm
  = SExp   { e :: Exp }
  | SIf    { e :: Exp, s1 :: Stm, s2 :: Stm }
  | SBlock { ss :: [Stm] }
