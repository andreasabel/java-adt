data Exp
  = EId { x :: String }
  | EAdd { e1 :: Exp, e2 :: Exp }
  | ESub { e1 :: Exp, e2 :: Exp }
--visitor void EvalVisitor
