data Exp
  = EInt  { i :: Integer }
  | EAdd  { e1 :: Exp, e2 :: Exp }
  | ECall { f :: String, es :: [Exp] }
--visitor Integer EvalVisitor
