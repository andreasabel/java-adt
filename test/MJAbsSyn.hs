-- Grammar for MJ ASTs

data Maybe a
  = Nothing
  | Just { just :: a }

data Decl =
  MethDecl    { ty :: Type, id :: String, ps :: [Param]
              , ds :: [Decl], ss :: [Statement], e ::  Expression }
  | VarDecl   { ty :: Type, id :: String}
  | ClassDecl { id :: String, id0 :: Maybe String, vs :: [Decl], ms :: [Decl]}
  | MainDecl  { id :: String, id0 :: String, s :: Statement}

data Type = BoolTy | IntTy | ObjTy { c :: Class } | ArrTy { ty :: Type }

data Param = Par { id :: String, ty :: Type }

data Statement
  = SList        { ss :: [Statement] }
  | IfStm        { e  :: Expression, sT :: Statement, sE :: Statement }
  | WhileStm     { e  :: Expression, s :: Statement }
  | PrintStm     { e  :: Expression }
  | AssignStm    { id :: String, e :: Expression }
  | ArrAssignStm { id :: String, e1 :: Expression, e2 :: Expression }

data Expression
  = CTrue
  | CFalse
  | This
  | NewInt { e  :: Expression}
  | New    { id :: String}
  | Infx   { e1 :: Expression, op :: int, e2 :: Expression }
  | Get    { e1 :: Expression, e2 ::  Expression }
  | Len    { e  :: Expression }
  | Invoke { e  :: Expression, id :: String, es :: [Expression] }
  | IConst { n  :: int }
  | Id     { id :: String }
