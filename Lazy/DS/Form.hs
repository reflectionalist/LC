module Form
  ( Form(..)
  , internalize, externalize
  , REnv
  , searchREnv, extendREnv )
where


import SExp
import Data.List


data Form
  = Var Name
  | Lam Name Form
  | App Form Form
  | Clo REnv Name Form
  | Def Name Form
  | Nul


desugarLams :: ([SExp], SExp) -> Form
desugarLams ([Atom name], sexp)
  = Lam name $ internalize sexp
desugarLams (Atom name : atoms, sexp)
  = Lam name $ desugarLams (atoms, sexp)

desugarApps :: [SExp] -> Form
desugarApps (sexp0 : sexp1 : sexps)
  = foldl (\papp opnd -> App papp $ internalize opnd)
          (App (internalize sexp0) (internalize sexp1))
          sexps

internalize :: SExp -> Form
internalize (Atom name)
  = Var name
internalize (List [Atom "lambda", List atoms@(_:_), sexp])
  = desugarLams (atoms, sexp)
internalize sexp@(List (Atom "lambda" : _))
  = error $ "syntax error: " ++ show sexp
internalize (List [Atom "define", Atom name, sexp])
  = Def name $ internalize sexp
internalize sexp@(List (Atom "define" : _))
  = error $ "syntax error: " ++ show sexp
internalize (List sexps@(_:_:_))
  = desugarApps sexps
internalize (List [sexp])
  = internalize sexp
internalize (List [])
  = Nul

ensugarLams :: Form -> ([SExp], SExp)
ensugarLams (Lam name form@(Lam _ _))
  = let (atoms, sexp) = ensugarLams form
     in (Atom name : atoms, sexp)
ensugarLams (Lam name form)
  = ([Atom name], externalize form)

ensugarApps :: Form -> [SExp]
ensugarApps (App form0@(App _ _) form1)
  = let sexps = ensugarApps form0
     in sexps ++ [externalize form1]
ensugarApps (App form0 form1)
  = [externalize form0, externalize form1]

externalize :: Form -> SExp
externalize (Var name)
  = Atom name
externalize form@(Lam _ _)
  = let (atoms, sexp) = ensugarLams form
     in List [Atom "lambda", List atoms, sexp]
externalize form@(App _ _)
  = List $ ensugarApps form
externalize (Clo _ name body)
  = externalize (Lam name body)
externalize (Def name form)
  = List [Atom "define", Atom name, externalize form]
externalize Nul
  = List []

instance Show Form where
  show = show . externalize


type Bind = (Name, Form)
type Scop = [Bind]
type REnv = Scop

searchREnv :: REnv -> Name -> Form
searchREnv renv nick = case lookup nick renv of
  Nothing   -> Var nick
  Just form -> form

extendREnv :: REnv -> [Bind] -> REnv
extendREnv = foldl' $ flip (:)

