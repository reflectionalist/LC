module Form
  ( Form(..)
  , internalize, externalize
  , REnv
  , vacantREnv, searchREnv, extendREnv
  , liftupForm, liftupREnv )
where


import SExp
import Data.List
import Prelude as Pld
import Data.Map as Map


type Indx = Int

data Form
  = Var Name Indx
  | Lam Name Form
  | App Form Form
  | Def Name Form
  | Nul


desugarLams :: ([SExp], SExp) -> Form
desugarLams ([Atom name], sexp)
  = Lam name $ internalize sexp
desugarLams (Atom name : atoms, sexp)
  = Lam name $ desugarLams (atoms, sexp)

desugarApps :: [SExp] -> Form
desugarApps (sexp0 : sexp1 : sexps)
  = Pld.foldl (\papp opnd -> App papp $ internalize opnd)
              (App (internalize sexp0) (internalize sexp1))
              sexps

internalize :: SExp -> Form
internalize (Atom name)
  = Var name 0
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
externalize (Var name indx)
  = Atom (name ++ if indx == 0 then "" else show indx)
externalize form@(Lam _ _)
  = let (atoms, sexp) = ensugarLams form
     in List [Atom "lambda", List atoms, sexp]
externalize form@(App _ _)
  = List $ ensugarApps form
externalize (Def name form)
  = List [Atom "define", Atom name, externalize form]
externalize Nul
  = List []

instance Show Form where
  show = show . externalize


type REnv = Map Name [Form]

vacantREnv :: REnv
vacantREnv = empty

searchREnv :: Name -> Indx -> REnv -> Form
searchREnv nick indx renv
  = case Map.lookup nick renv
      of Nothing    -> Var nick indx
         Just forms -> forms !! indx

extendREnv :: Name -> Form -> REnv -> REnv
extendREnv name form renv
  | name `member` renv = adjust (form : ) name renv
  | otherwise          = Map.insert name [form] renv

liftupForm :: Name -> Form -> Form
liftupForm name form@(Var nick indx)
  | nick == name = Var name (indx + 1)
  | otherwise    = form
liftupForm name form@(Lam nick body)
  | nick == name = form
  | otherwise    = Lam nick (liftupForm name body)
liftupForm name (App optr opnd)
  = App (liftupForm name optr) (liftupForm name opnd)

liftupREnv :: Name -> REnv -> REnv
liftupREnv name
  = Map.mapWithKey liftUpon
  where liftUpon nick forms
          | nick == name = forms
          | otherwise    = Pld.map (liftupForm name) forms

