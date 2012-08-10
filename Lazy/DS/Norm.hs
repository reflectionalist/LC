module Norm
  ( normalize )
where


import SExp (Name)
import Form


normalize :: Form -> Maybe Name -> REnv -> (REnv, Form)
normalize form@(Var nick indx) Nothing renv
  = (renv, searchREnv nick indx renv)
normalize form@(Var nick indx) (Just name) renv
  | nick == name && indx > 0 = (renv, liftupForm name $ searchREnv name (indx - 1) renv)
  | nick /= name             = (renv, liftupForm name $ searchREnv nick indx renv)
  | otherwise                = (renv, form)
normalize (Lam nick body) _ renv
  = (renv, Lam nick . snd $ normalize body (Just nick) renv)
normalize (App optr opnd) mnam renv
  = reduce (snd $ normalize optr mnam renv) opnd mnam renv
normalize (Def nick form) mnam renv
  = let (nenv, norm) = normalize form mnam renv
     in (extendREnv nick norm nenv, Nul)

reduce :: Form -> Form -> Maybe Name -> REnv -> (REnv, Form)
reduce (Lam nick body) opnd mnam renv
  = let lenv = extendREnv nick (snd $ normalize opnd mnam renv) vacantREnv
     in (renv, snd $ normalize body Nothing lenv)
reduce optr opnd mnam renv
  = (renv, App optr . snd $ normalize opnd mnam renv)

