module Norm
  ( normalize )
where


import SExp (Name)
import Form


normalize :: Form -> REnv -> (REnv, Form)
normalize (Var name indx) renv
  = (renv, searchREnv name indx renv)
normalize (Lam name body) renv
  = (renv, Lam name . snd . normalize body $ liftupREnv name renv)
normalize (App optr opnd) renv
  = reduce (snd $ normalize optr renv) opnd renv
normalize (Def name form) renv
  = let (nenv, norm) = normalize form renv
     in (extendREnv name norm nenv, Nul)

reduce :: Form -> Form -> REnv -> (REnv, Form)
reduce (Lam name body) opnd renv
  = let lenv = extendREnv name (snd $ normalize opnd renv) vacantREnv
     in (renv, snd . normalize body $ liftupREnv name lenv)
reduce optr opnd renv
  = (renv, App optr . snd $ normalize opnd renv)

