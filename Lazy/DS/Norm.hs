module Norm
  ( normalize )
where


import Form


normalize :: REnv -> Form -> (REnv, Form)
normalize renv (Var name)
  = (renv, searchREnv renv name)
normalize renv (Lam name body)
  = (renv, Clo renv name body)
normalize renv (App optr opnd)
  = reduce renv (snd $ normalize renv optr) opnd
normalize renv (Def name form)
  = let (nenv, norm) = normalize renv form
     in ((name, norm) : nenv, Nul)

reduce :: REnv -> Form -> Form -> (REnv, Form)
reduce renv (Clo senv name body) opnd
  = (renv, snd $ normalize (extendREnv senv [(name, snd $ normalize renv opnd)]) body)

