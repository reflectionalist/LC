module INNIE
  ( var, all, app, als, aps
  , normalize, deep, norm, nlz )
where


import Prelude as P hiding (all)
import Data.Map.Strict as M
import Data.Sequence as S

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader hiding (All)
import Control.Monad.State
import Control.Monad.Writer hiding (All)


type Nom = String
type Ind = Int
type Lvl = Int

data Imp
  = Var Nom Ind
  | All Nom Imp
  | App Imp Imp
  | Clo Env Nom Imp
  deriving (Show)

var :: Nom -> Imp
var nom = Var nom 0

all :: Nom -> Imp -> Imp
all = All

app :: Imp -> Imp -> Imp
app = App

als :: [Nom] -> Imp -> Imp
als = flip (P.foldr All)

aps :: Imp -> [Imp] -> Imp
aps = P.foldl App


type Env = Map Nom (Seq Imp)

data Result a
  = None
  | Exact a
  | Beyond

search :: Env -> Nom -> Ind -> Result Imp
search env nom ind = case M.lookup nom env of
    Just seq | ind < S.length seq -> Exact (S.index seq ind)
             | otherwise          -> Beyond
    Nothing  -> None

extend :: Env -> Nom -> Imp -> Env
extend env nom imp
  | M.null env = M.singleton nom (S.singleton imp)
  | otherwise  = M.insertWith (S.><) nom (S.singleton imp) env


type Normalization r = ErrorT String Identity r

run :: Normalization r -> Either String r
run = runIdentity . runErrorT

normalize :: Env -> Imp -> Normalization Imp
normalize env imp = case imp of
    Var nom ind -> case search env nom ind of
        Exact imp -> return imp
        Beyond    -> return $ Var nom (ind - 1)
        None      -> fail $ "Unbound variable: " ++ show imp
    All nom bod -> return (Clo env nom bod)
    App opr opd -> do
        whd <- normalize env opr
        case whd of
            Clo sen nom bod -> do arg <- normalize env opd
                                  normalize (extend sen nom arg) bod
            _               -> do arg <- normalize env opd
                                  return $ App whd arg

shift :: Nom -> Lvl -> Int -> Imp -> Imp
shift nam lvl stp imp = case imp of
    Var nom ind | nom /= nam -> imp
                | ind < lvl  -> imp
                | otherwise  -> Var nom (ind + stp)
    All nom bod     -> All nom     $ shift nam (level nom) stp bod
    Clo env nom bod -> Clo env nom $ shift nam (level nom) stp bod
    App opr opd     -> App (shift nam lvl stp opr) (shift nam lvl stp opd)
  where
    level nom = if nom == nam then lvl + 1 else lvl

deep :: Imp -> Normalization Imp
deep imp = case imp of
    Clo env nom bod -> do let wen = fmap (fmap $ shift nom 0 1) env
                              len = extend wen nom (var nom)
                          whd <- normalize len bod
                          hd  <- deep whd
                          return (All nom hd)
    App opr opd     -> do hd  <- deep opr
                          arg <- deep opd
                          return (App hd arg)
    _               -> return imp

norm :: Env -> Imp -> Normalization Imp
norm env imp = normalize env imp >>= deep

nlz :: Imp -> Imp
nlz imp = case run (norm M.empty imp) of
    Left err -> error err
    Right hn -> hn


tests :: [Imp]
tests = fmap nlz
  [ all "y" (app (als ["x", "y"] (var "x"))
            (var "y") )
  , all "y" (app (als ["x", "y", "z"] (var "x"))
            (var "y") )
  , all "y" (app (als ["x", "y", "y"] (var "x"))
            (var "y") )
  , all "y" (app (als ["x", "y", "y"] (var "x"))
            (all "x" (var "y")) )
  , all "y" (app (als ["x", "y", "y"] (var "x"))
            (als ["x", "y"] (var "y")) )
  , all "y" (app (als ["x", "y", "y"] (var "x"))
            (als ["x", "y", "z"] (var "y")) )
  ]

