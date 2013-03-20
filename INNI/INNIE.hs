module INNIE
  ( var, all, app, als, aps
  , norm, form, normalize )
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

type Norm r = Identity r

runNorm :: Norm r -> r
runNorm = runIdentity

norm :: Env -> Imp -> Norm Imp
norm env imp = case imp of
  Var nom ind -> case search env nom ind of
    Exact imp -> return imp
    Beyond    -> return $ Var nom (ind - 1)
    None      -> fail $ "Unbound variable: " ++ show imp
  All nom bod -> return (Clo env nom bod)
  App opr opd -> do
    wnf <- norm env opr
    case wnf of
      Clo sen nom bod -> do arg <- norm env opd
                            norm (extend sen nom arg) bod
      wnf             -> do arg <- norm env opd
                            return $ App wnf arg

shift :: Nom -> Lvl -> Int -> Imp -> Imp
shift nam lvl stp imp = case imp of
  Var nom ind | nom /= nam -> imp
              | ind < lvl  -> imp
              | otherwise  -> Var nom (ind + stp)
  All nom bod     -> All nom     $ shift nam (if nam == nom then lvl + 1 else lvl) stp bod
  Clo env nom bod -> Clo env nom $ shift nam (if nam == nom then lvl + 1 else lvl) stp bod
  App opr opd     -> App (shift nam lvl stp opr) (shift nam lvl stp opd)

form :: Imp -> Imp
form imp = case imp of
  Clo env nom bod -> let len = extend (M.map (fmap $ shift nom 0 1) env) nom (var nom)
                     in  All nom $ form $ runNorm (norm len bod)
  App opr opd     -> App (form opr) (form opd)
  _               -> imp

normalize :: Imp -> Imp
normalize = form . runNorm . norm M.empty


tests :: [Imp]
tests =
  [ normalize $ all "y" (app (als ["x", "y"] (var "x")) (var "y"))
  , normalize $ all "y" (app (als ["x", "y", "z"] (var "x")) (var "y"))
  , normalize $ all "y" (app (als ["x", "y", "y"] (var "x")) (var "y"))
  , normalize $ all "y" (app (als ["x", "y", "y"] (var "x")) (all "x" (var "y")))
  , normalize $ all "y" (app (als ["x", "y", "y"] (var "x")) (als ["x", "y"] (var "y")))
  , normalize $ all "y" (app (als ["x", "y", "y"] (var "x")) (als ["x", "y", "z"] (var "y"))) ]

