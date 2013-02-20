module EINNI where


import Prelude as P hiding (abs)
import Data.Map.Strict as M
import Data.Sequence as S


type Nom = String
type Ind = Int
type Lvl = Int

data Imp
  = Var Nom Ind
  | Abs Nom Imp
  | App Imp Imp
  | Clo Env Nom Imp
  deriving (Show)

var :: Nom -> Imp
var nom = Var nom 0

abs :: [Nom] -> Imp -> Imp
abs = flip (P.foldr Abs)

app :: Imp -> [Imp] -> Imp
app = P.foldl App

type Env = Map Nom (Seq Imp)

search :: Env -> Nom -> Ind -> Maybe (Maybe Imp)
search env nom ind = do
  seq <- M.lookup nom env
  if ind < S.length seq
     then Just (Just $ S.index seq ind)
     else Just Nothing

extend :: Env -> Nom -> Imp -> Env
extend env nom val
  | M.null env = M.singleton nom (S.singleton val)
  | otherwise  = M.insertWith (S.><) nom (S.singleton val) env

eval :: Env -> Imp -> Imp
eval env imp = case imp of
  Var nom ind -> case search env nom ind of
    Just Nothing    -> Var nom (ind - 1)
    Just (Just val) -> val
    Nothing         -> imp
  Abs nom bod -> Clo env nom bod
  App opr opd -> case eval env opr of
    Clo sen nom bod -> eval (extend sen nom $ eval env opd) bod
    wnf             -> App wnf (eval env opd)

shif :: Nom -> Lvl -> Int -> Imp -> Imp
shif nam lvl stp imp = case imp of
  Var nom ind | nom /= nam -> imp
              | ind < lvl  -> imp
              | otherwise  -> Var nom (ind + stp)
  Abs nom bod     -> Abs nom $ shif nam (if nam == nom then lvl + 1 else lvl) stp bod
  Clo env nom bod -> Clo env nom $ shif nam (if nam == nom then lvl + 1 else lvl) stp bod
  App opr opd     -> App (shif nam lvl stp opr) (shif nam lvl stp opd)

reif :: Imp -> Imp
reif imp = case imp of
  Clo env nom bod -> let len = extend (M.map (fmap $ shif nom 0 1) env) nom (var nom)
                     in  Abs nom $ reif (eval len bod)
  App opr opd     -> App (reif opr) (reif opd)
  _               -> imp

nbye :: Imp -> Imp
nbye = reif . eval M.empty

{- Tests
nbye $ app (abs ["x", "y"] (var "x")) [(var "y")]
nbye $ app (abs ["x", "y", "z"] (var "x")) [var "y"]
nbye $ app (abs ["x", "y", "y"] (var "x")) [var "y"]
nbye $ app (abs ["x", "y", "y"] (var "x")) [abs ["x"] (var "y")]
nbye $ app (abs ["x", "y", "y"] (var "x")) [abs ["x", "y"] (var "y")]
nbye $ app (abs ["x", "y", "y"] (var "x")) [abs ["x", "y", "z"] (var "y")]
-}

