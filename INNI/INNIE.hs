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


type Msg = String
data Err
  = Unbound Nom
  | Unknown Msg

instance Error Err where
  strMsg = Unknown

instance Show Err where
  show err = case err of
      Unbound nom -> "unbound variable `" ++ nom ++ "'"
      Unknown msg -> msg

type Stp = Integer
type Normalization r = ReaderT Env (ErrorT Err (StateT Stp Identity)) r

tick :: (Num s, MonadState s m) => m ()
tick = do st <- get
          put (st + 1)

run :: Env -> Stp -> Normalization r -> (Either Err r, Stp)
run env stp nlz = runIdentity $ runStateT (runErrorT $ runReaderT nlz env) stp

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

normalize :: Imp -> Normalization Imp
normalize imp = case imp of
    Var nom ind -> do
        tick
        env <- ask
        case search env nom ind of
            Exact imp -> return imp
            Beyond    -> return $ Var nom (ind - 1)
            None      -> throwError (Unbound nom)
    All nom bod -> do tick
                      env <- ask
                      return (Clo env nom bod)
    App opr opd -> do
        tick
        whd <- normalize opr
        case whd of
            Clo sen nom bod -> do arg <- normalize opd
                                  local (const $ extend sen nom arg)
                                        (normalize bod)
            _               -> do arg <- normalize opd
                                  return $ App whd arg

deep :: Imp -> Normalization Imp
deep imp = case imp of
    Clo sen nom bod -> do let wen = fmap (fmap $ shift nom 0 1) sen
                              len = extend wen nom (var nom)
                          whd <- local (const len) (normalize bod)
                          hd  <- deep whd
                          return (All nom hd)
    App opr opd     -> do hd  <- deep opr
                          arg <- deep opd
                          return (App hd arg)
    _               -> return imp

norm :: Imp -> Normalization Imp
norm imp = normalize imp >>= deep

nlz :: Imp -> Imp
nlz imp = case run M.empty 0 (norm imp) of
    (Left err, stp) -> error $ show err
                            ++ " encountered after "
                            ++ show stp ++ " steps"
    (Right hn, _  ) -> hn


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

