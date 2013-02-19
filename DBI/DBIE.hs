import Prelude hiding (abs)


type Ind = Int
type Lvl = Int

data Imp
  = Var Ind
  | Abs Imp
  | App Imp Imp
  | Clo Env Imp
  deriving (Show)

var :: Ind -> Imp
var = Var

abs :: Imp -> Imp
abs = Abs

app :: Imp -> Imp -> Imp
app = App

type Env = [Imp]

-- an efficient implementation

-- by-name evaluation
bne :: Env -> Imp -> Imp
bne env imp = case imp of
  Var _       -> imp
  Abs bod     -> Clo env bod
  App opr opd -> case bne env opr of
    Clo env bod -> bne (opd : env) bod
    whn         -> App whn opd

rfy :: Imp -> Imp
rfy imp = case imp of
  Clo env bod -> Abs . rfy $ bne (Var 0 : env) bod
  App opr opd -> App (rfy opr) (rfy opd)
  _           -> imp

nbe :: Imp -> Imp
nbe imp = rfy (bne [] imp)

