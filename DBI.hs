import Prelude hiding (abs)


type Ind = Int
type Lvl = Int

data Imp
  = Var Ind
  | Abs Imp
  | App Imp Imp
  deriving (Show)


var :: Ind -> Imp
var = Var

abs :: Imp -> Imp
abs = Abs

app :: Imp -> Imp -> Imp
app = App


{- an inefficient implementation

-- by-name normalization
bnn :: Imp -> Imp
bnn imp = case imp of
  Var _       -> imp
  Abs _       -> imp
  App opr opd -> case bnn opr of
    Abs bod -> bnn . shf 0 -1 $ cfs 0 (shf 0 1 opd) bod
    whn     -> App whn opd

-- capture-free substitution
cfs :: Ind -> Imp -> Imp -> Imp
cfs off sub imp = case imp of
  Var ind     | ind == off -> sub
              | otherwise  -> imp
  Abs bod     -> Abs $ cfs (off + 1) (shf 0 1 sub) bod
  App opr opd -> App (cfs off sub opr) (cfs off sub opd)

-- shift
shf :: Lvl -> Int -> Imp -> Imp
shf lvl stp imp = case imp of
  Var ind     | ind < lvl -> imp
              | otherwise -> Var (ind + stp)
  Abs bod     -> Abs $ shf (lvl + 1) stp bod
  App opr opd -> App (shf lvl stp opr) (shf lvl stp opd)

-}


-- an efficient implementation

-- by-name normalization
bnn :: Imp -> Imp
bnn imp = case imp of
  Var _       -> imp
  Abs _       -> imp
  App opr opd -> case bnn opr of
    Abs bod -> bnn (cfs 0 opd bod)
    whn     -> App whn opd

-- normal-order normalization
non :: Imp -> Imp
non imp = case imp of
  Var _       -> imp
  Abs bod     -> Abs (non bod)
  App opr opd -> case bnn opr of
    Abs bod -> non (cfs 0 opd bod)
    whn     -> App (non whn) (non opd)

-- by-value normalization
bvn :: Imp -> Imp
bvn imp = case imp of
  Var _       -> imp
  Abs _       -> imp
  App opr opd -> case bvn opr of
    Abs bod -> bvn $ cfs 0 (bvn opd) bod
    wnf     -> App wnf (bvn opd)

-- applicative-order normalization
aon :: Imp -> Imp
aon imp = case imp of
  Var _       -> imp
  Abs bod     -> Abs (aon bod)
  App opr opd -> case aon opr of
    Abs bod -> aon $ cfs 0 (aon opd) bod
    nfm     -> App nfm (aon opd)

-- hybrid applicative-order normalization
han :: Imp -> Imp
han imp = case imp of
  Var _       -> imp
  Abs bod     -> Abs (han bod)
  App opr opd -> case bvn opr of
    Abs bod -> han $ cfs 0 (han opd) bod
    wnf     -> App (han wnf) (han opd)

-- head-spine normalization
hsn :: Imp -> Imp
hsn imp = case imp of
  Var _       -> imp
  Abs bod     -> Abs (hsn bod)
  App opr opd -> case hsn opr of
    Abs bod -> hsn (cfs 0 opd bod)
    hnf     -> App hnf opd

-- head normalization
hdn :: Imp -> Imp
hdn imp = case imp of
  Var _       -> imp
  Abs bod     -> Abs (hdn bod)
  App opr opd -> case bnn opr of
    Abs bod -> hdn (cfs 0 opd bod)
    whn     -> App whn opd

-- hybrid normal-order normalization
hnn :: Imp -> Imp
hnn imp = case imp of
  Var _       -> imp
  Abs bod     -> Abs (hnn bod)
  App opr opd -> case hsn opr of
    Abs bod -> hnn (cfs 0 opd bod)
    hnf     -> App (hnn hnf) (hnn opd)

-- capture-free substitution
cfs :: Ind -> Imp -> Imp -> Imp
cfs lvl sub imp = case imp of
  Var ind     | ind < lvl -> imp
              | ind > lvl -> Var (ind - 1)
              | otherwise -> sub
  Abs bod     -> Abs (cfs (lvl + 1) (shf lvl sub) bod)
  App opr opd -> App (cfs lvl sub opr) (cfs lvl sub opd)

-- shift index by 1
shf :: Ind -> Imp -> Imp
shf lvl imp = case imp of
  Var ind     | ind < lvl -> imp
              | otherwise -> Var (ind + 1)
  Abs bod     -> Abs $ shf (lvl + 1) bod
  App opr opd -> App (shf lvl opr) (shf lvl opd)

