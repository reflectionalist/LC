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


-- shift
shf :: Int -> Imp -> Imp
shf = lsh 0
  where lsh :: Int -> Int -> Imp -> Imp  -- leveled shift
        lsh lvl stp imp = case imp of
          Var ind | ind < lvl -> imp
                  | otherwise -> Var (ind + stp)
          Abs bod             -> Abs $ lsh (lvl + 1) stp bod
          App opr opd         -> App (lsh lvl stp opr) (lsh lvl stp opd)

-- capture-free substitution
cfs :: Ind -> Imp -> Imp -> Imp
cfs fvi sub imp = case imp of
  Var ind     | fvi == ind -> sub
              | otherwise  -> imp
  Abs bod     -> Abs $ cfs (fvi + 1) (shf 1 sub) bod
  App opr opd -> App (cfs fvi sub opr) (cfs fvi sub opd)

-- binding-free substitution
bfs :: Imp -> Imp -> Imp
bfs sub = shf -1 . cfs 0 (shf 1 sub)

-- by-name normalization
bnn :: Imp -> Imp
bnn imp = case imp of
  Var _       -> imp
  Abs _       -> imp
  App opr opd -> case bnn opr of
    Abs bod -> bnn (bfs opd bod)
    whn     -> App whn opd

-- normal-order normalization
non :: Imp -> Imp
non imp = case imp of
  Var _       -> imp
  Abs bod     -> Abs (non bod)
  App opr opd -> case bnn opr of
    Abs bod -> non (bfs opd bod)
    whn     -> App (non whn) (non opd)

-- by-value normalization
bvn :: Imp -> Imp
bvn imp = case imp of
  Var _       -> imp
  Abs _       -> imp
  App opr opd -> case bvn opr of
    Abs bod -> bvn $ bfs (bvn opd) bod
    wnf     -> App wnf (bvn opd)

-- applicative-order normalization
aon :: Imp -> Imp
aon imp = case imp of
  Var _       -> imp
  Abs bod     -> Abs (aon bod)
  App opr opd -> case aon opr of
    Abs bod -> aon $ bfs (aon opd) bod
    nfm     -> App nfm (aon opd)

-- hybrid applicative-order normalization
han :: Imp -> Imp
han imp = case imp of
  Var _       -> imp
  Abs bod     -> Abs (han bod)
  App opr opd -> case bvn opr of
    Abs bod -> han $ bfs (han opd) bod
    wnf     -> App (han wnf) (han opd)

-- head-spine normalization
hsn :: Imp -> Imp
hsn imp = case imp of
  Var _       -> imp
  Abs bod     -> Abs (hsn bod)
  App opr opd -> case hsn opr of
    Abs bod -> hsn (bfs opd bod)
    hnf     -> App hnf opd

-- head normalization
hdn :: Imp -> Imp
hdn imp = case imp of
  Var _       -> imp
  Abs bod     -> Abs (hdn bod)
  App opr opd -> case bnn opr of
    Abs bod -> hdn (bfs opd bod)
    whn     -> App whn opd

-- hybrid normal-order normalization
hnn :: Imp -> Imp
hnn imp = case imp of
  Var _       -> imp
  Abs bod     -> Abs (hnn bod)
  App opr opd -> case hsn opr of
    Abs bod -> hnn (bfs opd bod)
    hnf     -> App (hnn hnf) (hnn opd)

