module INNI where


import Prelude as Pld hiding (abs)
import SExp


type Nom = String
type Ind = Int
type Lvl = Int

data Imp
  = Var Nom Ind
  | Abs Nom Imp
  | App Imp Imp

var :: Nom -> Imp
var nom = Var nom 0

abs :: Nom -> Imp -> Imp
abs = Abs

app :: Imp -> Imp -> Imp
app = App

desugarAbss :: [SExp] -> Imp
desugarAbss [Atom name, sexp]
  = Abs name $ internalize sexp
desugarAbss (Atom name : sexps)
  = Abs name $ desugarAbss sexps

desugarApps :: [SExp] -> Imp
desugarApps (sexp : sexp1 : sexps)
  = Pld.foldl (\opr opd -> App opr $ internalize opd)
              (App (internalize sexp) (internalize sexp1))
              sexps

internalize :: SExp -> Imp
internalize (Atom name)
  = var name
internalize (List (Atom "->" : sexps))
  = desugarAbss sexps
internalize (List (Atom "<-" : sexps))
  = desugarApps sexps

ensugarAbss :: Imp -> SExp
ensugarAbss imp = List (Atom "->" : ensugar imp)
  where ensugar (Abs nom bod@(Abs _ _))
          = let sexps = ensugar bod
             in (Atom nom : sexps)
        ensugar (Abs nom bod)
          = [Atom nom, externalize bod]

ensugarApps :: Imp -> SExp
ensugarApps imp = List (Atom "<-" : ensugar imp)
  where ensugar (App opr@(App _ _) opd)
          = let sexps = ensugar opr
             in sexps ++ [externalize opd]
        ensugar (App opr opd)
          = [externalize opr, externalize opd]

externalize :: Imp -> SExp
externalize imp = case imp of
  Var nom ind -> Atom (nom ++ if ind == 0 then "" else show ind)
  Abs _ _     -> ensugarAbss imp
  App _ _     -> ensugarApps imp

instance Show Imp where
  show = show . externalize


-- is normal form?
isNFM :: Imp -> Bool
isNFM imp = case imp of
  Var _   _   -> True
  Abs _   bod -> isNFM bod
  App opr opd -> isNFM opr && isNFM opd

-- is head normal form?
isHNF :: Imp -> Bool
isHNF imp = case imp of
  Var _   _   -> True
  Abs _   bod -> isHNF bod
  App opr opd -> isHNF opr

-- is weak normal form?
isWNF :: Imp -> Bool
isWNF imp = case imp of
  Var _   _   -> True
  Abs _   bod -> True
  App opr opd -> isWNF opr && isWNF opd

-- is weak head normal form?
isWHN :: Imp -> Bool
isWHN imp = case imp of
  Var _   _   -> True
  Abs _   bod -> True
  App opr opd -> isWHN opr


{- an inefficient implementation

type Sub = Imp -> Imp

-- by-name normalization
bnn :: Imp -> Imp
bnn imp = case imp of
  Var _ _     -> imp
  Abs _ _     -> imp
  App opr opd -> case bnn opr of
    Abs nom bod -> bnn $ cfs (spl nom opd) bod
    whn         -> App whn opd

-- normal-order normalization
non :: Imp -> Imp
non imp = case imp of
  Var _ _     -> imp
  Abs nom bod -> Abs nom (non bod) 
  App opr opd -> case bnn opr of
    Abs nom bod -> non $ cfs (spl nom opd) bod
    whn         -> App (non whn) (non opd)

-- capture-free substitution
cfs :: Sub -> Imp -> Imp
cfs sub imp = case imp of
  Var _ _     -> sub imp
  Abs nom bod -> Abs nom $ cfs (lif nom sub) bod
  App opr opd -> App (cfs sub opr) (cfs sub opd)

-- simple substitution
spl :: Nom -> Imp -> Sub
spl nam imp var@(Var nom ind)
  | nom /= nam = var
  | ind == 0   = imp
  | otherwise  = Var nom (ind - 1)

-- shift substitution
shf :: Nom -> Int -> Sub
shf nam stp var@(Var nom ind)
  | nom == nam = Var nom (ind + stp)
  | otherwise  = var

-- lift substitution
lif :: Nom -> Sub -> Sub
lif nam sub var@(Var nom ind)
  | nom /= nam = shf nam 1 (sub var)
  | ind == 0   = var
  | otherwise  = shf nam 1 . sub $ Var nom (ind - 1)
-}


-- an efficient implementation

-- by-name normalization
bnn :: Imp -> Imp
bnn imp = case imp of
  Var _   _   -> imp
  Abs _   _   -> imp
  App opr opd -> case bnn opr of
    Abs nom bod -> bnn (cfs nom 0 opd bod)
    whn         -> App whn opd

-- normal-order normalization
non :: Imp -> Imp
non imp = case imp of
  Var _   _   -> imp
  Abs nom bod -> Abs nom (non bod)
  App opr opd -> case bnn opr of
    Abs nom bod -> non (cfs nom 0 opd bod)
    whn         -> App (non whn) (non opd)

-- by-value normalization
bvn :: Imp -> Imp
bvn imp = case imp of
  Var _   _   -> imp
  Abs _   _   -> imp
  App opr opd -> case bvn opr of
    Abs nom bod -> case bvn opd of
                     arg | isWNF arg -> bvn $ cfs nom 0 arg bod
    wnf         -> case bvn opd of
                     arg | isWNF arg -> App wnf arg

-- applicative-order normalization
aon :: Imp -> Imp
aon imp = case imp of
  Var _ _     -> imp
  Abs nom bod -> Abs nom (aon bod)
  App opr opd -> case aon opr of
    Abs nom bod -> case aon opd of
                     arg | isNFM arg -> aon $ cfs nom 0 arg bod
    nfm         -> case aon opd of
                     arg | isNFM arg -> App nfm arg

-- hybrid applicative-order normalization
han :: Imp -> Imp
han imp = case imp of
  Var _ _     -> imp
  Abs nom bod -> Abs nom (han bod)
  App opr opd -> case bvn opr of
    Abs nom bod -> case han opd of
                     arg | isNFM arg -> han $ cfs nom 0 arg bod
    wnf         -> case han opd of
                     arg | isNFM arg -> App (han wnf) arg

-- head-spine normalization
hsn :: Imp -> Imp
hsn imp = case imp of
  Var _ _     -> imp
  Abs nom bod -> Abs nom (hsn bod)
  App opr opd -> case hsn opr of
    Abs nom bod -> hsn $ cfs nom 0 opd bod
    hnf         -> App hnf opd

-- head normalization
hdn :: Imp -> Imp
hdn imp = case imp of
  Var _ _     -> imp
  Abs nom bod -> Abs nom (hdn bod)
  App opr opd -> case bnn opr of
    Abs nom bod -> hdn $ cfs nom 0 opd bod
    whn         -> App whn opd

-- hybrid normal-order normalization
hnn :: Imp -> Imp
hnn imp = case imp of
  Var _ _     -> imp
  Abs nom bod -> Abs nom (hnn bod)
  App opr opd -> case hsn opr of
    Abs nom bod -> hnn $ cfs nom 0 opd bod
    hnf         -> App (hnn hnf) (hnn opd)

-- capture-free substitution
cfs :: Nom -> Lvl -> Imp -> Imp -> Imp
cfs nam lvl sub imp = case imp of
  Var nom ind | nom /= nam -> imp
              | ind < lvl  -> imp
              | ind > lvl  -> Var nom (ind - 1)
              | otherwise  -> sub
  Abs nom bod | nom /= nam -> Abs nom $ cfs nam lvl (shf nom 0 1 sub) bod
              | otherwise  -> Abs nom $ cfs nam (lvl + 1) (shf nom lvl 1 sub) bod
  App opr opd -> App (cfs nam lvl sub opr) (cfs nam lvl sub opd)

-- shift named-index by 1
shf :: Nom -> Lvl -> Int -> Imp -> Imp
shf nam lvl stp imp = case imp of
  Var nom ind | nom /= nam -> imp
              | ind < lvl  -> imp
              | otherwise  -> Var nom (ind + stp)
  Abs nom bod | nom /= nam -> Abs nom $ shf nom lvl stp bod
              | otherwise  -> Abs nom $ shf nam (lvl + 1) stp bod
  App opr opd -> App (shf nam lvl stp opr) (shf nam lvl stp opd)

