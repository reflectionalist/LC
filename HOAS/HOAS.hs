type Nom = String

data Imp
  = Var Nom
  | Abs (Imp -> Imp)
  | App Imp Imp


-- by-name normalization
bnn :: Imp -> Imp
bnn imp = case imp of
  Var _       -> imp
  Abs _       -> imp
  App opr opd -> case bnn opr of
    Abs fun -> bnn (fun opd)
    whn     -> App whn opd

-- normal-order normalization
non :: Imp -> Imp
non imp = case imp of
  Var _       -> imp
  Abs fun     -> Abs (bnn . fun)
  App opr opd -> case bnn opr of
    Abs fun -> non (fun opd)
    whn     -> App (non whn) (non opd)

-- by-value normalization
bvn :: Imp -> Imp
bvn imp = case imp of
  Var _       -> imp
  Abs _       -> imp
  App opr opd -> case bnn opr of
    Abs fun -> bvn $ fun (bvn opd)
    wnf     -> App wnf (bvn opd)

-- applicative-order normalization
aon :: Imp -> Imp
aon imp = case imp of
  Var _       -> imp
  Abs fun     -> Abs (aon . fun)
  App opr opd -> case aon opr of
    Abs fun -> aon $ fun (aon opd)
    nfm     -> App nfm (aon opd)

-- hybrid applicative-order normalization
han :: Imp -> Imp
han imp = case imp of
  Var _       -> imp
  Abs fun     -> Abs (han . fun)
  App opr opd -> case bvn opr of
    Abs fun -> han $ fun (han opd)
    wnf     -> App (han wnf) (han opd)

-- head-spine normalization
hsn :: Imp -> Imp
hsn imp = case imp of
  Var _       -> imp
  Abs fun     -> Abs (hsn . fun)
  App opr opd -> case hsn opr of
    Abs fun -> hsn (fun opd)
    hnf     -> App hnf opd

-- head normalization
hdn :: Imp -> Imp
hdn imp = case imp of
  Var _       -> imp
  Abs fun     -> Abs (hdn . fun)
  App opr opd -> case bnn opr of
    Abs fun -> hdn (fun opd)
    whn     -> App whn opd

-- hybrid normal-order normalization
hnn :: Imp -> Imp
hnn imp = case imp of
  Var _       -> imp
  Abs fun     -> Abs (hnn . fun)
  App opr opd -> case hsn opr of
    Abs fun -> hnn (fun opd)
    hnf     -> App (hnn hnf) (hnn opd)

