data Imp
  = Abs (Imp -> Imp)
  | App Imp Imp


-- by-name normalization
bnn :: Imp -> Imp
bnn imp = case imp of
  Abs fun     -> imp
  App opr opd -> case bnn opr of
    Abs fun -> bnn (fun opd)

-- normal-order normalization
non :: Imp -> Imp
non imp = case imp of
  Abs fun     -> Abs (bnn . fun)
  App opr opd -> case bnn opr of
    Abs fun -> non (fun opd)

-- by-value normalization
bvn :: Imp -> Imp
bvn imp = case imp of
  Abs fun     -> imp
  App opr opd -> case bnn opr of
    Abs fun -> bvn $ fun (bvn opd)

-- applicative-order normalization
aon :: Imp -> Imp
aon imp = case imp of
  Abs fun     -> Abs (aon . fun)
  App opr opd -> case aon opr of
    Abs fun -> aon $ fun (aon opd)

-- hybrid applicative-order normalization
han :: Imp -> Imp
han imp = case imp of
  Abs fun     -> Abs (han . fun)
  App opr opd -> case bvn opr of
    Abs fun -> han $ fun (han opd)

-- head-spine normalization
hsn :: Imp -> Imp
hsn imp = case imp of
  Abs fun     -> Abs (hsn . fun)
  App opr opd -> case hsn opr of
    Abs fun -> hsn (fun opd)

-- head normalization
hdn :: Imp -> Imp
hdn imp = case imp of
  Abs fun     -> Abs (hdn . fun)
  App opr opd -> case bnn opr of
    Abs fun -> hdn (fun opd)

-- hybrid normal-order normalization
hnn :: Imp -> Imp
hnn imp = case imp of
  Abs fun     -> Abs (hnn . fun)
  App opr opd -> case hsn opr of
    Abs fun -> hnn (fun opd)

