module Main where


import System.IO
import Text.Parsec
import SExp (parseSExp, readSExp)
import INNI


readForm :: String -> String -> Imp
readForm fnm inp
  = case parse parseSExp fnm inp
      of Left  perr -> error (show perr)
         Right sexp -> internalize sexp

flushStr :: String -> IO ()
flushStr str
  = putStr str >> hFlush stdout

prompt :: IO ()
prompt = flushStr "LC> "

repl :: (Imp -> Imp) -> IO ()
repl cal
  = do prompt
       inp <- readSExp
       if inp == ""
          then putStrLn ""
          else case words inp of
                 [":set"]         -> do putStrLn ("Too few arguments to :set")
                                        repl cal
                 ":set" : nam : _ -> case lookup nam cals of
                   Just cal -> repl cal
                   Nothing  -> do putStrLn ("Unsupported calculators: " ++ nam)
                                  repl cal
                 _                -> do let imp = readForm "stdin" inp
                                            ans = cal imp
                                        putStrLn (show ans)
                                        repl cal

cals :: [(String, Imp -> Imp)]
cals =
  [ ("bnn", bnn)
  , ("non", non)
  , ("bvn", bvn)
  , ("aon", aon)
  , ("han", han)
  , ("hsn", hsn)
  , ("hdn", hdn)
  , ("hnn", hnn) ]

main :: IO ()
main = repl non

