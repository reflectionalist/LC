module Main where


import System.IO
import Text.Parsec
import SExp (parseSExp, readSExp)
import Form
import Norm


readForm :: String -> String -> Form
readForm fnam inpt
  = case parse parseSExp fnam inpt
      of Left  perr -> error $ show perr
         Right sexp -> internalize sexp

flushStr :: String -> IO ()
flushStr str
  = putStr str >> hFlush stdout

prompt :: IO ()
prompt = flushStr "CbNLC> "

rep :: REnv -> String -> IO REnv
rep renv inpt
  = do let form = readForm "stdin" inpt
           (nenv, norm) = normalize form renv
       putStrLn $ show norm
       return nenv

repl :: REnv -> IO ()
repl renv
  = do prompt
       inpt <- readSExp
       if inpt == ""
          then putStrLn ""
          else do nenv <- rep renv inpt
                  repl nenv

main :: IO ()
main = repl vacantREnv

