module Main where

import System.Environment             (getArgs)

import Prover.Solve 
import Prover.Parser 

import Prover.Pretty ()

main :: IO ()
main = getArgs >>= (runSolver . head)


runSolver :: FilePath -> IO ()
runSolver fn = 
  do query <- parseQuery fn
     putStrLn ("initProver " ++ show (solve query))