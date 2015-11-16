module Prover.SMTInterface where

import Language.Fixpoint.Smt.Interface 

import Language.Fixpoint.Types

makeContext :: FilePath -> [(Symbol, Sort)] -> IO Context 
makeContext = makeZ3Context

checkValid :: Context -> [Pred] -> Pred -> IO Bool
checkValid me is q = checkValidWithContext me [] (pAnd is) q

assert :: Context -> Pred -> IO ()
assert =  smtAssert
