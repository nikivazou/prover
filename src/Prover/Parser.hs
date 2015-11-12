module Prover.Parser where

import Prover.Types 

import Text.Parsec

import Language.Fixpoint.Parse hiding (bindP)
import Language.Fixpoint.Types        (Pred(PTrue))

parseQuery :: String -> IO BQuery
parseQuery fn = parseFromFile (queryP fn) fn 


queryP fn = do
  vars   <- sepBy bindP whiteSpace
  semi
  axioms <- sepBy axiomP whiteSpace
  semi
  ctors  <- sepBy ctorP whiteSpace
  semi 
  goal   <- goalP
  return $ mempty { q_axioms = axioms
                  , q_vars   = vars
                  , q_ctors  = ctors
                  , q_goal   = goal
                  , q_fname  = fn
                  }

goalP :: Parser (Predicate a)
goalP = reserved "goal" >> colon >> predicateP

ctorP :: Parser BCtor
ctorP = do reserved "constructor"
           v <- varP
           (vs, p) <- try (ctorAxiomP)
           return $ Ctor v vs p

ctorAxiomP 
   =  do reserved "with"
         reserved "forall"
         aargs <- argumentsP
         abody <- predicateP
         return (aargs, abody) 
  <|> return ([], Pred PTrue)

bindP :: Parser BVar
bindP = reserved "bind" >> varP

predicateP :: Parser (Predicate a)
predicateP = Pred <$> predP

axiomP :: Parser BAxiom
axiomP = do 
  reserved "axiom"
  aname <- symbolP
  colon
  reserved "forall"
  aargs <- argumentsP
  abody <- predicateP
  return $ Axiom aname aargs abody


argumentsP :: Parser ([BVar])
argumentsP = brackets $ sepBy varP comma

varP :: Parser BVar
varP = do 
  x <- symbolP
  colon
  s <- sortP
  return $ Var x s ()

