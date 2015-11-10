module Prover.Pretty where

import Prover.Types

import Language.Fixpoint.PrettyPrint

instance Show (Axiom a) where
   show a = "\n\nAxiom: " ++ par(show (axiom_vars a)) ++ show (axiom_body a)

instance Show (Instance a) where
   show i = "\n\nInstance: " ++ show (inst_axiom i) ++ par (show (inst_args i)) 

instance Show (Var a) where
   show v = show (var_name v)

instance Show (Expr a) where
   show (EVar v)    = show v 
   show (EApp c es) = show c ++ par (show es)  

instance Show (Predicate a) where
   show (Pred p) = showpp $ pprint p

instance Show (Query a) where
   show q = "\nQuery\n" ++ 
              "\t\t Axioms = \n\n" ++ (show $ q_axioms q) ++ 
              "\t\t Vars   = \n\n" ++ (show $ q_vars   q) ++ 
              "\t\t Ctors  = \n\n" ++ (show $ q_ctors  q) ++ 
              "\t\t Goal = "   ++ (show $ q_goal q)

par str = " (" ++ show str ++ ") "