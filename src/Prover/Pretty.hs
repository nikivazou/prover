module Prover.Pretty where

import Prover.Types


instance Show (Axiom a) where
   show a = "\n\nAxiom: " ++ par(show (axiom_vars a)) ++ show (axiom_body a)

instance Show (Instance a) where
   show i = "\n\nInstance: " ++ show (inst_axiom i) ++ par (show (inst_args i)) 

instance Show (Var a) where
   show v = show (var_name v)

instance Show (Expr a) where
   show (EVar v)    = show v 
   show (EApp c es) = show c ++ par (show es)  

par str = " (" ++ show str ++ ") "