module Prover.Pretty where

import Prover.Types

import Language.Fixpoint.PrettyPrint

instance PPrint (Var a) where
   pprint = pprint . var_name 

instance PPrint (Expr a) where
   pprint = pprint . mkExpr 

instance PPrint (Predicate a) where
   pprint = pprint . p_pred 

instance Show (Axiom a) where
   show a = showpp (axiom_name a) ++ ": " ++ "forall"++ par(sep ", " $ map showpp (axiom_vars a)) ++ "."  ++ show (axiom_body a) ++ "\n"

instance Show (Instance a) where
   show i = "Instance :: " ++ show (inst_axiom i) ++ "With Arguments :: " ++  (sep ", " $ map show (inst_args i)) 

instance Show (Var a) where
   show v = showpp (var_name v)

instance Show (Expr a) where
   show (EVar v)    = showpp v 
   show (EApp c es) = showpp c ++ par (sep ", "  $ map show es)  

instance Show (Predicate a) where
   show (Pred p) = showpp $ pprint p

instance Show (Query a) where
   show q = "\nQuery\n" ++ 
              "\nAxioms::\n" ++ (showNum $ q_axioms q) ++ 
              "\nVars  ::" ++ (sep ", " $ map showpp $ q_vars   q) ++ 
              "\nCtors ::" ++ (sep ", " $ map showpp $ q_ctors  q) ++ 
              "\nGoal  ::" ++ (show $ q_goal q)

instance Show (Proof a) where
  show Invalid    = "\nInvalid\n"
  show (Proof is) = "\nProof ::\n" ++ (sep "\n" $ map show is)


instance Show (ArgExpr a) where
  show ae = "\nArgExpr for " ++ show (arg_sort ae) ++ "\n\nEXPRS = \n\n" ++  (sep ", " (map show $ arg_exprs ae)) ++ 
            "\n\nConstructors = " ++ (sep ", " (map show $ arg_ctors ae)) ++ "\n\n"

showNum ls = concat [ show i ++ " . " ++ show l | (l, i) <- zip ls [1..] ]


par str = " (" ++ str ++ ") "
sep _ []     = []
sep _ [x]    = x
sep c (x:xs) = x ++ c ++ sep c xs