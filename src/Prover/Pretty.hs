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
   show a = showpp (axiom_name a) ++ ": " ++ "forall"++ par(sep ", " $ map show (axiom_vars a)) ++ "."  ++ show (axiom_body a) ++ "\n"

instance Show (Instance a) where
   show i = "\nInstance :: " ++ show (inst_axiom i) ++ "With Arguments :: " ++  (sep ", " $ map show (inst_args i)) 
                          --  ++ "\n\nPredicate = " ++ show (inst_pred i)  ++ "\n\n" 

instance Show (Var a) where
   show v = showpp (var_name v) ++ " : " ++ showpp (var_sort v) 

instance Show (Ctor a) where
   show c = showpp (ctor_var c)

instance Show (Expr a) where
   show (EVar v)    = showpp v 
   show (EApp c es) = show c ++ par (sep ", "  $ map show es)  

instance Show (Predicate a) where
   show (Pred p) = showpp $ pprint p

instance Show (Query a) where
   show q = "\nQuery\n" ++ 
              "\nAxioms::" ++ (showNum $ q_axioms q) ++ 
              "\nVars  ::" ++ (sep ", " $ map showpp $ q_vars   q) ++ 
              "\nCtors ::" ++ (sep ", " $ map show   $ q_ctors  q) ++ 
              "\nDecls ::" ++ (sep ", " $ map show   $ q_decls  q) ++ 
              "\nGoal  ::" ++ (show $ q_goal q) ++ 
              "\nFname ::" ++ (show $ q_fname q)

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