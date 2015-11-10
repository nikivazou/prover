{-# LANGUAGE TypeSynonymInstances #-}

module Prover.Types where

import qualified Language.Fixpoint.Types as F 


data Axiom a = Axiom { axiom_vars :: [Var a]
                     , axiom_body :: Expr a
                     } 

data Var a   = Var { var_name :: F.Symbol
                   , var_sort :: F.Sort 
                   , var_info :: a 
                   } 

type Ctor a  = Var a 

data Expr a  = EVar (Var a) 
             | EApp (Ctor a) [Expr a]

type Predicate a = F.Pred

type Proof a     = [Instance a]

data Instance a  = Inst { inst_axiom :: Axiom a 
                        , inst_args  :: [Expr a]
                        }


data Query a = Query { pr_axioms :: [Axiom a] 
                     , pr_ctors  :: [Ctor a] 
                     , pr_vars   :: [Var a] 
                     , pr_pred   :: Predicate a
                     } 

instance Monoid (Query a) where
    mempty        = Query { pr_axioms = mempty
                          , pr_ctors  = mempty
                          , pr_vars   = mempty
                          , pr_pred   = mempty
                          }
    mappend q1 q2 = Query { pr_axioms = pr_axioms q1 `mappend` pr_axioms q2
                          , pr_ctors  = pr_ctors  q1 `mappend` pr_ctors  q2 
                          , pr_vars   = pr_vars   q1 `mappend` pr_vars   q2 
                          , pr_pred   = pr_pred   q1 `mappend` pr_pred   q2 
                          }