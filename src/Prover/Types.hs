{-# LANGUAGE TypeSynonymInstances #-}

module Prover.Types where

import qualified Language.Fixpoint.Types as F 
import Language.Fixpoint.Types hiding (Predicate, EApp, EVar, Expr)

type BVar   = Var   ()
type BCtor  = Ctor   ()
type BAxiom = Axiom ()
type BQuery = Query ()


data Axiom a = Axiom { axiom_name :: Symbol
                     , axiom_vars :: [Var a]
                     , axiom_body :: Predicate a
                     } 

data Var a   = Var { var_name :: Symbol
                   , var_sort :: Sort 
                   , var_info :: a 
                   } 

type Ctor a  = Var a 

data Expr a  = EVar (Var a) 
             | EApp (Ctor a) [Expr a]

newtype Predicate a = Pred {p_pred :: Pred}

type Proof a     = [Instance a]

data Instance a  = Inst { inst_axiom :: Axiom a 
                        , inst_args  :: [Expr a]
                        }


data Query a = Query { q_axioms :: [Axiom a] 
                     , q_ctors  :: [Ctor a] 
                     , q_vars   :: [Var a] 
                     , q_goal   :: Predicate a
                     } 

instance Monoid (Predicate a) where
    mempty                      = Pred mempty 
    mappend (Pred p1) (Pred p2) = Pred (p1 `mappend` p2)

instance Monoid (Query a) where
    mempty        = Query { q_axioms = mempty
                          , q_ctors  = mempty
                          , q_vars   = mempty
                          , q_goal   = mempty
                          }
    mappend q1 q2 = Query { q_axioms = q_axioms q1 `mappend` q_axioms q2
                          , q_ctors  = q_ctors  q1 `mappend` q_ctors  q2 
                          , q_vars   = q_vars   q1 `mappend` q_vars   q2 
                          , q_goal   = q_goal   q1 `mappend` q_goal   q2 
                          }


instance F.Subable (Predicate a) where
  subst su (Pred p) = Pred $ subst su p 
  substa su (Pred p) = Pred $ substa su p 
  substf su (Pred p) = Pred $ substf su p 
  syms (Pred p)     = syms p 

mkExpr :: Expr a -> F.Expr
mkExpr (EVar v)    = F.EVar (var_name v)
mkExpr (EApp c es) = F.EApp (F.dummyLoc $ var_name c) (mkExpr <$> es)


