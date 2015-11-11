module Prover.Solve where

import Prover.Types
import Prover.SMTInterface
import Prover.Pretty ()

import Prover.Misc (findM, powerset)

import Language.Fixpoint.Smt.Interface (Context)

-- import Language.Fixpoint.Misc
import Language.Fixpoint.Sort
import qualified Language.Fixpoint.Types as F 

import Data.List  (nubBy)
import Data.Maybe (isJust)

solve :: Query a -> IO (Proof a)
solve q = 
  do cxt <- makeContext (q_fname q ++ ".smt" ) env
     b   <- checkValid cxt (p_pred . inst_pred <$> is) (p_pred $ q_goal q)  
     if b then Proof <$> minize cxt is (q_goal q) else return Invalid  
  where 
    sorts = makeSorts q
    es    = [(s, EVar <$> filter (unifiable s . var_sort) (q_vars q)) | s <- sorts]
    is    = concatMap (`instantiate` es) (q_axioms q)
    env   = [ (var_name v, var_sort v) | v <- (q_ctors q ++ q_vars q)]

minize :: Context -> [Instance a] -> Predicate a -> IO [Instance a]
minize cxt ps q 
  = findM (\is -> checkValid cxt (p_pred . inst_pred <$> is) q') (powerset ps)
  where
    q'  = p_pred q

instantiate :: Axiom a -> [(F.Sort, [Expr a])] -> [Instance a]
instantiate a ses = if any null ess then [] else axiomInstance a <$> go [] (reverse $ ess)
    where
        sorts = var_sort <$> axiom_vars a 
        ess   = snd . head <$> ((\s' -> (filter (unifiable s' . fst) ses)) <$> sorts)

        go acc (es:ess) = go (combine acc es) ess 
        go acc []       = acc 


        combine []  es     = map (\e -> [e]) es  
        combine _   []     = []
        combine acc (e:es) = (map (e:) acc) ++ combine acc es



axiomInstance :: Axiom a -> [Expr a] -> Instance a 
axiomInstance a es 
  = Inst { inst_axiom = a
         , inst_args  = es
         , inst_pred  = F.subst (F.mkSubst $ zip (var_name <$> (axiom_vars a)) (mkExpr <$> es)) (axiom_body a)
         }


makeSorts :: Query a -> [F.Sort]
makeSorts q = nubBy unifiable (asorts ++ csorts)
  where 
     asorts = var_sort <$> (concatMap axiom_vars $ q_axioms q)
     csorts = concatMap argumentsort (var_sort <$> q_ctors q)


-- | Manipulationg Sorts 

argumentsort :: F.Sort -> [F.Sort]
argumentsort (F.FFunc _ ss) = init ss
argumentsort _            = []

unifiable :: F.Sort -> F.Sort -> Bool
unifiable (F.FVar _) (F.FVar _) = True 
unifiable (F.FVar _) (F.FObj _) = True 
unifiable (F.FObj _) (F.FVar _) = True 
unifiable (F.FVar _) _          = False 
unifiable _          (F.FVar _) = False 
unifiable t1 t2 = isJust $ unify t1 t2


