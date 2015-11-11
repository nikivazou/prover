module Prover.Solve where

import Prover.Types
import Prover.Pretty ()

import Language.Fixpoint.Sort
import Language.Fixpoint.PrettyPrint
import qualified Language.Fixpoint.Types as F 

import Data.List  (nubBy)
import Data.Maybe (isJust)

import Debug.Trace

solve :: Query a -> Proof a
solve q = trace (show q ++ "\n\nSorts = \n\n" ++ showpp (pprint sorts) ++ "\n\nES = \n\n" ++ 
                   showpp (pprint es) ++ 
                   "\n\nINSTANCES\n\n" ++ showpp (pprint is) 
                 ++ "\n\n") [] 
  where 
    sorts = makeSorts q
    es    = [(s, EVar <$> filter (unifiable s . var_sort) (q_vars q)) | s <- sorts]
    is    = concatMap (`instantiate` es) (q_axioms q)

instantiate :: Axiom a -> [(F.Sort, [Expr a])] -> [Predicate a]
instantiate a ses = if any null ess then [] else axiomApply a <$> go [] (reverse $ ess)
    where
        sorts = var_sort <$> axiom_vars a 
        ess   = snd <$> head ((\s' -> (filter (unifiable s' . fst) ses)) <$> sorts)

        go acc (es:ess) = go (combine acc es) ess 
        go acc []       = acc 


        combine []  es     = map (\e -> [e]) es  
        combine _   []     = []
        combine acc (e:es) = (map (e:) acc) ++ combine acc es

axiomApply :: Axiom a -> [Expr a] -> Predicate a 
axiomApply a es 
  = traceShow ("\n\nmkApp\n\n" ++ show a ++ "\n\nWITH\n\n" ++ show es ++ "\n\n") $ 
       F.subst (F.mkSubst $ zip (var_name <$> (axiom_vars a)) (mkExpr <$> es)) (axiom_body a)


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


