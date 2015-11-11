module Prover.Solve where

import Prover.Types
import Prover.SMTInterface
import Prover.Pretty ()

import Prover.Misc (findM, powerset, combine2)

import Language.Fixpoint.Smt.Interface (Context)

-- import Language.Fixpoint.Misc
import Language.Fixpoint.Sort
import qualified Language.Fixpoint.Types as F 

import Data.List  (nubBy)
import Data.Maybe (isJust)

solve :: Query a -> IO (Proof a)
solve q = 
  do cxt <- makeContext (q_fname q ++ ".smt" ) env
     iterativeSolve 2 cxt es (p_pred $ q_goal q) (q_axioms q)
  where 
    sorts = makeSorts q
    es    = initExpressions (q_vars q) (q_ctors q) sorts 
    env   = [ (var_name v, var_sort v) | v <- (q_ctors q ++ q_vars q)]


iterativeSolve :: Int -> Context -> [ArgExpr a] -> F.Pred -> [Axiom a] -> IO (Proof a)
iterativeSolve iter cxt es q axioms = go [] ((\e -> e{arg_exprs = []}) <$> es) es 0 
  where 
    go _  _      _  i | i == iter = return Invalid 
    go as old_es es i = do b   <- checkValid cxt (p_pred . inst_pred <$> is) q  
                           if b then Proof <$> minize cxt is q
                                else go is (zipWith appendExprs es old_es) new_es (i+1)
                        where 
                         is     = concatMap (instantiate old_es es) axioms ++ as 
                         new_es = makeExpressions es
                         appendExprs ae1 ae2 = ae1 {arg_exprs = (arg_exprs ae1) ++ (arg_exprs ae2)}


makeExpressions :: [ArgExpr a] -> [ArgExpr a]
makeExpressions _ = [] -- NV HERE!!!!

initExpressions :: [Var a] -> [Ctor a] -> [F.Sort] -> [ArgExpr a]
initExpressions vs ctors sorts 
  = [ArgExpr { arg_sort  = s
             , arg_exprs = EVar <$> filter (unifiable s . var_sort) vs
             , arg_ctors = filter (unifiable s . var_sort) ctors
             } | s <- sorts]

minize :: Context -> [Instance a] -> F.Pred -> IO [Instance a]
minize cxt ps q 
  = findM (\is -> checkValid cxt (p_pred . inst_pred <$> is) q) (powerset ps)

instantiate :: [ArgExpr a] -> [ArgExpr a] -> Axiom a -> [Instance a]
instantiate oldses ses a = if any null ess then [] else axiomInstance a <$> combine2 oess ess
    where
        sorts = var_sort <$> axiom_vars a 
        ess   = arg_exprs . head <$> ((\s' -> (filter (unifiable s' . arg_sort) ses))    <$> sorts)
        oess  = arg_exprs . head <$> ((\s' -> (filter (unifiable s' . arg_sort) oldses)) <$> sorts)

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

resultSort :: F.Sort -> F.Sort 
resultSort (F.FFunc _ ss) = last ss
resultSort s              = s

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


