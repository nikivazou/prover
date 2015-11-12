module Prover.Solve where

import Prover.Types
import Prover.SMTInterface
import Prover.Pretty ()
import Prover.Constants 

import Prover.Misc (findM, powerset, combine2, combine)

import Language.Fixpoint.Smt.Interface (Context)

import Language.Fixpoint.Sort
import qualified Language.Fixpoint.Types as F 

import Data.List  (nubBy)
import Data.Maybe (isJust, fromJust)

import Control.Monad (filterM)

solve :: Query a -> IO (Proof a)
solve q = 
  do cxt <- makeContext (smtFile $ q_fname q) env
     iterativeSolve (q_depth q) cxt es (p_pred $ q_goal q) (q_axioms q)
  where 
    sorts = makeSorts q
    es    = initExpressions (q_vars q) (q_ctors q) sorts 
    env   = [ (var_name v, var_sort v) | v <- ((ctor_var <$> q_ctors q) ++ q_vars q)]


iterativeSolve :: Int -> Context -> [ArgExpr a] -> F.Pred -> [Axiom a] -> IO (Proof a)
iterativeSolve iter cxt es q axioms = go [] ((\e -> e{arg_exprs = []}) <$> es) 0 es
  where 
    go _  _      i _  | i == iter = return Invalid 
    go as old_es i es = do prf   <- findValid cxt is q   
                           if isJust prf 
                                then Proof <$> minimize cxt (fromJust prf) q
                                else makeExpressions cxt is es >>= mapM (assertExpressions cxt) >>=
                                     go is (zipWith appendExprs es old_es) (i+1) 
                        where 
                         is = concatMap (instantiate old_es es) axioms ++ as
                         appendExprs ae1 ae2 = ae1 { arg_exprs = (arg_exprs ae1) ++ (arg_exprs ae2)
                                                   , arg_ctors = (arg_ctors ae1) ++ (arg_ctors ae2) 
                                                   }



findValid :: Context -> [Instance a] -> F.Pred -> IO (Maybe [Instance a])
findValid cxt ps q 
  = (\b -> if b then Just ps else Nothing) <$> checkValid cxt (p_pred . inst_pred <$> ps) q

minimize :: Context -> [Instance a] -> F.Pred -> IO [Instance a]
minimize cxt ps q | length ps < epsilon = fromJust <$> bruteSearch cxt ps q 
minimize cxt ps q = go 1 [] ps 
  where
    n = length ps `div` delta
    go _ acc [] = if (length acc < length ps) then minimize cxt acc q else fromJust <$> bruteSearch cxt acc q  
    go i acc is = do let (ps1, ps2) = splitAt n is 
                     let as = p_pred . inst_pred <$> (acc ++ ps2)
                     res <- checkValid cxt as q
                     let msg = replicate 80 '*' -- show i ++ " / " ++ show delta 
                     putStrLn msg 
                     -- print ""
                     if res then go (i+1) acc          ps2 
                            else go (i+1) (acc ++ ps1) ps2 

bruteSearch :: Context -> [Instance a] -> F.Pred -> IO (Maybe [Instance a])
bruteSearch cxt ps q 
  = findM (\is -> checkValid cxt (p_pred . inst_pred <$> is) q) (powerset ps)

filterEquivalentExpressions :: Context -> [Instance a] -> (ArgExpr a, ArgExpr a) -> IO (ArgExpr a)
filterEquivalentExpressions cxt is (aeold, aenew) 
  = do es <- filterM f (arg_exprs aenew)
       return $ aenew{arg_exprs = es}
  where 
    f e = not <$> checkValid cxt (p_pred . inst_pred <$> is) (F.POr [F.PAtom F.Eq (mkExpr e) (mkExpr e') | e' <- (arg_exprs aeold)])


assertExpressions :: Context -> ArgExpr a -> IO (ArgExpr a)
assertExpressions cxt ae = (mapM go $ arg_exprs ae) >> return ae 
  where
    go (EVar _)    = return ()
    go (EApp c es) = do mapM go es 
                        assert cxt $ predCtor c (mkExpr <$> es)

    predCtor c es = let su = F.mkSubst $ zip (var_name <$> ctor_vars c) es
                    in F.subst su (p_pred $ ctor_prop c)


makeExpressions :: Context -> [Instance a] -> [ArgExpr a] -> IO [ArgExpr a]
makeExpressions cxt is es 
  = mapM (filterEquivalentExpressions cxt is) $ zip es 
           [ArgExpr { arg_sort  = s 
                    , arg_exprs = concatMap (instantiateCtor es) cs
                    , arg_ctors = cs
                    } | ArgExpr s _ cs <- es]

initExpressions :: [Var a] -> [Ctor a] -> [F.Sort] -> [ArgExpr a]
initExpressions vs ctors sorts 
  = [ArgExpr { arg_sort  = s
             , arg_exprs = EVar <$> filter (unifiable s . var_sort) vs
             , arg_ctors = filter (unifiable s . resultSort . var_sort . ctor_var) ctors
             } | s <- sorts]


instantiateCtor :: [ArgExpr a] -> Ctor a -> [Expr a]
instantiateCtor aes ctor = EApp ctor <$> combine ess 
  where
    ess = arg_exprs . head <$> ((\s' -> (filter (unifiable s' . arg_sort) aes)) <$> (argumentsort $ var_sort $ ctor_var ctor))


instantiate :: [ArgExpr a] -> [ArgExpr a] -> Axiom a -> [Instance a]
instantiate oldses ses a = axiomInstance a <$> combine2 oess ess
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
     csorts = concatMap argumentsort (var_sort . ctor_var <$> q_ctors q)


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


