module Prover.Solve where

import Prover.Types
import Prover.Pretty ()

import Debug.Trace

solve :: Query a -> Proof a
solve q = trace (show q) [] 