module Prover.Parser where

import Prover.Types 

parseQuery :: String -> IO (Query a)
parseQuery _ = return mempty