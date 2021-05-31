-- Haskell data types for the abstract syntax.
-- Generated by the BNF converter.

-- | The abstract syntax of language Calc.

module Calc.Abs where

import Prelude (Integer)
import qualified Prelude as C (Eq, Ord, Show, Read)

data Exp
    = EAdd Exp Exp
    | ESub Exp Exp
    | EMul Exp Exp
    | EDiv Exp Exp
    | EInt Integer
  deriving (C.Eq, C.Ord, C.Show, C.Read)

