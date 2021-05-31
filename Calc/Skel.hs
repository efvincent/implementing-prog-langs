-- Haskell module generated by the BNF converter

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Calc.Skel where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified Calc.Abs

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transExp :: Calc.Abs.Exp -> Result
transExp x = case x of
  Calc.Abs.EAdd exp1 exp2 -> failure x
  Calc.Abs.ESub exp1 exp2 -> failure x
  Calc.Abs.EMul exp1 exp2 -> failure x
  Calc.Abs.EDiv exp1 exp2 -> failure x
  Calc.Abs.EInt integer -> failure x