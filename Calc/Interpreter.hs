module Calc.Interpreter where

import Calc.Abs

eval :: Exp -> Integer
eval x = case x of 
  EAdd x y -> eval x + eval y
  ESub x y -> eval x - eval y
  EMul x y -> eval x * eval y
  EDiv x y -> eval x `div` eval y
  EInt x -> x