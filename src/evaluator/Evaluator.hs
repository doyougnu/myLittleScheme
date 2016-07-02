module Evaluator.Evaluator where

import SimpleParser.SimpleParser

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Character _) = val
eval val@(Rational _) = val
eval val@(Vector _) = val
eval val@(Float _) = val
eval val@(Complex _) = val
eval val@(Atom _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval x = x
