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
eval (List (Atom f:args)) = apply f $ map eval args

apply :: String -> [LispVal] -> LispVal
-- given a string, lookup that string in primitive
-- if you get a match primitives returns the right function
-- apply that function to the arguements
-- else return book false, the behaviour of maybe dictates the whole thing
apply f args = maybe (Bool False) ($ args) $ lookup f primitives

primitives :: [(String, [LispVal] -> LispVal)]  
primitives = [("+", numericBinop (+))
             , ("-", numericBinop (-))
             , ("*", numericBinop (*))
             , ("/", numericBinop div)
             , ("mod", numericBinop mod)
             , ("quotient", numericBinop quot)
             , ("remainder", numericBinop rem)
             , ("string?", unaryOp stringp)
             , ("symbol?", unaryOp symbolp)
             , ("number?", unaryOp nump)
             , ("list?", unaryOp listp)
             , ("bool?", unaryOp boolp)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal 
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n

unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

--------------------------- Exercise 4.1.2.3 ------------------------------------
-- given the exercises its hard to tell what is actually being asked for
unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp f [values] = f values

listp, boolp, nump, symbolp, stringp :: LispVal -> LispVal
stringp (String _) = Bool True
stringp _ = Bool False
symbolp (Atom _) = Bool True
symbolp _ = Bool False
nump (Number _) = Bool True
nump _ = Bool False
boolp (Bool _) = Bool True
boolp _ = Bool False
listp (List _) = Bool True
listp (DottedList _ _) = Bool True
listp _ = Bool False

strTosym, symTostr :: LispVal -> LispVal
symTostr (Atom x) = String x
symTostr _ = String ""
strTosym (String x) = Atom x
strTosym _ = String ""
