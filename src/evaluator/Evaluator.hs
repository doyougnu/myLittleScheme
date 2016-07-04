module Evaluator.Evaluator where

import SimpleParser.SimpleParser
import Text.Parsec hiding ( spaces )
import Control.Monad.Error --deprecated but following the book :/
import Text.ParserCombinators.Parsec.Error -- for ParseError 

eval :: LispVal -> ThrowsError LispVal
--so much repition, why not use _ as catch all?
eval val@(String _) = return val
eval val@(Character _) = return val
eval val@(Rational _) = return val
eval val@(Vector _) = return val
eval val@(Float _) = return val
eval val@(Complex _) = return val
eval val@(Atom _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom f:args)) = mapM eval args >>= apply f
eval badform = throwError $ BadSpecialForm "Unrecognized special Form" badform

apply :: String -> [LispVal] -> ThrowsError LispVal
-- given a string, lookup that string in primitive
-- if you get a match primitives returns the right function
-- apply that function to the arguements
-- else return book false, the behaviour of maybe dictates the whole thing
apply f args = maybe (throwError $ NotFunction
                      "Unrecognized primitive function args" f)
  ($ args) (lookup f primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]  
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

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal]
             -> ThrowsError LispVal 
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleval@[_] = throwError $ NumArgs 2 singleval
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
  if null parsed
     then throwError $ TypeMismatch "number" $ String n
          else return . fst . head $ parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

--------------------------- Exercise 4.1.2.3 ------------------------------------
-- given the exercises its hard to tell what is actually being asked for
unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [values] = return $ f values

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

--------------------------- Begin section on Error handling ---------------------
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where show = showError

showError :: LispError -> String
showError (NumArgs expected found) = "Expected: " ++ show expected ++
  " args but found: " ++ unwordsList found
showError (TypeMismatch expected found) = "Type mismatch; Got: " ++
  show found ++ " but Expected: " ++ expected
showError (Parser error) = "Parse error at: " ++ show error
showError (UnboundVar message var) = message ++ " : " ++ show var
showError (BadSpecialForm message val) = message ++ " : " ++ show val 
showError (NotFunction message func) = message ++ " : " ++ show func

instance Error LispError where
  noMsg = Default "and error has occured"
  strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right a) = a --purposefully leave Left to fail, so we fail fast

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right value -> return value
