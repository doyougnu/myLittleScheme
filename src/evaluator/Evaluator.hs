{-# LANGUAGE ExistentialQuantification #-}
module Evaluator.Evaluator where
import SimpleParser.SimpleParser
import Text.Parsec hiding ( spaces )
import Control.Monad.Error --deprecated but following the book :/
import Text.ParserCombinators.Parsec.Error -- for ParseError

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
             , ("bool?", unaryOp boolp)
             , ("=", numBoolBinop (==))
             , ("<", numBoolBinop (<))
             , (">", numBoolBinop (>))
             , ("<=", numBoolBinop (<=))
             , ("=>", numBoolBinop (>=))
             , ("/=", numBoolBinop (/=))
             , ("&&", boolBoolBinop (&&))
             , ("||", boolBoolBinop (||))
             , ("string=?", strBoolBinop (==))
             , ("string<?", strBoolBinop (<))
             , ("string>?", strBoolBinop (>))
             , ("string<=?", strBoolBinop (<=))
             , ("string>=?", strBoolBinop (>=))
             , ("car", car)
             , ("cdr", cdr)
             , ("cons", cons)
             , ("eq?", eqv)
             , ("eqv?", eqv)
             , ("equal?", equal)]

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal]
             -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ head args
                                     right <- unpacker $ args !! 1
                                     return . Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number n) = return $ show n
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "bool" notBool

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

--------------------------- List Ops -------------------------------------------
car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_:xs) ys] = return $ DottedList xs ys
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x , List xs] = return $ List (x : xs)
cons [x, DottedList ys zs] = return $ DottedList (x:ys) zs
cons [x, y] = return $ DottedList [x] y
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool arg1, Bool arg2] = return $ Bool (arg1 == arg2)
eqv [Number arg1, Number arg2] = return $ Bool (arg1 == arg2)
eqv [String arg1, String arg2] = return $ Bool (arg1 == arg2)
eqv [DottedList as a, DottedList bs b] = eqv [List $ as ++ [a], List $ bs ++ [b]]
eqv [List arg1, List arg2] = return . Bool $ (length arg1 == length arg2)
  && (all eqvPair $ zip arg1 arg2)
  where
    eqvPair (x, y) = case eqv [x, y] of
      Left err -> False
      Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

-------------------- Begin Section Weak Typing and heterogeneous lists ---------
data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpacksEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpacksEquals arg1 arg2 (AnyUnpacker f) =
  do unpacked1 <- f arg1
     unpacked2 <- f arg2
     return $ unpacked1 == unpacked2
     `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] =
  do primitiveEquals <- liftM or $ mapM (unpacksEquals arg1 arg2)
       [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
     eqvEquals <- eqv [arg1, arg2]
     return . Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

--------------------------- Exercise 5.1 ---------------------------------------
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
eval (List [Atom "if", pred, conseq, alt]) =
  do
    result <- eval pred
    case result of
      Bool False -> eval alt
      Bool True -> eval conseq
      otherwise -> throwError $ TypeMismatch "bool" pred
eval (List (Atom f:args)) = mapM eval args >>= apply f
eval badform = throwError $ BadSpecialForm "Unrecognized special Form" badform
