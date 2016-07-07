{-# LANGUAGE ExistentialQuantification #-}
module Evaluator.Evaluator where

import System.IO
import Data.IORef
import Text.Parsec hiding ( spaces )
import Text.ParserCombinators.Parsec.Error -- for ParseError
import SimpleParser.SimpleParser
import Control.Monad.Error --deprecated but following the book :/

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
  if num params /= num args && varargs == Nothing
     then throwError $ NumArgs (num params) args
          else (liftIO . bindVars closure $ zip params args)
               >>= bindVarArgs varargs
               >>= evalBody
  where
    remainingArgs = drop (length params) args
    num = toInteger . length
    evalBody env = liftM last $ mapM (eval env) body
    bindVarArgs arg env = case arg of
      Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
      Nothing -> return env
apply (IOFunc func) args = func args

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

instance Error LispError where
  noMsg = Default "and error has occured"
  strMsg = Default

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right a) = a --purposefully leave Left to fail, so we fail fast

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err  -> throwError $ Parser err
    Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)

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

-------------------- Begin Section Weak Typing and heterogeneous lists ---------
data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpacksEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpacksEquals arg1 arg2 (AnyUnpacker f) =
  do unpacked1 <- f arg1
     unpacked2 <- f arg2
     return $ unpacked1 == unpacked2
     `catchError` (const $ return False)

--------------------------- Exercise 5.1 ---------------------------------------
eval :: Env -> LispVal -> IOThrowsError LispVal
--so much repition, why not use _ as catch all?
eval env val@(String _) = return val
eval env val@(Character _) = return val
eval env val@(Rational _) = return val
eval env val@(Vector _) = return val
eval env val@(Float _) = return val
eval env val@(Complex _) = return val
eval env val@(Atom id) = getVar env id
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) =
  do
    result <- eval env pred
    case result of
      Bool False -> eval env alt
      Bool True -> eval env conseq
      otherwise -> throwError $ TypeMismatch "bool" pred
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var:params) varargs:body)) =
  makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params:body)) =
  makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
  makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
  makeVarArgs varargs env [] body
eval env (List [Atom "load", String filename]) = load filename
  >>= liftM last . mapM (eval env)
eval env (List (function : args)) =
  do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
eval env form@(List (Atom "cond" : clauses)) =
  if null clauses
     then throwError $ BadSpecialForm "no true clause in cond expression: " form
     else case head clauses of
      List [Atom "else", expr] -> eval env expr
      List [test, expr] -> eval env $ List [Atom "if"
                                       , test
                                       , expr
                                       , List (Atom "cond" : tail clauses)]
      _ -> throwError $ BadSpecialForm "ill-formed cond expression: " form
eval _ badform = throwError $ BadSpecialForm "Unrecognized special Form" badform

--------------------------- Exercise 5.2 ---------------------------------------
eqvList :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
eqvList eqvFunc [(List arg1), (List arg2)] = return . Bool $ (length arg1 ==
                                                             length arg2) &&
  (all eqvPair $ zip arg1 arg2)
  where eqvPair (x, y) = case eqvFunc [x, y] of
          Left err -> False
          Right (Bool val) -> val

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool arg1, Bool arg2] = return $ Bool (arg1 == arg2)
eqv [Number arg1, Number arg2] = return $ Bool (arg1 == arg2)
eqv [String arg1, String arg2] = return $ Bool (arg1 == arg2)
eqv [l@(List arg1), k@(List arg2)] = eqvList equal [l, k]
eqv [DottedList as a, DottedList bs b] = eqv [List $ as ++ [a], List $ bs ++ [b]]
eqv [List arg1, List arg2] = return . Bool $ (length arg1 == length arg2)
  && (all eqvPair $ zip arg1 arg2)
  where
    eqvPair (x, y) = case eqv [x, y] of
      Left err -> False
      Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

equal :: [LispVal] -> ThrowsError LispVal
equal [l@(List arg1), k@(List arg2)] = eqvList equal [l, k]
equal [(DottedList xs x), (DottedList ys y)] = equal [List $ xs ++ [x]
                                                     , List $ ys ++ [y]]
equal [arg1, arg2] =
  do primitiveEquals <- liftM or $ mapM (unpacksEquals arg1 arg2)
       [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
     eqvEquals <- eqv [arg1, arg2]
     return . Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

--------------------------- Exercise 5.3 ---------------------------------------
--see eval

--------------------- Begin Section on Variable Assignment ---------------------

nullEnv :: IO Env
nullEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>=
  return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var =
  do env <- liftIO $ readIORef envRef
     maybe (throwError $ UnboundVar "Getting an unbound variable" var)
       (liftIO . readIORef)
       (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value =
  do env <- liftIO $ readIORef envRef
     maybe (throwError $ UnboundVar "Setting an unbounded varaible" var)
       (liftIO . (flip writeIORef value))
       (lookup var env)
     return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value =
  do alreadyDefined <- liftIO $ isBound envRef var
     if alreadyDefined
       then setVar envRef var value >> return value
       else liftIO $
            do valueRef <- newIORef value
               env <- readIORef envRef
               writeIORef envRef ((var, valueRef) : env)
               return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
        addBinding (var, value) =
          do ref <- newIORef value
             return (var, ref)

primitiveBindings :: IO Env
primitiveBindings = nullEnv
  >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
       ++ map (makeFunc PrimitiveFunc) primitives)
  where
    makeFunc constructor (var, func) = (var, constructor func)

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc)
               , ("open-input-file", makePort ReadMode)
               , ("open-output-file", makePort WriteMode)
               , ("close-input-port", closePort)
               , ("close-output-port", closePort)
               , ("read", readProc)
               , ("write", writeProc)
               , ("read-contents", readContents)
               , ("read-all", readAll)]
         
makeFunc varargs env params body = return $
  Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func:args) = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port . liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String . liftIO . readFile

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

