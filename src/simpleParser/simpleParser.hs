module SimpleParser.SimpleParser where

import Text.Parsec hiding ( spaces )
import Text.Parsec.String
import Control.Monad
import Numeric
import Data.Complex -- for complex number representation in 2.7
import Data.Ratio -- for rational numbers
import Data.Array -- could use haskell vectors instead, harder implementation
import Data.IORef
import Control.Monad.Error --deprecated but following the book :/

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
               | List [LispVal]
               | DottedList [LispVal] LispVal
               | Number Integer
               | String String
               | Bool Bool
               | Character Char
               | Float Double
               | Complex (Complex Double)
               | Rational Rational
               | Vector (Array Int LispVal)
               | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
               | Func { params :: [String], vararg :: Maybe String
                      , body :: [LispVal], closure :: Env}

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (NumArgs expected found) = "Expected: " ++ show expected ++
  " args but found: " ++ unwordsList found
showError (TypeMismatch expected found) = "Type mismatch; Got: " ++
  show found ++ " but Expected: " ++ expected
showError (Parser error) = "Parse error at: " ++ show error
showError (UnboundVar message var) = message ++ " : " ++ show var
showError (BadSpecialForm message val) = message ++ " : " ++ show val 
showError (NotFunction message func) = message ++ " : " ++ show func

instance Show LispError where show = showError
type ThrowsError = Either LispError
type Env = IORef [(String, IORef LispVal)]

parseAtom :: Parser LispVal
parseAtom =
  do
    first <- letter <|> symbol -- <|> is parsec choice operator
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
      "#t" -> Bool True
      "#f" -> Bool False
      _    -> Atom atom

--------------------------- Exercise 2.1 ----------------------------------------
parseNumber' :: Parser LispVal
parseNumber' =
  do
    s <- many1 digit
    return (Number . read $ s)

--------------------------- Exercise 2.2 ----------------------------------------
parseNumber'' :: Parser LispVal
parseNumber'' = many1 digit >>= return . Number. read

--------------------------- Exercise 2.3 ----------------------------------------
specials :: Parser Char
specials = do char '\\'
              x <- oneOf "\\\"nrt"
              return $ case x of
                '\\' -> x
                '"'  -> x
                'n'  -> '\n'
                'r'  -> '\r'
                't'  -> '\t'

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ specials <|> noneOf "\"\\"
                 char '"'
                 return $ String x
--------------------------- Exercise 2.4 ----------------------------------------
--Scheme defines Octal as #o, decimal as #d, hex as #h
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

-- so we can no longer parse bools with a prefixed '#'
parseBool :: Parser LispVal
parseBool =
  do
    char '#' --match a #
    (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseNumber :: Parser LispVal
parseNumber = parseDecimal
  <|> parseSchemeDecimal
  <|> parseHex
  <|> parseOct
  <|> parseBin

parseDecimal :: Parser LispVal
parseDecimal = many1 digit >>= return . Number . read

parseSchemeDecimal:: Parser LispVal
parseSchemeDecimal = do try $ string "#d"
                        x <- many1 digit
                        (return . Number . read) x

parseHex :: Parser LispVal
parseHex = do try $ string "#x"
              x <- many1 hexDigit
              return $ Number (hex2dig x)

parseOct :: Parser LispVal
parseOct = do try $ string "#o"
              x <- many1 octDigit
              return $ Number (oct2dig x)

parseBin :: Parser LispVal
parseBin = do try $ string "#b"
              x <- many1 (oneOf "10")
              return $ Number (bin2dig x)

numToList :: Int -> [Int]
numToList = map (read . (:[])) . show --probably slow, could use div and mod

oct2dig x = fst $ readOct x !! 0
hex2dig x = fst $ readHex x !! 0
bin2dig  = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in
                         bin2dig' old xs
--------------------------- Exercise 2.5 ----------------------------------------
parseCharacter :: Parser LispVal
--parseChar
             --try to parse a whole match on newline or space
             --or try to match any character not followed by an alpha numeric
             --return thst as a list
             -- now try to match newline or space return the corresponding value
             --if not a match then return the first character match by previous
             -- do
parseCharacter =
  do
    try $ string "#\\"
    x <- try (string "newline" <|> string "space")
         <|> do {str <- anyChar; notFollowedBy alphaNum; return [str]}
    return . Character $ case x of
      "newline" -> '\n'
      "space" -> ' '
      otherwise -> (x !! 0) --use of unsafe head

--------------------------- Exercise 2.6 ----------------------------------------
-- I keep struggling to find the appropriate informatin to match on in the
-- R5RS, I thought I had to implement exact/nonexact in addition to this...
parseFloat :: Parser LispVal
parseFloat =
  do
    x <- many1 digit --many  digits before a '.'
    char '.' -- match on the ','
    y <- many1 digit --more digitts after
    return $ Float . fst . head . readFloat $ (x ++ y) -- readFloat is a zipper

--------------------------- Exercise 2.7 ----------------------------------------
toDouble :: LispVal -> Double
toDouble (Float f) = realToFrac f
toDouble (Number n) = fromInteger n

parseComplexNumbers :: Parser LispVal
parseComplexNumbers =
  do
    a <- parseFloat <|> parseNumber
    char '+' --complex nums have pattern a+bi
    b <- parseFloat <|> parseNumber
    char 'i'
    return (Complex (toDouble a :+ toDouble b))

parseRationalNumbers :: Parser LispVal
parseRationalNumbers =
  do
    a <- many1 digit-- a rational num is denoted by a / b /= 0
    char '/'
    b <- many1 digit
    return (Rational ((read a) % (read b))) --didn't know that (%) existed

------------------------Begin section on recursive parsers-----------------------
parseList :: Parser LispVal
--sepBy parseExpr spaces seperates a string by spaces on parseExpr criteria
parseList = liftM List $ sepBy parseExpr spaces 

parseDottedList :: Parser LispVal
parseDottedList =
  do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted =
  do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
  <|> parseString
  <|> try parseBool
  <|> try parseComplexNumbers
  <|> try parseRationalNumbers
  <|> parseNumber --try's are required because these start with a '#'
  <|> parseCharacter
  <|> parseQuasiQuoted
  <|> parseUnQuote
  <|> parseQuoted
  <|> try parseVector'
  <|> do char '(' --first bracker
         x <- try parseList <|> parseDottedList
         char ')'
         return x
--------------------------- Exercise 4.1 ----------------------------------------
parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted =
  do
    char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote =
  do
    char ','
    x <- parseExpr
    return $ List [Atom "unquote", x]

--------------------------- Exercise 4.2 ----------------------------------------
parseVector :: Parser LispVal
parseVector =
  do
    values <- sepBy parseExpr spaces
    return $ Vector (listArray (0, (length values) - 1) values)

parseVector' :: Parser LispVal
parseVector' =
  do string "#("
     vector <- parseVector
     char ')'
     return vector

--------------------------- Exercise 4.3 ----------------------------------------
     --skip

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Float num) = show num
showVal (Complex num) = show num
showVal (Rational num) = show num
showVal (Vector arr) = unlines . map showVal . elems $ arr
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List values) = "(" ++ unwordsList values ++ ")"
showVal (DottedList h t) = "(" ++ unwordsList h ++ " . " ++ showVal t
  ++ ")"
showVal (Character c) = [c]
showVal (PrimitiveFunc _) = "primitive"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
  "(lambda (" ++ unwords (map show args) ++
  (case varargs of
      Nothing -> ""
      Just arg -> " . " ++ arg) ++ ") ...)"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal
