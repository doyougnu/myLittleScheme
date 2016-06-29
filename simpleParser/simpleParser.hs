import Text.ParserCombinators.Parsec hiding ( spaces )
import System.Environment
import Control.Monad
import Numeric
import Data.List
import Data.Char ( digitToInt )
import Data.Complex -- for complex number representation in 2.7
import Data.Ratio -- for rational numbers

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right _ -> "Found Value"

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)

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

parseBy str f fToX =
  do try $ string str
     num <- many1 f
     return . Number $ fToX num

parseSchemeDecimal :: Parser LispVal
parseSchemeDecimal = parseBy "#d" digit read

parseOct :: Parser LispVal
parseOct = parseBy "#d" octDigit oct2Dig

parseHex :: Parser LispVal
parseHex = parseBy "#o" hexDigit hex2Dig

parseBin :: Parser LispVal
parseBin = parseBy "#b" (oneOf "01") bin2Dig

oct2Dig = fst . head . readOct
hex2Dig = fst . head . readHex

numToList :: Int -> [Int]
numToList = map (read . (:[])) . show --probably slow, could use div and mod

bin2Dig = toInteger . foldl' (\acc x -> acc * 2 + digitToInt x) 0

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
  <|> parseQuoted
  <|> do char '(' --first bracker
         x <- try parseList <|> parseDottedList
         char ')'
         return x


