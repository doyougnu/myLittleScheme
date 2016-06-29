import Text.ParserCombinators.Parsec hiding ( spaces )
import System.Environment
import Control.Monad
import Numeric
import Data.List
import Data.Char ( digitToInt )

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

parseString :: Parser LispVal
parseString = do
                 x <- many $ specials <|> noneOf "\"\\"
                 return $ String x

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
parseNumber = parseDecimal <|> parseSchemeDecimal <|> parseHex <|> parseOct <|>
  parseBin

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
    x <- try (string "newline" <|> string "space") <|> (do
                                                           str <- anyChar
                                                           notFollowedBy alphaNum
                                                           return [str])
    return . Character $ case x of
      "newline" -> '\n'
      "space" -> ' '
      otherwise -> head x --use of unsafe head

--now add parseBool to parse expression
--2.5 add parseChar to parse expression
parseExpr :: Parser LispVal
parseExpr = parseAtom
  <|> parseString
  <|> try parseFloat -- try to get a float before assuming an Int, Hex, Bin, Oct
  <|> try parseNumber --try's are required because these start with a '#'
  <|> try parseCharacter --had to get that from the solutions
  <|> try parseBool

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