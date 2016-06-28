import Text.ParserCombinators.Parsec hiding ( spaces )
import System.Environment
import Control.Monad

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

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

specials :: Parser Char
specials =
  do
    s <- oneOf "\\\"nrt" --this is using haskell escaped backslash and quote
    return $ case s of
      'n' -> '\n'
      't' -> '\t'
      'r' -> '\r'
      '\\' -> s
      '"' -> s
    
data LispVal = Atom String
               | List [LispVal]
               | DottedList [LispVal] LispVal
               | Number Integer
               | String String
               | Bool Bool 

literals :: Parser Char
literals = letter <|> digit <|> symbol <|> specials
  
parseString :: Parser LispVal
parseString =
  do
    char '"'
    x <- many $ specials <|> noneOf "\\\""
    char '"'
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

parseNumber :: Parser LispVal
parseNumber = liftM (Number. read) $ many1 digit

--------------------------- Exercise 2.1 ----------------------------------------
parseNumber' :: Parser LispVal
parseNumber' =
  do
    s <- many1 digit
    return (Number . read $ s)

--------------------------- Exercise 2.2 ----------------------------------------
parseNumber'' :: Parser LispVal
parseNumber'' = many1 digit >>= return . Number. read

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber
