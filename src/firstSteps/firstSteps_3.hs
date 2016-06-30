module Main where
import System.Environment

main :: IO ()
main = do
  (a:b:_)<- getArgs
  let num1 = read a :: Integer
  let num2 = read b :: Integer
  let val = num1 + num2
  putStrLn "What else?"
  e <- getLine
  putStrLn ("Wonderful " ++ e)
