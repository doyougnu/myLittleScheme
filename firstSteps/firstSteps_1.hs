module Main where
import System.Environment

main :: IO ()
main = do
  (a:b:_)<- getArgs
  putStrLn "The args are: "
  mapM_ putStrLn ([a] ++ [b])
  putStrLn "done"
