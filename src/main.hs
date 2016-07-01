module Main where

import SimpleParser
import Evaluator

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
