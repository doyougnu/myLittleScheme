#!/usr/bin/env stack
-- stack --resolver lts-3.2 --install-ghc runghc --package myLittleScheme
module Main where

import SimpleParser.SimpleParser
import Evaluator.Evaluator
import System.Environment

main :: IO ()
main =
  do
    args <- getArgs
    --the evaled line is awful, besides the antipattern of !! 0, the >>= has
    --high precedence than $, so you the data in the statement changes directions
    evaled <- return $ fmap show $ readExpr (args !! 0) >>= eval
    putStrLn . extractValue . trapError $ evaled
