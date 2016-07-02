#!/usr/bin/env stack
-- stack --resolver lts-3.2 --install-ghc runghc --package myLittleScheme
module Main where

import SimpleParser.SimpleParser
import Evaluator.Evaluator
import System.Environment

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
