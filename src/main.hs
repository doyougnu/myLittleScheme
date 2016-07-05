#!/usr/bin/env stack
-- stack --resolver lts-3.2 --install-ghc runghc --package myLittleScheme
module Main where

import System.Environment
import Repl.Repl

main :: IO ()
main =
  do
    args <- getArgs
    case length args of
      0 -> runRepl
      1 -> evalAndPrint . head $ args
      otherwise -> putStrLn "Program only takes 1 or 0 arguments"
