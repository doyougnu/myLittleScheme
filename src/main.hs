#!/usr/bin/env stack
-- stack --resolver lts-3.2 --install-ghc runghc --package myLittleScheme
module Main where

import System.Environment
import Repl.Repl

main :: IO ()
main =
  do args <- getArgs
     if null args then runRepl else runOne args
