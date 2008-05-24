module Main where

import Clixra
import ClixraMonad

import Network.FastCGI
import Control.Concurrent
import Control.Monad
import Control.Monad.State

main :: IO ()
main = do
  v <- newMVar ()
  runFastCGIConcurrent' forkIO 100 (handleErrors $ runClixra clixra v)
