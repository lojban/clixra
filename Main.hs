module Main where

import Clixra

import Network.FastCGI
import Control.Concurrent

main :: IO ()
main = runFastCGIConcurrent' forkIO 100 (handleErrors clixra)
 