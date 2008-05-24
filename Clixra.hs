module Clixra where

import Update
import View
import ClixraMonad

import Network.CGI
import Control.Concurrent

clixra :: Clixra CGIResult
clixra = do
  page <- getInput "page"
  case page of
    Just "update" -> updatePage
    _ -> view

