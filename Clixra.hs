module Clixra where

import Update
import View

import Network.CGI

clixra :: CGI CGIResult
clixra = do
  page <- getInput "page"
  case page of
    Just "update" -> do liftIO update
                        output "Updated."
    _ -> view
 