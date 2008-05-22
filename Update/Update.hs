module Update where

import Network.Curl
import Text.Regex
import Data.Maybe
import IO

update :: IO ()
update = do
  (code,body) <- curlGetString url []
  if code == CurlOK 
     then do
       h <- openFile "gismu.db" WriteMode
       hPutStr h $ show $ gismuList body
       hFlush h
     else return ()
    where gismuList = map pair . mapMaybe (matchRegex regex) . lines
          pair [a,b] = (a,b,"") -- Intentional empty image URL entry
          regex = mkRegex "^(Image:.*)\\|\\[\\[(.*)\\]\\]$"
          url = "http://jbo.wikipedia.org/w/index.php?\
                \title=pixra_liste_loi_gismu&action=edit"
