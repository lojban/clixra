module Update where

import Network.Curl
import Text.Regex
import Data.Maybe
import IO
import Text.HTML.TagSoup
import Data.List
import Control.Concurrent

update :: IO ()
update = do
  (code,body) <- curlGetString url []
  if code == CurlOK 
     then case entry $ parseTags body of
            Just entry' -> do
              h <- openFile "gismu.db" WriteMode
              hPutStr h $ show $ gismus entry'
              hFlush h
            _           -> return ()
     else return ()
    where gismus = map pair . mapMaybe (matchRegex regex) . lines
          pair [a,b] = (a,b,"") -- Intentional empty image URL entry
          regex = mkRegex "^(Image:.*)\\|\\[\\[(.*)\\]\\]$"
          url = "http://jbo.wikipedia.org/w/index.php?\
                \title=pixra_liste_loi_gismu&action=edit"

entry tags = text where
    text = case ts of
             Just (_:TagText t:_) -> Just t
             _                    -> Nothing
    ts = find textarea $ tails tags
    textarea ((TagOpen "textarea" _):(TagText _):_) = True
    textarea _ = False
