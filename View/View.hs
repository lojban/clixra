module View where

import Network.CGI
import Text.XHtml.Strict
import System.Random
import Control.Monad
import Network.Curl
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Types
import Text.XML.HaXml.Xtract.Parse
import Data.Maybe
import Data.List
import Text.XHtml.Strict

view :: CGI CGIResult
view = output . showHtml =<< do
  (img,gismu) <- randImage
  url <- imageLookup img
  case url of
    Just url -> return $ h1 << gismu +++ p << image ! [src url]
    Nothing  -> return $ p << ("Couldn't get image address for " ++ img)

imageLookup :: String -> CGI (Maybe String)
imageLookup img = do
  (code,body) <- liftIO $ curlGetString url []
  if code == CurlOK
     then return $ getUrl body
     else return Nothing
  where url = "http://commons.wikimedia.org/w/api.php?action=query&titles="
              ++ img ++
              "&prop=imageinfo&iiprop=url&&iiurlwidth=500&format=xml"

getUrl :: String -> Maybe String
getUrl xml = url' where
    url' = case url of
             Just (_,AttValue [Left u]) -> Just u
             _ -> Nothing
    url = find ((=="thumburl") . fst) attrs
    CElem (Elem _ attrs _) = xtract "//ii[@thumburl]" cont !! 0
    cont = CElem root    
    (Document _ _ root _) = xmlParse "Wikipedia API" xml

randImage :: CGI (String,String)
randImage = liftIO $ do
  db <- readFile "gismu.db"
  let entries = read db :: [(String,String,String)]
  r <- getStdRandom $ randomR (0,length entries-1)
  let (img,gismu,_) = entries !! r
  return $ (urlEncode img,gismu)
