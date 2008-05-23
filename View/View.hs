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

type DB = [(String,String,String)]

view :: CGI CGIResult
view = output . showHtml =<< do
  db <- liftIO $ gismuDB
  (img,gismu) <- randImage db
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

repSpaces :: String -> String
repSpaces = id

getUrl :: String -> Maybe String
getUrl xml =
    case extract xml of
      [CElem (Elem _ a _)] -> innerUrl $ find ((=="thumburl") . fst) a
      _                    -> Nothing

innerUrl elem = 
    case elem of
      Just (_,AttValue [Left u]) -> Just u
      _                          -> Nothing

extract xml = xtract "//ii[@thumburl]" cont where
    cont = CElem root    
    (Document _ _ root _) = xmlParse "Wikipedia API" xml    

randImage :: DB -> CGI (String,String)
randImage db = liftIO $ do
  r <- getStdRandom $ randomR (0, length db - 1)
  let (img,gismu,_) = db !! r
  return $ (urlEncode img,gismu)

gismuDB :: IO DB
gismuDB = do
  db <- readFile "gismu.db"
  return $ read db :: IO DB
