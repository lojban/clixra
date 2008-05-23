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
import IO

type Entry = (String,String,String)
type DB = [Entry]

view :: CGI CGIResult
view = output . showHtml =<< do
  db <- getDB
  rEntry <- randomEntry db
  case rEntry of
    Just entry -> showEntry entry
    Nothing    -> retFail db

showEntry :: Entry -> CGI Html
showEntry (_,gismu,url) = do
  return $ h1 << gismu
           +++
           image ! [src url]

retFail :: DB -> CGI Html
retFail db = do
  tryEx <- doExistingEntry db
  return $ p << "Couldn't retrieve entry from Wikipedia."
           +++
           tryEx

doExistingEntry :: DB -> CGI Html
doExistingEntry db = do
  rExEntry <- randomEntry $ filter hasURL db
  case rExEntry of
    Just entry -> showEntry entry
    Nothing    -> return $ p << "Could not get any cached entries either!"
    where hasURL (_,_,"") = False
          hasURL _        = True

randomEntry :: DB -> CGI (Maybe Entry)
randomEntry [] = return Nothing
randomEntry db = do
  i <- randomIndex db
  case db !! i of
    e@(img,gismu,"") -> tryLookup db e
    entry            -> return $ Just entry

tryLookup :: DB -> Entry -> CGI (Maybe Entry)
tryLookup db (img,gismu,_) = do
  url <- imageLookup img
  case url of
    Just url' -> do updateDBURL db img url'
                    return $ Just (img,gismu,url')
    _         -> return $ Nothing

updateDBURL :: DB -> String -> String -> CGI ()
updateDBURL db img url = liftIO $ do
  let newDB = mapMaybe update db
      update (img',gismu,_) | img' == img = Just (img,gismu,url)
      update (img,gismu,u) = Just (img,gismu,u)
  h <- openFile "gismu.db" WriteMode
  hSetBuffering h NoBuffering
  hPutStr h $ show newDB
  hClose h

imageLookup :: String -> CGI (Maybe String)
imageLookup img = do
  (code,body) <- liftIO $ curlGetString url []
  if code == CurlOK
     then return $ getUrl body
     else return Nothing
  where url = "http://commons.wikimedia.org/w/api.php?action=query&titles="
              ++ urlEncode(img) ++
              "&prop=imageinfo&iiprop=url&&iiurlwidth=500&format=xml"

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

randomIndex :: DB -> CGI Int
randomIndex db = liftIO $ getStdRandom $ randomR (0,length db-1)

getDB :: CGI DB
getDB = liftIO $ liftM read $ readFile "gismu.db"
