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
import Text.Regex

type Entry = (String,String,String)
type DB = [Entry]

view :: CGI CGIResult
view = output . showHtml . template =<< do
  db <- getDB
  gismu <- getInput "gismu"
  case gismu of
    Just gismu' -> tryGismu db gismu'
    Nothing     -> tryRandom db

template = (header << (thetitle << "clixra") +++) . (body<<)

tryRandom :: DB -> CGI Html
tryRandom db = do
  rEntry <- randomEntry db
  case rEntry of
    Just entry -> showEntry entry
    Nothing    -> retFail db

tryGismu :: DB -> String -> CGI Html
tryGismu db gismu =
    case find gisEq db of
      Just entry' -> do entry <- validEntry db entry'
                        case entry of
                          Just entry -> showEntry entry
                          Nothing -> return $ p << "Couldn't retrieve from Wiki."
      Nothing     -> notExists
    where gisEq (_,gismu',_) | gismu' == gismu = True
                             | otherwise       = False

showEntry :: Entry -> CGI Html
showEntry (_,gismu,url) = do
  def <- places gismu
  showDef <- getInput "places"
  return $ h1 << gismu
           +++
           (maybe noHtml (const $ p << italics << def) showDef)
           +++
           p << image ! [src url, alt ""]
           +++
           p << ("Permalink: " +++ hotlink perm (primHtml perm))
  where perm = "http://jbotcan.org/clixra2/Clixra.fcgi?gismu=" ++ gismu

notExists :: CGI Html
notExists = return $ p << "That gismu does not exist on the Wiki!"

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
  validEntry db $ db !! i

validEntry :: DB -> Entry -> CGI (Maybe Entry)
validEntry db entry' = do
  case entry' of
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

places :: String -> CGI String
places gismu = liftIO $ do
  gismus <- liftM lines $ readFile "gismu.txt"
  let match = maybe "" head $ matchRegex (mkRegex "^.{61}(.{98})(\\-|.{10}(.*))") def
      def = maybe "" id $ find correct gismus
      correct = (==gismu) . tail . take 6
  return match
