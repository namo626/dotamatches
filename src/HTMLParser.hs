{-# LANGUAGE OverloadedStrings #-}

module HTMLParser
  ( LiveMatchDetails(..)
  , UpMatchDetails(..)
  , MatchInfo(..)
  , MatchDisplay(..)
  , liveDetails
  , upDetails
  , matchTimes
  , getBody
  , scrape'
  , processURL
  , getMatchURL
  ) where

{- TODOs:
* Filter for selected teams
* Pretty printing
* Terminal color support
* Concurrency design
* Caching
* Error handling
-}

import Cmd
import Text.HTML.Scalpel hiding (URL)
import Options.Applicative (execParser)
import System.IO
import Data.Char (isDigit)
import Control.Lens
import Network.HTTP.Client (HttpException (..))
import Control.Exception
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (decodeUtf8)
import Network.Wreq hiding (Options)
import Text.HTML.TagSoup (parseTags)
import Data.List.Split
import Data.String.Utils
import Control.Monad
import Control.Applicative ((<|>), (<$>), (<*>))
import Data.Monoid ((<>))
import Control.Concurrent (threadDelay)


type URL = T.Text
type Match = (T.Text, T.Text)

-- | A MatchInfo consists of a link for details and time ticker (for upcoming ones)
data MatchInfo = Live URL
               | Upcoming T.Text URL
               deriving (Show)

getMatchURL :: MatchInfo -> URL
getMatchURL (Live t) = t
getMatchURL (Upcoming _ t) = t

-- | Contains details of a live match gathered from the link
data LiveMatchDetails = LiveMatchDetails
  { lTitle :: T.Text         -- ^ Title of the tournament
  , lType :: T.Text          -- ^ BO1, BO3, etc.
  , lMatchup :: Match        -- ^ Team names
  , lResults :: Maybe [T.Text]     -- ^ Results for past and current games
  , lCurrent :: Int       -- ^ Current game being played, e.g. 2nd game
  } deriving (Show)

-- | Contains details of an upcoming match gathered from the link
data UpMatchDetails = UpMatchDetails
  { uTitle :: T.Text    -- ^ Title of the tournament
  , uType :: T.Text     -- ^ BO1, BO3, etc.
  , uMatchup :: Match   -- ^ Team names
  } deriving (Show)

-- | Final container for the info and details of each match
data MatchDisplay = LiveDisplay MatchInfo (Either SomeException LiveMatchDetails)
                  | UpDisplay MatchInfo (Either SomeException UpMatchDetails)
                  deriving (Show)


-- | Parses the time for an upcoming match to go live
time :: Scraper T.Text T.Text
time = text ("span" @: ["class" @= "live-in"])

-- | URL header for inner links from which the details will be parsed
headerURL = "https://www.gosugamers.net"

-- | Parses the detail URL in the right side (with logo)
matchURL :: Scraper T.Text URL
matchURL = fmap (headerURL <>) $ attr "href" $ "a" @: ["class" @= "tooltip-right"]

-- | Parses the detail URL in the center
matchURL' :: Scraper T.Text URL
matchURL' = fmap (headerURL <>) $ attr "href" $ "a" @: ["class" @= "match hover-background"]

-- | Parses a live match
liveMatchInfo :: Scraper T.Text MatchInfo
liveMatchInfo = do
  tournament <- matchURL'
  return $ Live tournament

-- | Parses an upcoming match
upMatchInfo :: Scraper T.Text MatchInfo
upMatchInfo = do
  t <- time
  tournament <- matchURL'
  return $ Upcoming (T.strip $ noEscape t) tournament

noEscape :: T.Text -> T.Text
noEscape = T.filter f where
  f c
    | c == '\n' = False
    | c == '\r' = False
    | otherwise = True

-- | Main MatchInfo parser
matchInfo = upMatchInfo <|> liveMatchInfo

-- | Parses multiple MatchInfos
matchInfos :: Scraper T.Text [MatchInfo]
matchInfos = chroots "tr" matchInfo

-- | Separately parses Live and Upcoming MatchInfos
matchTimes :: Scraper T.Text [[MatchInfo]]
matchTimes = chroots ("div" @: ["class" @= "box"]) matchInfos



-- * Navigating match links for details
-- These functions are used within each match's own page

-- | Parses the tournament name
detName :: Scraper T.Text T.Text
detName = text $ "h1" // "a"

-- | Parses match type (e.g. best of 3)
detType :: Scraper T.Text T.Text
detType = text $ "p" @: ["class" @= "bestof"]

-- | Parses and combines two teams into a match tuple
detMatchup :: Scraper T.Text Match
detMatchup = liftM2 (,) opp1 opp2

-- | Parses a (left or right) team's name
opp :: String -> Scraper T.Text T.Text
opp tok = chroot ("div" @: [hasClass tok]) (text $ "h3" // "a")

opp1 = opp "opponent1"
opp2 = opp "opponent2"

-- | Parses a list of winners of previous games
detResult :: Scraper T.Text [T.Text]
detResult = attrs "winner" $ "input" @: ["class" @= "btn-winner"]

-- | Parses the current game that is being played, 1st - 5th
detCurrent :: Scraper T.Text Int
detCurrent =
  fmap currentGame $ texts $ "a" @: ["class" @= "button live js-parent-hover", "pos" @= "0"]
  where
    currentGame :: [T.Text] -> Int
    currentGame = maximum . map (read . filter isDigit . T.unpack)


-- | Main detail parser for a live match. Accepts the given command line options.
liveDetails :: Options -> Scraper T.Text LiveMatchDetails
liveDetails opts = do
  n <- detName
  t <- detType
  m <- detMatchup
  r <- fmap choice detResult
  c <- detCurrent
  return $ LiveMatchDetails n t m r c where
    choice :: [T.Text] -> Maybe [T.Text]
    choice res | getSpoilMode opts = Just res
               | otherwise = Nothing

-- | Main detail parser for an upcoming match. Accepts the given command line options
upDetails :: Options -> Scraper T.Text UpMatchDetails
upDetails opts = do
  n <- detName
  t <- detType
  m <- detMatchup
  return $ UpMatchDetails n t m



-- * Functions to perform actual scraping

data MatchException = MatchException T.Text    -- ^ The scraper didn't find anything on the detail link of a match
  deriving Show

instance Exception MatchException


-- | Converts an HTTP response to a list of tags, and scrape it
scrape' s = scrape s . parseTags

-- | Takes a URL and return its response body
getBody :: URL -> IO T.Text
getBody url = do
  response <- get (T.unpack url)
  return $ response ^. responseBody . to decodeUtf8

-- | Executes a given Scraper on the URL. Can throw a MatchException
-- based on the message given.
processURL :: T.Text -> Scraper T.Text a -> URL -> IO a
processURL message p url = do
  body <- getBody url
  threadDelay $ 1 * 10^6
  let tourPage = scrape' p body
  case tourPage of
    Nothing -> throwIO $ MatchException message
    Just tn -> return tn
