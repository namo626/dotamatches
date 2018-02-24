{-# LANGUAGE OverloadedStrings #-}

module HTMLParser
  where

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


type Team = T.Text
type URL = T.Text
type Match = (Team, Team)
type Time = T.Text
type Tournament = T.Text

data MatchInfo = Live Tournament
               | Upcoming Time Tournament
               deriving (Eq)

getTourURL :: MatchInfo -> URL
getTourURL (Live t) = t
getTourURL (Upcoming _ t) = t

-- | Specific match detail after following URL
data MatchDetails = MatchDetails_l LiveMatchDetails
                  | MatchDetails_u UpMatchDetails
                  deriving (Eq)

data LiveMatchDetails = LiveMatchDetails
  { lTitle :: T.Text         -- ^ Title of the tournament
  , lType :: T.Text          -- ^ BO1, BO3, etc.
  , lMatchup :: Match        -- ^ Team names
  , lResults :: Maybe [T.Text]     -- ^ Results for past and current games
  , lCurrent :: Int       -- ^ Current game being played, e.g. 2nd game
  } deriving (Eq)

data UpMatchDetails = UpMatchDetails
  { uTitle :: T.Text
  , uType :: T.Text
  , uMatchup :: Match
  } deriving (Eq)

data MatchDisplay = MatchDisplay MatchInfo (Either SomeException MatchDetails)

instance Show MatchInfo where
  show (Live _) = "\n"
  show (Upcoming t tour) =
    T.unpack $ T.concat ["Live in ", t, "\n"]

instance Show MatchDetails where
  show (MatchDetails_l lmd) = show lmd
  show (MatchDetails_u umd) = show umd

instance Show LiveMatchDetails where
  show (LiveMatchDetails n t (t1, t2) res c) =
    T.unpack t1 <> " vs. " <> T.unpack t2 <> "\n"
    <> (T.unpack $ n <> "\n" <> t) <> "\n"
    <> (show $ fmap (map T.unpack) res) <> "\n"
    <> "Currently playing game " <> show c <> "\n"

instance Show UpMatchDetails where
  show (UpMatchDetails n t (t1, t2)) =
    T.unpack $ T.concat [t1, " vs. ", t2, "\n", n, "\n", t]

instance Show MatchDisplay where
  show (MatchDisplay info (Left err)) =
    show err <> "\n" <> show info
  show (MatchDisplay info (Right details)) =
    show details <> "\n" <> (show info)


team :: String -> Scraper T.Text Team
team side = chroot ("span" @: ["class" @= ("opp " ++ side)]) $ text "span"

leftTeam = team "opp1"
rightTeam = team "opp2"

time :: Scraper T.Text Time
time = text ("span" @: ["class" @= "live-in"])

-- | URL header for inner links
headerURL = "https://www.gosugamers.net"

-- | Parses the tournament URL in the right side (with logo)
tourURL :: Scraper T.Text URL
tourURL = fmap (headerURL <>) $ attr "href" $ "a" @: ["class" @= "tooltip-right"]

-- | Parses the tournament URL in the center
tourURL' :: Scraper T.Text URL
tourURL' = fmap (headerURL <>) $ attr "href" $ "a" @: ["class" @= "match hover-background"]

-- | Parses a live match
liveMatchInfo :: Scraper T.Text MatchInfo
liveMatchInfo = do
  --left <- leftTeam
  --right <- rightTeam
  tournament <- tourURL'
  return $ Live tournament

-- | Parses an upcoming match
upMatchInfo :: Scraper T.Text MatchInfo
upMatchInfo = do
--  left <- leftTeam
--  right <- rightTeam
  t <- time
  tournament <- tourURL'
  return $ Upcoming (T.strip $ noEscape t) tournament

matchInfo = upMatchInfo <|> liveMatchInfo

matchInfos :: Scraper T.Text [MatchInfo]
matchInfos = chroots "tr" matchInfo

matchTimes :: Scraper T.Text [[MatchInfo]]
matchTimes = chroots ("div" @: ["class" @= "box"]) matchInfos


-- Navigating tournament links for more information -------------------------------

-- | Parses the tournament name
tourName :: Scraper T.Text Tournament
tourName = text $ "h1" // "a"

-- | Parses match type (e.g. best of 3)
tourType :: Scraper T.Text Tournament
tourType = text $ "p" @: ["class" @= "bestof"]

-- | Combines two teams into a match tuple
tourMatchup :: Scraper T.Text Match
tourMatchup = liftM2 (,) tourOpp1 tourOpp2

-- | Parses a team's name
tourOpp :: String -> Scraper T.Text T.Text
tourOpp tok = chroot ("div" @: [hasClass tok]) (text $ "h3" // "a")

tourOpp1 = tourOpp "opponent1"
tourOpp2 = tourOpp "opponent2"

-- | Winners of previous games
tourResult :: Scraper T.Text [T.Text]
tourResult = attrs "winner" $ "input" @: ["class" @= "btn-winner"]

-- | Current game that is being played, 1st - 5th
tourCurrent :: Scraper T.Text Int
tourCurrent = fmap currentGame $ texts $ "a" @: ["class" @= "button live js-parent-hover", "pos" @= "0"] where
  currentGame :: [T.Text] -> Int
  currentGame r = maximum . map (read . filter isDigit . T.unpack) $ r

--tourResults :: Scraper T.Text [T.Text]
--tourResults = chroot ("div" @: ["class" @= "matches-streams"]) tourResult

-- | Main tournament parsers, live or upcoming games
liveTourParser :: Options -> Scraper T.Text MatchDetails
liveTourParser opts = do
  n <- tourName
  t <- tourType
  m <- tourMatchup
  r <- fmap choice tourResult
  c <- tourCurrent
  return $ MatchDetails_l $ LiveMatchDetails n t m r c where
    choice :: [T.Text] -> Maybe [T.Text]
    choice res | (getSpoilMode opts) = Just res
               | otherwise = Nothing

upTourParser :: Options -> Scraper T.Text MatchDetails
upTourParser opts = do
  n <- tourName
  t <- tourType
  m <- tourMatchup
  return $ MatchDetails_u $ UpMatchDetails n t m

-- | Main tournament parser
--tourParser :: Scraper T.Text MatchDetails
--tourParser = liveTourParser <|> upTourParser


-- Scraping functions (IO involved) ---------------------------------------------

data MatchException = TournamentException
  deriving Show

instance Exception MatchException

-- | Converting HTTP response to a list of tags, and scrape it
scrape' s = scrape s . parseTags

-- | Takes a URL and return the response body
getBody :: URL -> IO T.Text
getBody url = do
  response <- get (T.unpack url)
  return $ response ^. responseBody . to decodeUtf8

-- | Executes the main tournament parser on the tournament URL and given tournament parser
processTour :: Scraper T.Text a -> URL -> IO a
processTour p url = do
  body <- getBody url
  threadDelay $ 1 * 10^6
  let tourPage = scrape' p body
  case tourPage of
    Nothing -> throwIO TournamentException
    Just tn -> return tn

noEscape :: T.Text -> T.Text
noEscape str = T.filter f str where
  f c
    | c == '\n' = False
    | c == '\r' = False
    | otherwise = True
