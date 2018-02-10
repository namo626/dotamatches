{-# LANGUAGE OverloadedStrings #-}

import Cmd

import Text.HTML.Scalpel hiding (URL)
import Options.Applicative (execParser)
import System.IO
import Data.Either (rights)
import Control.Lens
import Network.HTTP.Client (HttpException (..))
import Control.Exception
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (decodeUtf8)
import Network.Wreq hiding (Options)
import Text.HTML.TagSoup (parseTags)
import Data.List.Split
import Control.Concurrent.Async
import Control.Concurrent (forkIO, ThreadId(..))
import Data.String.Utils
import Control.Monad
import Control.Applicative ((<|>), (<$>), (<*>))
import Data.Monoid ((<>))


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

data MatchDetails = MatchDetails
  { getTitle :: T.Text
  , getType :: T.Text
  , getMatchup :: Match
  } deriving (Eq)

data MatchDisplay = MatchDisplay
  { getMatchInfo :: MatchInfo
  , getMatchDetails :: MatchDetails
  } deriving (Eq)

instance Show MatchInfo where
  show (Live _) = "\n"
  show (Upcoming t tour) =
    T.unpack $ "Live in " <> t <> "\n"

instance Show MatchDetails where
  show (MatchDetails n t m) =
    show m <> (T.unpack $ "\n" <> n <> "\n" <> t)

instance Show MatchDisplay where
  show (MatchDisplay info details) =
    show details <> "\n" <> show info


url = "https://www.gosugamers.net/dota2/gosubet"

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


-- Navigating tournament links for more information -------------------------------------

-- | Parses the tournament name
tourName :: Scraper T.Text Tournament
tourName = text $ "h1" // "a"

-- | Parses match type (e.g. best of 3)
tourType :: Scraper T.Text Tournament
tourType = text $ "p" @: ["class" @= "bestof"]

tourMatchup :: Scraper T.Text Match
tourMatchup = do
  opp1 <- tourOpp1
  opp2 <- tourOpp2
  return (opp1, opp2)

tourOpp :: String -> Scraper T.Text T.Text
tourOpp tok = chroot ("div" @: [hasClass tok]) (text $ "h3" // "a")

tourOpp1 = tourOpp "opponent1"
tourOpp2 = tourOpp "opponent2"

-- | Main tournament parser
tourParser :: Scraper T.Text MatchDetails
tourParser = do
  n <- tourName
  t <- tourType
  m <- tourMatchup
  return $ MatchDetails n t m


-- Scraping functions (IO involved) -------------------------------------------------------------

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

-- | Executes the main tournament parser on the tournament URL
processTour :: URL -> IO MatchDetails
processTour url = do
  body <- getBody url
  let tourPage = scrape' tourParser body
  case tourPage of
    Nothing -> throwIO TournamentException
    Just tn -> return tn


-- | Reworking exception handling instead of Maybe type

-- | Exception handler for network

nowOrNext :: Options -> [[a]] -> [[a]]
nowOrNext _ [] = [[],[]]
nowOrNext (Options Now) (m:ms) = [m,[]]
nowOrNext (Options Next) (l:u:_) = [[], u]
nowOrNext _ mss = mss

main :: IO ()
main = do
  opts <- execParser greet
  print opts
  r <- try $ getBody url
  case r of
    Left (HttpExceptionRequest _ _) -> putStrLn "Network Error!"
    Left (InvalidUrlException _ _) -> putStrLn "Invalid URL"
    Right body -> do
      let res = scrape' matchTimes body
      case res of
        Nothing -> putStrLn "Data not found"
        Just mss -> do
          let (lives:upcomings:_) = nowOrNext opts mss
              liveURLs = map getTourURL lives
              upcomingURLs = map getTourURL upcomings

          -- Generate an MVar (in an Async) for each tournament URL
          -- to be scraped concurrently
          ls <- mapM (async . processTour) liveURLs
          as <- mapM (async . processTour) (take 3 upcomingURLs)

          -- Option for user to cancel downloading
          forkIO $ do
            putStrLn "Fetching match infos...\nPress q to cancel download.\n"
            hSetBuffering stdin NoBuffering
            forever $ do
              c <- getChar
              when (c == 'q') $ do putStrLn "Canceled by user"
                                   mapM_ cancel ls
                                   mapM_ cancel as

          liveDetails <- mapM waitCatch ls
          upcomingDetails <- mapM waitCatch as

          let lds = zipWith MatchDisplay lives (rights liveDetails)
              mds = zipWith MatchDisplay upcomings (rights upcomingDetails)

          -- Print only ones that were already downloaded (if user tried to cancel)
          when (not $ null lds) $ putStrLn "Live matches: \n"
          mapM_ print lds
          when (not $ null mds) $ putStrLn "Upcoming matches: \n"
          mapM_ print mds



--Utilities
noEscape :: T.Text -> T.Text
noEscape str = T.filter f str where
  f c
    | c == '\n' = False
    | c == '\r' = False
    | otherwise = True
