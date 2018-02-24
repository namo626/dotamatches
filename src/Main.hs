{-# LANGUAGE OverloadedStrings #-}

{- TODOs:
* Filter for selected teams
* Pretty printing
* Terminal color support
* Concurrency design
* Caching
* Error handling
-}

import Cmd
import HTMLParser
import System.Environment (getArgs)
import Options.Applicative (execParser)
import System.IO
import Data.Char (isDigit)
import Data.Either (rights)
import Network.HTTP.Client (HttpException (..))
import Control.Exception
import qualified Data.Text.Lazy as T
import Data.List.Split
import Control.Concurrent.Async
import Control.Concurrent (forkIO, ThreadId(..))
import Data.String.Utils
import Control.Monad
import Control.Applicative ((<|>), (<$>), (<*>))
import Data.Monoid ((<>))
import Control.Concurrent.MSem
--import Control.Concurrent.CachedIO (cachedIO)



-- | Parsing user option to view only live or upcoming matches (or both, by default)
nowOrNext :: Options -> [[a]] -> [[a]]
nowOrNext _ [] = [[],[]]
nowOrNext opts ls = case getViewTime opts of
                      Both -> ls
                      Now -> [head ls, []]
                      Next -> [[], ls !! 1]

url = "https://www.gosugamers.net/dota2/gosubet"

main :: IO ()
main = do
  opts <- execParser greet
  print opts
  print teams
  r <- try $ getBody url
  case r of
    Left (HttpExceptionRequest _ _) -> putStrLn "Network Error."
    Left (InvalidUrlException _ _) -> putStrLn "Invalid URL."
    Right body -> do
      let res = scrape' matchTimes body
      case res of
        Nothing -> putStrLn "Match summary not found."
        Just mss -> do
          let (lives:upcomings:_) = nowOrNext opts mss
              liveURLs = map getTourURL lives
              upcomingURLs = map getTourURL upcomings
              n = maxUpDisplay opts

          sem <- new $ getThreads opts

          -- Generate an MVar (in an Async) for each tournament URL
          -- to be scraped concurrently
          -- ls, as are MatchDetails
          ls <- mapM (async . with sem . processTour (liveTourParser opts)) liveURLs
          as <- mapM (async . with sem . processTour (upTourParser opts)) (take n upcomingURLs)


          liveDetails <- mapM waitCatch ls   --Left exception OR Right MatchDetails
          upcomingDetails <- mapM waitCatch as

          let lds = filter (isFavTeam todo teams . extractMatch) $ zipWith MatchDisplay lives liveDetails
              mds = filter (isFavTeam todo teams . extractMatch) $ zipWith MatchDisplay upcomings upcomingDetails
              todo = getFollowing opts

          -- Print only ones that were already downloaded (if user tried to cancel)
          when (not $ null lds) $ putStrLn "Live matches: \n"
          mapM_ print lds
          when (not $ null mds) $ putStrLn "Upcoming matches: \n"
          mapM_ print mds


-- | Option for user to cancel downloading
cancelDownload :: [Async a] -> [Async a] -> IO ThreadId
cancelDownload ls as = forkIO $ do
    putStrLn "Fetching match infos...\nPress q to cancel download.\n"
    hSetBuffering stdin NoBuffering
    forever $ do
      c <- getChar
      when (c == 'q') $ do putStrLn "\nProcess canceled by user"
                           mapM_ cancel ls
                           mapM_ cancel as

--Utilities

teams = ["OG Dota2"]
extractMatch :: MatchDisplay -> Match
extractMatch (MatchDisplay _ (Right (MatchDetails_l lm))) = lMatchup lm
extractMatch (MatchDisplay _ (Right (MatchDetails_u um))) = uMatchup um
extractMatch (MatchDisplay _ (Left _)) = ("","")

-- | If one of the followed teams is in the match pair, select the pair
isFavTeam :: Bool -> [Team] -> Match -> Bool
isFavTeam todo teams (t1, t2) = if todo
                                then (elem (T.toLower t1) ts
                                      || elem (T.toLower t2) ts)
                                else True
  where
    ts = map T.toLower teams
