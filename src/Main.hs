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
import HTMLParser hiding (UpMatchDetails, LiveMatchDetails, MatchInfo, MatchDisplay)
import PrettyPrint

import Text.PrettyPrint.ANSI.Leijen (pretty)
import System.Exit
import System.Environment (getArgs)
import Options.Applicative (execParser)
import System.IO
import Data.Char (isDigit)
--import Data.Either (rights)
import Network.HTTP.Client (HttpException(..))
import Control.Exception
import qualified Data.Text.Lazy as T
import Data.List.Split
import Control.Concurrent.Async
import Control.Concurrent (forkIO, ThreadId(..))
import Data.String.Utils
import Control.Monad
import Data.Monoid ((<>))
import Control.Concurrent.MSem
--import Control.Concurrent.CachedIO (cachedIO)




url = "https://www.gosugamers.net/dota2/gosubet"

main :: IO ()
main = do
  -- command line args
  opts <- execParser greet
  when (getThreads opts < 1) $ do
    putStrLn "No. of threads must be at least 1"
    exitWith (ExitFailure 1)

  r <- try $ processURL "URL contains no data" matchTimes url
  case r of
    Left (HttpExceptionRequest _ _) -> putStrLn "Network Error"
    Left (InvalidUrlException _ _) -> putStrLn "Invalid URL"
    Right [] -> putStrLn "Match summary not found"
    Right mss -> do
      let (lives:upcomings:_) = selectTime (getViewTime opts) mss
          liveURLs            = map getMatchURL lives
          upcomingURLs        = map getMatchURL upcomings
          n                   = maxUpDisplay opts

      sem <- new $ getThreads opts

      let parDownload p = async . with sem . processURL message p
          message = "Match details not found"
          --select = isFavTeam todo teams . extractMatch
          --todo = getFollowing opts

      ls <- mapM (parDownload $ liveDetails opts) liveURLs
      as <- mapM (parDownload $ upDetails opts) (take n upcomingURLs)

      lds <- mapM waitCatch ls   --Left exception OR Right MatchDetails
      uds <- mapM waitCatch as

      let lds' = zipWith LiveDisplay lives lds
          mds' = zipWith UpDisplay upcomings uds

      unless (null lds') $ putStrLn "Live matches: \n"
      mapM_ (print . pretty) lds'

      unless (null mds') $ putStrLn "\nUpcoming matches: \n"
      mapM_ (print . pretty) mds'






-- Additional options

-- | Parsing user option to view only live or upcoming matches (or both, by default)
selectTime :: ViewTime -> [[a]] -> [[a]]
selectTime _ [] = [[],[]]
selectTime Both ls = ls
selectTime Now ls = [ls !! 0, []]
selectTime Next ls =  [[], ls !! 1]

{-
cancelDownload :: [Async a] -> [Async a] -> IO ThreadId
cancelDownload ls as = forkIO $ do
    putStrLn "Fetching match infos...\nPress q to cancel download.\n"
    hSetBuffering stdin NoBuffering
    forever $ do
      c <- getChar
      when (c == 'q') $ do putStrLn "\nProcess canceled by user"
                           mapM_ cancel ls
                           mapM_ cancel as

teams = ["OG Dota2"]
extractMatch :: MatchDisplay -> Match
extractMatch (LiveDisplay _ lmd) = case lmd of
  Left _-> ("","")
  Right md -> lMatchup md
extractMatch (UpDisplay _ umd) = case umd of
  Left _ -> ("","")
  Right md -> uMatchup md

-- | If one of the followed teams is in the match pair, select the pair
isFavTeam :: Bool -> [T.Text] -> Match -> Bool
isFavTeam todo teams (t1, t2) = if todo
                                then (elem (T.toLower t1) ts
                                      || elem (T.toLower t2) ts)
                                else True
  where
    ts = map T.toLower teams
-}
