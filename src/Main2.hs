{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.Scalpel
import Control.Monad
import Control.Applicative ((<|>), (<$>), (<*>))

type Team = String
type Match = (Team, Team)
type Time = String
type Tournament = String

data MatchInfo = Live Match Tournament
               | Upcoming Match Time Tournament
               deriving (Eq, Show)

url = "http://liquipedia.net/dota2/Liquipedia:Upcoming_and_ongoing_matches"

teamName :: Scraper String Team
teamName = attr "data-highlightingclass" "span"


leftTeam :: Scraper String Team
leftTeam = chroot ("td" @: ["class" @= "team-left"]) teamName

rightTeam :: Scraper String Team
rightTeam = chroot ("td" @: ["class" @= "team-right"]) teamName

countDown :: Scraper String Time
countDown = innerHTML $ "span" @: [hasClass "timer-object"]

parseTournament :: Scraper String Tournament
parseTournament = attr "title" $ "div" // "a"

upcoming :: Scraper String MatchInfo
upcoming = do
  left <- leftTeam
  right <- rightTeam
  time <- countDown
  tournament <- parseTournament
  return $ Upcoming (left, right) time tournament

live :: Scraper String MatchInfo
live = do
  left <- leftTeam
  right <- rightTeam
  tournament <- parseTournament
  return $ Live (left, right) tournament

matchInfo = upcoming <|> live

matchInfos :: Scraper String [MatchInfo]
matchInfos = chroots ("table" @: [hasClass "infobox_matches_content"]) matchInfo

matchTimes :: Scraper String [[MatchInfo]]
matchTimes = chroots ("div" @: ["id" @= "infobox_matches"]) matchInfos

main :: IO ()
main = do
  results <- scrapeURL url matchTimes
  case results of
    Just (ongoings:upcomings:_) -> do
      print $ head ongoings
      print $ head upcomings
    Nothing -> putStrLn "Data not found"

prettyPrint :: [Match] -> IO ()
prettyPrint matches = mapM_ display matches where
  display (team1, team2) = putStrLn $ team1 ++ " vs. " ++ team2

filterTBD :: [Match] -> [Match]
filterTBD = filter noTBD where
  noTBD ("tbd","tbd") = False
  noTBD _ = True
