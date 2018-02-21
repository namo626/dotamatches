{-# LANGUAGE OverloadedStrings #-}

module Cmd
  ( Options(..)
  , ViewTime(..)
  , greet
  ) where

import Options.Applicative
import Data.Semigroup ((<>))
import Control.Applicative

data Options = Options
  { getViewTime :: ViewTime
  , getSpoilMode :: Bool
  , getFollowing :: Bool
  , getThreads :: Int
  , maxUpDisplay :: Int
  }
  deriving (Show)

data ViewTime = Both
              | Now
              | Next
              deriving (Show)

greet :: ParserInfo Options
greet = info (helper <*> options) ( fullDesc
                                  <> progDesc "Parses Dota 2 matches"
                                  <> header "dotamatches - a command-line Dota 2 Match scraper" )

-- | Main option collector
options :: Parser Options
options = Options <$> (now <|> next) <*> spoil <*> onlyFavs <*> threads <*> maxUp

-- | Specify the maximum number of upcoming matches to display
maxUp :: Parser Int
maxUp = option auto
        ( short 'n'
        <> long "maxup"
        <> metavar "N"
        <> value 5
        <> help "Display a maximum of N upcoming matches" )

-- | Choice for number of threads to use
threads :: Parser Int
threads = option auto
          ( short 't'
          <> long "threads"
          <> metavar "THREADNUM"
          <> value 1
          <> help "Specify the number of threads to use" )

-- | Choice to view results for live games (if available)
spoil :: Parser Bool
spoil = switch (short 's' <> long "spoil" <> help "Show available results for live matches")

-- | Choice to view only followed teams
onlyFavs :: Parser Bool
onlyFavs = switch
           ( short 'f'
           <> long "following"
           <> help "Display only followed teams" )

-- | Choice to view only live or upcoming matches
now :: Parser ViewTime
now = flag' Now
       ( short 'l'
      <> long "live"
      <> help "Show only live games" )

next :: Parser ViewTime
next = flag Both Next
        ( short 'u'
        <> long "upcoming"
        <> help "Show only upcoming matches" )
