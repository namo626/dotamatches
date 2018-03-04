{-# LANGUAGE OverloadedStrings #-}

module Dotamatches.Cmd
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
  , getThreads :: Int
  , maxUpDisplay :: Int
  }
  deriving (Show)

data ViewTime = Both
              | Now
              | Next
              deriving (Show)

greet :: ParserInfo Options
greet = info (helper <*> options)
             ( fullDesc
            <> progDesc "Displays information on ongoing and upcoming Dota 2 matches from GosuGamers."
            <> header "dotamatches - a command line Dota 2 match reporter" )

-- | Main option collector
options :: Parser Options
options = pure Options
          <*> (now <|> next)
          <*> spoil
          <*> threads
          <*> maxUp

-- | Specify the maximum number of upcoming matches to display
maxUp :: Parser Int
maxUp = option auto
        ( short 'n'
        <> long "maxup"
        <> metavar "N"
        <> value 7
        <> help "Display a maximum of N upcoming matches" )

-- | Choice for number of threads to use
threads :: Parser Int
threads = option auto
          ( short 't'
          <> long "threads"
          <> metavar "THREADNUM"
          <> value 4
          <> help "Specify the number of threads to use" )

-- | Choice to view results for live games (if available)
spoil :: Parser Bool
spoil = switch
       ( short 's'
      <> long "spoil"
      <> help "Show available results for live matches" )

-- Choice to view only followed teams
{-
onlyFavs :: Parser Bool
onlyFavs = switch
           ( short 'f'
           <> long "following"
           <> help "Display only followed teams" )
-}

-- | Choice to view only live matches
now :: Parser ViewTime
now = flag' Now
       ( short 'l'
      <> long "live"
      <> help "Display only live games" )

-- | Choice to view only upcoming matches
next :: Parser ViewTime
next = flag Both Next
        ( short 'u'
        <> long "upcoming"
        <> help "Display only upcoming games" )
