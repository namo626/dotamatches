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
options = Options <$> (now <|> next) <*> spoil

-- | Choice to view results for live games (if available)
spoil :: Parser Bool
spoil = switch (short 's' <> long "spoil" <> help "Show available results for live matches")


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
