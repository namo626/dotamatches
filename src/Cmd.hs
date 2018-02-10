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
  { getViewTime :: ViewTime }
  deriving (Show)

data ViewTime = Both
              | Now
              | Next
              deriving (Show)

greet :: ParserInfo Options
greet = info (helper <*> options) ( fullDesc
                                  <> progDesc "Parses Dota 2 matches"
                                  <> header "dotamatches - a command-line Dota 2 Match scraper" )

options :: Parser Options
options = Options <$> (now <|> next)

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
