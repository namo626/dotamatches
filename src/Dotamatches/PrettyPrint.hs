{-# LANGUAGE OverloadedStrings #-}

module Dotamatches.PrettyPrint
  ( UpMatchDetails(..)
  , LiveMatchDetails(..)
  , MatchInfo(..)
  , MatchDisplay(..)) where

import Dotamatches.HTMLParser
import Text.PrettyPrint.ANSI.Leijen
import Control.Exception
import qualified Data.Text.Lazy as T
import Prelude hiding ((<$>))

--test = prettyUpDetails (UpMatchDetails "DAC 2018" "Best of 1" ("Mineski", "Navi"))

instance Pretty UpMatchDetails where
  pretty = prettyUpDetails

prettyUpDetails ::  UpMatchDetails -> Doc
prettyUpDetails (UpMatchDetails title t (t1, t2)) =
  hang 2 $ bold (dullgreen (text t1' <+> text "vs." <+> text t2'))
  <$> bold (blue (text title'))
  <$> text t'
  where title' = T.unpack title
        t' = T.unpack t
        t1' = T.unpack t1
        t2' = T.unpack t2

--test' = prettyLiveDetails (LiveMatchDetails "DAC 2018" "Best of 1" ("OG", "EG") Nothing 1)
--test'' = prettyLiveDetails (LiveMatchDetails "DAC 2018" "Best of 1" ("OG", "EG") (Just ["OG"]) 1)

instance Pretty LiveMatchDetails where
  pretty = prettyLiveDetails

prettyLiveDetails :: LiveMatchDetails -> Doc
prettyLiveDetails (LiveMatchDetails title t (t1, t2) rs c) =
  hang 2 $ bold (dullyellow (text t1' <+> text "vs." <+> text t2'))
  <$> bold (blue $ text title')
  <$> text t'
  <> bold (showSpoil rs')
  <$> (text "Currently playing game" <+> int c)
  where title' = T.unpack title
        t' = T.unpack t
        t1' = T.unpack t1
        t2' = T.unpack t2
        rs' = fmap (map T.unpack) rs
        showSpoil (Just xs) = line <> (parens $ align $ cat $ punctuate comma $ map text xs)
        showSpoil _ = empty


--info = Upcoming "20hr" "url"

instance Pretty MatchInfo where
  pretty = prettyInfo

prettyInfo :: MatchInfo -> Doc
prettyInfo (Live _) = empty
prettyInfo (Upcoming t _) =
  ("Live in" <+> text t')
  where t' = T.unpack t


--res = prettyDisplay $ UpDisplay (Upcoming "20hr" "url") (Right $ UpMatchDetails "DAC 2018" "Best of 3" ("NAvi", "OG"))
--res' = prettyDisplay $ UpDisplay (Upcoming "20hr" "url") (Left $ toException $ MatchException "random error")

instance Pretty MatchDisplay where
  pretty = prettyDisplay

prettyDisplay :: MatchDisplay -> Doc
prettyDisplay (LiveDisplay mi emd) =
  case emd of
    Left err -> text "Could not retrieve match details"
    Right md -> pretty md
prettyDisplay (UpDisplay mi emd) =
  case emd of
    Left err -> text "Could not retrieve match details"
    Right md -> nest 2 $ pretty md <$> pretty mi
