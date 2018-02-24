{-# LANGUAGE OverloadedStrings #-}

import Text.PrettyPrint.ANSI.Leijen
import HTMLParser
import qualified Data.Text.Lazy as T
import Prelude hiding ((<$>))

test = prettyUpDetails (UpMatchDetails "DAC 2018" "Best of 1" ("Mineski", "Navi"))

prettyUpDetails ::  UpMatchDetails -> Doc
prettyUpDetails (UpMatchDetails title t (t1, t2)) =
  hang 2 $ (bold $ dullgreen (text t1' <+> text "vs." <+> text t2'))
  <$> (bold $ blue (text title'))
  <$> (text t')
  where title' = T.unpack title
        t' = T.unpack t
        t1' = T.unpack t1
        t2' = T.unpack t2
{-
data LiveMatchDetails = LiveMatchDetails
  { lTitle :: T.Text         -- ^ Title of the tournament
  , lType :: T.Text          -- ^ BO1, BO3, etc.
  , lMatchup :: Match        -- ^ Team names
  , lResults :: Maybe [T.Text]     -- ^ Results for past and current games
  , lCurrent :: Int       -- ^ Current game being played, e.g. 2nd game
  } deriving (Eq)
-}

test' = prettyLiveDetails (LiveMatchDetails "DAC 2018" "Best of 1" ("OG", "EG") Nothing 1)
prettyLiveDetails :: LiveMatchDetails -> Doc
prettyLiveDetails (LiveMatchDetails title t (t1, t2) rs c) =
  hang 2 $ (bold $ dullyellow (text t1' <+> text "vs." <+> text t2'))
  <$> (bold $ blue $ text title')
  <$> (text t')
  -- <$> (text $ showSpoil rs')
  <$> (text "Currently playing game" <+> int c)
  where title' = T.unpack title
        t' = T.unpack t
        t1' = T.unpack t1
        t2' = T.unpack t2
        rs' = fmap (map T.unpack) rs
        s = undefined
    --    showSpoil Just xs = xs
     --   showSpoil _ = []
