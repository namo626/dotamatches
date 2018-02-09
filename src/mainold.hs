{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wreq
import Control.Lens hiding (element, children)
import Text.Taggy.Lens
import Data.Text.Lazy
import Data.Text.Lazy.Encoding (decodeUtf8)


titleSearch = html . allNamed (only "h1") . children . traverse . contents

teamNames position = html . allAttributed (folded . only position) . children . traverse . element . attr "data-highlightingclass" . _Just

upComing = teamNames "team-left"


upComing = html . allAttributed (folded . only "team-left") . children . traverse . element . attr "data-highlightingclass" . _Just

decodeResponse = responseBody . to decodeUtf8

link = "http://liquipedia.net/dota2/Liquipedia:Upcoming_and_ongoing_matches"

main :: IO ()
main = do
  r <- get link
  print (r ^? decodeResponse . titleSearch)
  print (r ^.. responseBody . to decodeUtf8 . upComing)
