{-# LANGUAGE QuasiQuotes #-}
module LongStrings
  where

import Data.String.Here.Interpolated


help = [i|JUKEBOT 3000
help or h -- show this message
now playing or playing or np -- see what's playing now
tokens -- see how many tokens you have
songs -- see what songs are available to play
page [x] -- see songs on page [x]
get tokens or give me tokens -- ask for tokens
play [code] -- play song with the given code

each song costs 1 token
new songs are added to the queue
you get 4 tokens per request
you can ask for tokens every 4 hours

toot @\swizzard@dice.camp with questions/comments
enjoy!
|]

examples = [i|Examples
page 1 -- show songs on the first page
play b1 or play B1 or play B-1 -- play "Only the Good Die Young"
|]

