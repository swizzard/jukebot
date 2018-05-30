module Toot
  ( toot
  , tootAt
  ) where

import Control.Monad.Reader
import Data.Semigroup ((<>))
import Network.HTTP.Simple (JSONException)
import Web.Hastodon

import Types


handleResp :: Either JSONException Status -> App ()
handleResp (Left e) = do
  l <- asks logger
  liftIO $ logErr l e
handleResp (Right _) = return ()


toot :: String -> App ()
toot s = do
  c <- asks client
  let o = visibility VisibilityUnlisted
  (liftIO $ postStatusWithOption c o s) >>= handleResp


tootAt :: AccountId -> String -> App ()
tootAt hId s = do
  c <- asks client
  let o = inReplyToId hId <> visibility VisibilityUnlisted
  (liftIO $ postStatusWithOption c o s) >>= handleResp
