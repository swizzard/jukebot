module Lib
    ( loop
    ) where


import Conduit (runConduitRes, mapM_C, (.|))
import Data.Conduit.Lift (evalStateC)
import Web.Hastodon (streamUser)

import Handlers (handler)
import Types (getClient, initialState, App(..))


loop :: IO ()
loop = do
  mastoClient <- getClient
  s <- initialState
  runConduitRes $ evalStateC s $ streamUser mastoClient .| mapM_C (unApp . handler)
