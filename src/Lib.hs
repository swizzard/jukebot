module Lib
    ( loop
    ) where


import Conduit
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Conduit.Lift (runReaderC)
import Web.Hastodon (streamUser, StreamingPayload)

import Handlers (handler)
import Types


loop :: IO ()
loop = do
  cfg <- initialConfig
  let c = client cfg
  flip runReaderT cfg $ unApp $
    runResourceT $ runConduit $ streamUser c .| mapM_C (lift . handler)
