module Lib
    ( loop
    ) where


import Conduit
import Control.Monad.Reader (runReaderT)
import Web.Hastodon (streamUser)

import Handlers (handler)
import Types (App(..), Config(..), initialConfig)


loop :: IO ()
loop = do
  cfg <- initialConfig
  flip runReaderT cfg $ unApp $ runConduitRes $ streamUser (client cfg) .| mapM_C (lift . handler)
