-- | 

module Client where

import qualified Data.Text as T


-- | Typeclass for data coming to and from a connected socket
class (Monad m) => ClientM m where
  getMessage :: m T.Text
  sendMessage :: T.Text -> m ()

