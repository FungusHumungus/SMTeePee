-- | 

module Effects.Client where

import Control.Monad.Freer
import qualified Control.Monad.Freer as FR
import Control.Monad.Freer.TH
import qualified Data.Text as T
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)


-- | Typeclass for data coming to and from a connected socket
data Client r where
  GetMessage :: Client T.Text
  SendMessage :: T.Text -> Client ()

makeEffect ''Client

runClientSocket :: Member IO effs => Socket -> Eff (Client ': effs) a -> Eff effs a
runClientSocket socket = 
  interpret $ \case
  GetMessage -> do
    input <- FR.send $ recv socket 4096
    let input' = decodeUtf8 input

    -- Todo this should probably be a separate effect.
    FR.send $ putStrLn $ T.unpack $ "C: '" <> input' <> "'" 

    return $ input'

  SendMessage msg -> do
    FR.send $ putStrLn $ T.unpack $ "S: " <> msg
    FR.send $ sendAll socket $ encodeUtf8 $ msg <> "\n"
 
