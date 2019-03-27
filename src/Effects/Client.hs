-- | 

module Effects.Client where

import Polysemy
import Polysemy.Effect.TH
import Polysemy.Effect.New
import qualified Data.Text as T
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)


-- | Effect for data coming to and from a connected socket
data Client m a where
  GetMessage :: (T.Text -> a) -> Client m a
  SendMessage :: T.Text -> a -> Client m a
  deriving (Effect, Functor)

makeSemantic ''Client

runClientSocket :: Member (Lift IO) sems => Socket -> Semantic (Client ': sems) a -> Semantic sems a
runClientSocket socket = 
  interpret $ \case
  GetMessage k -> do
    input <- sendM $ recv socket 4096
    let input' = decodeUtf8 input

    -- Todo this should probably be a separate effect.
    sendM $ putStrLn $ T.unpack $ "C: '" <> input' <> "'" 

    return $ k input'

  SendMessage msg k -> do
    sendM $ putStrLn $ T.unpack $ "S: " <> msg
    sendM $ sendAll socket $ encodeUtf8 $ msg <> "\n"
    return k
 
