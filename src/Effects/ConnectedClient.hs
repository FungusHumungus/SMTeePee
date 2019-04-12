module Effects.ConnectedClient where

import Polysemy
import qualified Polysemy.State as State

import Control.Monad (void, forever)
import Control.Exception (bracket)
import Control.Concurrent (forkFinally)
import qualified Data.Text as T
import Network.Socket (AddrInfo, Socket, SockAddr)
import qualified Network.Socket as S
import Network.Socket.ByteString (recv, sendAll)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Effects.Server (Client)


-- | Effect that handles socket communication once a client has connected.
-- Theoretically the Client tok should ensure that this effect can only be called
-- once we have a connected socket in our state.
-- I haven't quite sussed that one out yet...
data ConnectedClient m a where
  GetMessage :: Client tok -> ConnectedClient m T.Text 
  SendMessage :: Client tok -> T.Text -> ConnectedClient m ()

makeSemantic ''ConnectedClient


runConnectedClient :: Member (Lift IO) sems
                   => Member (State.State (Maybe S.Socket)) sems
                   => Semantic (ConnectedClient ': sems) a -> Semantic sems a
runConnectedClient =
  interpret $ \case
  GetMessage _ -> do
    socket <- State.get 
    case socket of
      Just socket' -> do
        msg <- sendM $ recv socket' 4096
        let msg' = decodeUtf8 msg
        return msg'
      Nothing -> fail "You really should have a socket by now."
    
  SendMessage _ msg -> do
    socket <- State.get 
    case socket of
      Just socket' -> do
        sendM $ sendAll socket' $ encodeUtf8 $ msg <> "\n"
      Nothing -> fail "You really should have a socket by now."

    return ()
