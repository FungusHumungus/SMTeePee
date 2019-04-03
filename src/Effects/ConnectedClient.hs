module Effects.ConnectedClient where

import Polysemy
import Polysemy.Effect.TH
import Polysemy.Effect.New
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
  GetMessage :: Client tok -> ( T.Text -> a ) -> ConnectedClient m a
  SendMessage :: Client tok -> T.Text -> a -> ConnectedClient m a

makeSemantic ''ConnectedClient

deriving instance Functor (ConnectedClient m)
deriving instance Effect ConnectedClient

runConnectedClient :: Member (Lift IO) sems
                   => Member (State.State (Maybe S.Socket)) sems
                   => Semantic (ConnectedClient ': sems) a -> Semantic sems a
runConnectedClient =
  interpret $ \case
  GetMessage _ k -> do
    socket <- State.get 
    case socket of
      Just socket' -> do
        msg <- sendM $ recv socket' 4096
        let msg' = decodeUtf8 msg
        return . k $ msg'
      Nothing -> fail "You really should have a socket by now."
    
  SendMessage _ msg k -> do
    socket <- State.get 
    case socket of
      Just socket' -> do
        sendM $ sendAll socket' $ encodeUtf8 $ msg <> "\n"
      Nothing -> fail "You really should have a socket by now."

    return k
