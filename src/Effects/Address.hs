module Effects.Address where

import Polysemy
import Polysemy.Effect.TH
import Polysemy.Effect.New
import qualified Data.Text as T
import Network.Socket (AddrInfo, Socket, SockAddr)
import qualified Network.Socket as S
import Network.Socket.ByteString (recv, sendAll)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)


data Address m a where
  Resolve :: S.ServiceName -> (AddrInfo -> a) -> Address m a
  Open :: AddrInfo -> (Socket -> a) -> Address m a
  Close :: Socket -> a -> Address m a
  Accept :: Socket -> ((Socket, SockAddr) -> a) -> Address m a
  deriving (Functor, Effect)

makeSemantic ''Address


runAddress :: Member (Lift IO) sems => Semantic (Address ': sems) a -> Semantic sems a
runAddress =
  interpret $ \case
  Resolve port k -> do
    let hints = S.defaultHints { S.addrFlags = [S.AI_PASSIVE]
                               , S.addrSocketType = S.Stream
                               }
    addr <- sendM $ S.getAddrInfo (Just hints) Nothing (Just port)
    return $ k $ head addr

  Open addr k -> do
    sock <- sendM $ S.socket (S.addrFamily addr) (S.addrSocketType addr) (S.addrProtocol addr)
    sendM $ S.setSocketOption sock S.ReuseAddr 1
    sendM $ S.bind sock (S.addrAddress addr)

    let fd = S.fdSocket sock
    sendM $ S.setCloseOnExecIfNeeded fd
    sendM $ S.listen sock 10
    return $ k sock

  Close socket k -> sendM $ S.close socket >> pure k

  Accept socket k -> do
    res <- sendM $ S.accept socket
    return $ k res


