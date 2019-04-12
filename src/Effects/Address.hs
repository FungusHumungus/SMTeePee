module Effects.Address where

import Polysemy
import qualified Data.Text as T
import Network.Socket (AddrInfo, Socket, SockAddr)
import qualified Network.Socket as S
import Network.Socket.ByteString (recv, sendAll)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)


data Address m a where
  Resolve :: S.ServiceName -> Address m AddrInfo
  Open :: AddrInfo -> Address m Socket
  Close :: Socket -> Address m ()
  Accept :: Socket -> Address m (Socket, SockAddr)

makeSemantic ''Address


runAddress :: Member (Lift IO) sems => Semantic (Address ': sems) a -> Semantic sems a
runAddress =
  interpret $ \case
  Resolve port -> do
    let hints = S.defaultHints { S.addrFlags = [S.AI_PASSIVE]
                               , S.addrSocketType = S.Stream
                               }
    addr <- sendM $ S.getAddrInfo (Just hints) Nothing (Just port)
    return $ head addr

  Open addr -> do
    sock <- sendM $ S.socket (S.addrFamily addr) (S.addrSocketType addr) (S.addrProtocol addr)
    sendM $ S.setSocketOption sock S.ReuseAddr 1
    sendM $ S.bind sock (S.addrAddress addr)

    let fd = S.fdSocket sock
    sendM $ S.setCloseOnExecIfNeeded fd
    sendM $ S.listen sock 10
    return sock

  Close socket -> sendM $ S.close socket

  Accept socket -> 
    sendM $ S.accept socket


