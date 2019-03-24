module Effects.Address where

import Control.Monad.Freer
import qualified Control.Monad.Freer as FR
import Control.Monad.Freer.TH
import qualified Data.Text as T
import Network.Socket (AddrInfo, Socket, SockAddr)
import qualified Network.Socket as S
import Network.Socket.ByteString (recv, sendAll)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

data Address r where
  Resolve :: S.ServiceName -> Address AddrInfo
  Open :: AddrInfo -> Address Socket
  Close :: Socket -> Address ()
  Accept :: Socket -> Address (Socket, SockAddr)

makeEffect ''Address

runAddress :: Member IO effs => Eff (Address ': effs) a -> Eff effs a
runAddress =
  interpret $ \case
  Resolve port -> do
    let hints = S.defaultHints { S.addrFlags = [S.AI_PASSIVE]
                               , S.addrSocketType = S.Stream
                               }
    addr <- send $ S.getAddrInfo (Just hints) Nothing (Just port)
    return $ head addr

  Open addr -> do
    sock <- send $ S.socket (S.addrFamily addr) (S.addrSocketType addr) (S.addrProtocol addr)
    send $ S.setSocketOption sock S.ReuseAddr 1
    send $ S.bind sock (S.addrAddress addr)

    let fd = S.fdSocket sock
    send $ S.setCloseOnExecIfNeeded fd
    send $ S.listen sock 10
    return sock

  Close socket -> send $ S.close socket 

  Accept socket -> send $ S.accept socket


