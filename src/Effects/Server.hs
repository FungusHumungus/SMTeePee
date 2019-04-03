{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Effects.Server where

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


data Client tok = Client

-- | Effect to handle the running of a tcp server.
data Server m a where
  RunServer :: ((forall tok. Client tok) -> m ()) -> a -> Server m a


instance Functor (Server m) where
  fmap f (RunServer fn a) = RunServer fn $ f a


instance Effect Server where
  weave s f (RunServer action k) =
    RunServer
    (\client -> void . f $ action client <$ s)
    (k <$ s)
  

  hoist = defaultHoist

makeSemantic ''Server

runTcpServer :: forall sems a
             . Member (Lift IO) sems
             => Member (State.State (Maybe S.Socket)) sems
             => S.ServiceName
             -> (forall x. Semantic sems x -> IO (Maybe Socket, x))
             -> Semantic (Server ': sems) a
             -> Semantic sems a
runTcpServer port unlift = interpret $ \case
  RunServer action k -> sendM $ S.withSocketsDo $ do
    let hints = S.defaultHints { S.addrFlags = [S.AI_PASSIVE]
                               , S.addrSocketType = S.Stream
                               }
    addr <- S.getAddrInfo (Just hints) Nothing (Just port)
    bracket (open $ head addr) S.close loop

    return k

    where
      loop sock = forever $ do
        (conn, peer) <- S.accept sock
        _ <- forkFinally (runIt conn $ action Client) (const $ S.close conn)
        return ()

      runIt :: Member (State.State (Maybe S.Socket)) sems
            => S.Socket
            -> Semantic (Server ': sems) x
            -> IO (Maybe Socket, x)
      runIt conn mon = ( unlift . runTcpServer port unlift ) $ do
        State.put $ Just conn
        mon

      open addr = do
        sock <- S.socket (S.addrFamily addr) (S.addrSocketType addr) (S.addrProtocol addr)
        S.setSocketOption sock S.ReuseAddr 1
        S.bind sock (S.addrAddress addr)

        let fd = S.fdSocket sock
        S.setCloseOnExecIfNeeded fd
        S.listen sock 10
        return sock


