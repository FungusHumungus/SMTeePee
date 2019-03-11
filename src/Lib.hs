{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Lib
    ( run
    ) where

import Control.Concurrent (ThreadId, forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import Control.Monad.Reader (MonadReader(..), ReaderT(..), runReaderT, asks, lift)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.IO.Unlift (MonadUnliftIO(..), UnliftIO(..), unliftIO, withUnliftIO)
import Control.Monad.State.Strict (MonadState(..), StateT(..), evalStateT, gets, modify')
import qualified Data.ByteString as S
import qualified Data.Char as C
import qualified Data.Text as T
import Data.Time.Clock.POSIX as Time
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import GHC.Generics
import Client (ClientM(..))
import Transport (step)
import State (Env(..), State(..), Current(..), Message(..))
import System.FilePath ((<.>), (</>))

newtype SmtpM a = SmtpM { runSmtpM :: (ReaderT Env IO a) }
  deriving (Functor, Applicative, Monad, MonadReader Env)

instance MonadIO SmtpM where
  liftIO = SmtpM . liftIO 

instance MonadUnliftIO SmtpM where
  askUnliftIO = SmtpM $ withUnliftIO $ \u ->
        return (UnliftIO (unliftIO u . runSmtpM))


runServer :: SmtpM ()
runServer = do
  port <- asks _port
  addr <- liftIO $ resolve (T.unpack port)
  withRunInIO $ \runInIO ->
    E.bracket (open addr) close (runInIO . loop)
  --socket <- liftIO $ open addr
  --loop socket


resolve :: ServiceName -> IO AddrInfo
resolve port = do
  let hints = defaultHints {
        addrFlags = [AI_PASSIVE]
        , addrSocketType = Stream
        }
  addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
  return addr

open :: AddrInfo -> IO Socket
open addr = do
  sock <- liftIO $ socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress addr)

  let fd = fdSocket sock
  setCloseOnExecIfNeeded fd
  listen sock 10
  return sock

loop :: Socket -> SmtpM ()
loop sock = forever $ do
  (conn, peer) <- liftIO $ accept sock
  liftIO $ putStrLn $ "Connection from " ++ show peer
  void $ smtpMForkFinally2 (talk conn) (\_ -> close conn)

smtpMForkFinally :: SmtpM m -> (Either E.SomeException m -> IO ()) -> SmtpM ThreadId 
smtpMForkFinally action and_then = SmtpM $ ReaderT $ run
  where
    run :: Env -> IO ThreadId
    run env = forkFinally ( runReaderT ( runSmtpM action ) env ) and_then

smtpMForkFinally2 :: SmtpM m -> (Either E.SomeException m -> IO ()) -> SmtpM ThreadId 
smtpMForkFinally2 action and_then =  
  withRunInIO $ \runInIO -> forkFinally (runInIO action) and_then


emptyMessage :: Message
emptyMessage = Message { _from = ""
                       , _to = []
                       , _data = "" }


newtype ThreadStateM a = ThreadStateM { runThreadStateM :: (StateT State SmtpM a) }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState State, MonadReader Env)


trimNewlines :: T.Text -> T.Text
trimNewlines = T.dropWhile isLineSeparator . T.dropWhileEnd isLineSeparator
  where
   isLineSeparator :: Char -> Bool
   isLineSeparator c = case c of
                         '\r' -> True
                         '\n' -> True
                         _ -> False


instance ClientM ThreadStateM where
  getMessage = do
    socket <- gets _socket
    input <- liftIO $ recv socket 4096
    let input' = trimNewlines $ decodeUtf8 input
    liftIO $ putStrLn $ T.unpack $ "C: '" <> input' <> "'"
    return $ input'

  sendMessage msg = do
    socket <- gets _socket
    liftIO $ putStrLn $ T.unpack $ "S: " <> msg
    liftIO $ sendAll socket $ encodeUtf8 $ msg <> "\n"
            


talk :: Socket -> SmtpM ()
talk conn = do
  evalStateT ( runThreadStateM $ runThread conn ) $ State { _socket = conn
                                                          , _current = SendGreeting
                                                          , _message = emptyMessage }

dumpMessage :: FilePath -> T.Text -> IO ()
dumpMessage dir msg = do
  ts <- Time.getPOSIXTime
  let path = dir </> show ts <.> "eml"
  writeFile path $ T.unpack msg


runThread :: Socket -> ThreadStateM ()
runThread conn = do
  state <- gets _current
  (liftIO . putStrLn) $ show state
  step state
  if state == End
    then do path <- asks _output
            msg <- gets (_data . _message)
            (liftIO . dumpMessage path) msg
    else runThread conn

run :: IO ()
run = withSocketsDo $ do
  let
    env = Env { _port = "28"
              , _output = "./received/"
              , _domain = "smtp.ponk.com"
              , _app = "Smteepee"
              , _version = "0.1" }
  putStrLn $ "Listening on port " <> (T.unpack $ _port env)
  runReaderT (runSmtpM runServer) env 


