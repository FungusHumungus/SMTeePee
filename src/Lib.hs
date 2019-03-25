module Lib
    ( runSMTeePee
    ) where

import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import Control.Monad.Freer
import Control.Monad.Freer.TH
import qualified Control.Monad.Freer.Error as E
import qualified Control.Monad.Freer.Reader as R
import qualified Control.Monad.Freer.State as S 
import qualified Control.Monad.Freer.Writer as W
import qualified Data.Text as T
import Network.Socket (Socket, withSocketsDo)
import Effects.Client (Client(..), runClientSocket)
import Effects.Address (Address(..), runAddress, resolve, open, close, accept)
import Effects.Fork (Fork(..), runFork, forkFinally)
import Effects.DumpMessage (DumpMessage(..), runDumpMessage, dumpMessage)
import Effects.Log (Log(..), runLog, logMessage)
import Transport (step)
import Options.Applicative ( Parser, strOption, long, short, help
                           , showDefault, value, execParser
                           , info, helper, fullDesc, progDesc
                           , (<**>) )
import State (Env(..), State(..), Current(..), Message(..))
import System.FilePath ((<.>), (</>))



runServer :: Member IO effs
          => Member (R.Reader Env) effs
          => Member Address effs
          => Member Fork effs
          => Member Log effs
          => Eff effs ()
runServer = do
  port <- R.asks _port
  addr <- resolve (T.unpack port)

  logMessage $ "Listening on port " <> port

  --withRunInIO $ \runInIO ->
  --  E.bracket (open addr) close (runInIO . loop)
  socket <- open addr
  loop socket


-- | Continuously listen for incoming connections.
loop :: Member (R.Reader Env) effs
     => Member Address effs
     => Member Log effs
     => Member Fork effs
     => Socket
     -> Eff effs ()
loop sock = forever $ do
  (conn, peer) <- accept sock
  env <- R.ask
  logMessage $ "Connection from " <> (T.pack . show) peer
  _ <- forkFinally (threadEffs conn env) runThread (const $ closeSocket conn)

  return ()


closeSocket :: Member Address eff
            => Socket
            -> Eff eff () 
closeSocket conn =
   close conn


emptyMessage :: Message
emptyMessage = Message { _from = ""
                       , _to = []
                       , _data = "" }


-- | Build up the Effs we want in our thread
threadEffs :: Socket -> Env -> Eff '[Address, Client, R.Reader Env, S.State State, DumpMessage, IO] a -> IO a
threadEffs sock env eff = 
  fst <$> ( runM $ runDumpMessage $ (S.runState state) $ (R.runReader env) $ runClientSocket sock $ runAddress eff )
  where
    state = State { _current = SendGreeting
                  , _message = emptyMessage }



-- | We need to set up a new effect chain here.
-- As far as I can tell effects won't work very well over thread boundaries.
runThread :: Member (R.Reader Env) effs
          => Member (S.State State) effs
          => Member Client effs
          => Member DumpMessage effs
          => Member Address effs
          => Eff effs ()
runThread = do
  state <- S.gets _current
  step state
  if state == End
    then do path <- R.asks _output
            msg <- S.gets (_data . _message)
            dumpMessage path msg
    else runThread 


args :: Parser Env
args = Env <$> strOption ( long "port"
                           <> short 'p'
                           <> showDefault <> value "28"
                           <> help "The port to listen on" )
       <*> strOption ( long "output"
                       <> short 'o'
                       <> showDefault <> value "./received/"
                       <> help "The folder to dump the received emails" )
       <*> strOption ( long "domain"
                       <> short 'd'
                       <> showDefault <> value "smtp.ponk.com"
                       <> help "The domain to say we are from" )
       <*> strOption ( long "app"
                       <> short 'a'
                       <> showDefault <> value "SMTeepee"
                       <> help "The app we say we are" )


runSMTeePee :: IO ()
runSMTeePee = do
  env <- execParser opts
  withSocketsDo $
    runM $ (R.runReader env) $ runLog $ runFork $ runAddress runServer

  where
    opts = info ( args <**> helper )
      ( fullDesc <>
      progDesc "Run a fake smtp server" )


