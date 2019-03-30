module Lib
    ( runSMTeePee
    ) where

import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import Polysemy
import qualified Polysemy.Reader as R
import qualified Polysemy.Resource as R
import qualified Polysemy.State as S 
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



runServer :: Member (R.Reader Env) sems
          => Member Address sems
          => Member R.Resource sems
          => Member Fork sems
          => Member Log sems
          => Member (Lift IO) sems
          => Member (S.State State) sems
          => Member DumpMessage sems
          => Semantic sems ()
runServer = do
  port <- _port <$> R.ask
  addr <- resolve (T.unpack port)

  logMessage $ "Listening on port " <> port

  R.bracket (open addr) close loop


-- | Continuously listen for incoming connections.
loop :: Member (R.Reader Env) sems
     => Member Address sems
     => Member Log sems
     => Member Fork sems
     => Member (Lift IO) sems
     => Member (S.State State) sems
     => Member DumpMessage sems
     => Socket
     -> Semantic sems ()
loop sock = forever $ do
  (conn, peer) <- accept sock
  logMessage $ "Connection from " <> (T.pack . show) peer
  _ <- forkFinally (runThread conn) (const $ closeSocket conn)

  return ()


closeSocket :: Member Address sems
            => Socket
            -> Semantic sems () 
closeSocket conn =
   close conn


emptyMessage :: Message
emptyMessage = Message { _from = ""
                       , _to = []
                       , _data = "" }


runThread :: Member (R.Reader Env) sems
          => Member (S.State State) sems
          => Member DumpMessage sems
          => Member Address sems
          => Member Log sems
          => Member (Lift IO) sems
          => Socket
          -> Semantic sems ()
runThread conn = runClientSocket conn go
  where
    go :: Member (R.Reader Env) sems
       => Member (S.State State) sems
       => Member Client sems
       => Member DumpMessage sems
       => Member Address sems
       => Member Log sems
       => Semantic sems ()
    go = do
      state <- S.gets _current
      step state
      if state == End
        then do path <- _output <$> R.ask
                msg <- S.gets (_data . _message)
                dumpMessage path msg
        else go


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

    let
      lower = runM . runFork runM
      sems = lower . R.runResource lower . runLog . runDumpMessage . S.runState state . R.runReader env . runAddress $ runServer
    in
      sems >> return ()

  where
    opts = info
           ( args <**> helper )
           ( fullDesc <>
             progDesc "Run a fake smtp server" )
  
    state = State { _current = SendGreeting
                  , _message = emptyMessage }


