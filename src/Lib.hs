module Lib
    ( runSMTeePee
    ) where

import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import Polysemy
import qualified Polysemy.Reader as R
import qualified Polysemy.State as S 
import qualified Data.Text as T
import Network.Socket (Socket, withSocketsDo)
import Effects.ConnectedClient (ConnectedClient, runConnectedClient)
import Effects.DumpMessage (DumpMessage, runDumpMessage, dumpMessage)
import Effects.Log (Log, runLog, logMessage)
import Effects.Server (Client, Server, runTcpServer, runServer)
import Transport (step)
import Options.Applicative ( Parser, strOption, long, short, help
                           , showDefault, value, execParser
                           , info, helper, fullDesc, progDesc
                           , (<**>) )
import State (Env(..), State(..), Current(..), Message(..))
import System.FilePath ((<.>), (</>))


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

  let lower = runM . S.runState (Nothing :: Maybe Socket)
      lower2 = lower . runTcpServer ( T.unpack $ _port env ) lower 
      sems = lower2 . runConnectedClient . runLog . runDumpMessage . S.runState state . R.runReader env $ runServerOok
  
  sems >> return ()

  where
    opts = info
           ( args <**> helper )
           ( fullDesc <>
             progDesc "Run a fake smtp server" )

    emptyMessage :: Message
    emptyMessage = Message { _from = ""
                           , _to = []
                           , _data = "" }
  
    state = State { _current = SendGreeting
                  , _message = emptyMessage }


runServerOok :: Member (R.Reader Env) sems
             => Member Log sems
             => Member (S.State State) sems
             => Member DumpMessage sems
             => Member Server sems
             => Member ConnectedClient sems
             => Semantic sems ()
runServerOok =
  logMessage "Listening..." >>
  runServer runThread
 

runThread :: Member (R.Reader Env) sems
          => Member (S.State State) sems
          => Member DumpMessage sems
          => Member Log sems
          => Member ConnectedClient sems
          => Client tok
          -> Semantic sems ()
runThread token = do
      state <- S.gets _current
      step token state
      if state == End
        then do path <- _output <$> R.ask
                msg <- S.gets (_data . _message)
                dumpMessage path msg
        else runThread token


