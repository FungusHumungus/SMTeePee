-- | 

module Transport where

import Polysemy
import qualified Polysemy.Reader as R
import qualified Polysemy.State as S 
import qualified Data.ByteString as S
import qualified Data.Text as T
import Effects.ConnectedClient (ConnectedClient, sendMessage, getMessage)
import Effects.Server (Client)
import State (Env(..), State(..), Message(..), Current(..))

-- | Udpate the state with the next step.
next :: Member (S.State State) sems
     => Current 
     -> Semantic sems ()
next current = S.modify (\st -> st { _current = current })


-- | Handles the various stages of the SMTP protocol.
-- The current state is held in the State effect.
step :: Member (R.Reader Env) sems
     => Member (S.State State) sems
     => Member ConnectedClient sems
     => Client tok
     -> Current
     -> Semantic sems ()
step tok SendGreeting = do
  domain <- _domain <$> R.ask
  app <- _app <$> R.ask
  sendMessage tok $ "220 " <> domain <> " ESMTP " <> app
  next ReceiveGreeting


step tok ReceiveGreeting = do
  input <- getMessage tok
  if T.isPrefixOf "HELO" input
    then next Accepted
    else next Rejected


step tok Accepted = do
  domain <- _domain <$> R.ask 
  sendMessage tok $ "250 " <> domain <> ", I hope this day finds you well."
  next Accept


step tok Accept = do
  input <- getMessage tok
  if T.isPrefixOf "MAIL FROM:" input
    then do sendMessage tok "250 OK"
            S.modify (\st -> st { _message = (_message st) { _from = input }})
    else if T.isPrefixOf "RCPT TO:" input
    then do sendMessage tok "250 OK"
            S.modify (\st -> let message = _message st in
                        st { _message = message { _to = input : (_to message) } })
    else if T.isPrefixOf "DATA" input
    then do sendMessage tok "354 End data with <CR><LF>.<CR><LF>"
            next AcceptData
    else if T.isPrefixOf "QUIT" input
    then next End
    else if input == ""
    then next End
    else pure ()


step tok AcceptData = do
  input <- getMessage tok
  let datums = T.unlines $ takeWhile (/= ".") $ T.lines input

  S.modify $ addDataLine datums

  let remainder = dropWhile (/= ".") $ trimNewLines <$> T.lines input
  if remainder == ["."]
    then do sendMessage tok "250 Ok: queued as plork"
            next Accept
    else return ()
  

step tok Rejected = do
  sendMessage tok "Boo"
  next End


step tok End = 
  sendMessage tok "221 Bye"


trimNewLines :: T.Text -> T.Text
trimNewLines = T.dropWhile isNewLine . T.dropWhileEnd isNewLine 
  where
    isNewLine c = c == '\n' || c == '\r'

addDataLine :: T.Text -> State -> State
addDataLine line st = 
  let message = _message st in
    st { _message = message { _data = T.append (_data message) line  } }
