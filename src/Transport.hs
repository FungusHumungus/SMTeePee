-- | 

module Transport where

import Polysemy
import qualified Polysemy.Reader as R
import qualified Polysemy.State as S 
import qualified Data.ByteString as S
import qualified Data.Text as T
import Effects.Client (Client(..), getMessage, sendMessage)
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
     => Member Client sems
     => Current
     -> Semantic sems ()
step SendGreeting = do
  domain <- _domain <$> R.ask
  app <- _app <$> R.ask
  sendMessage $ "220 " <> domain <> " ESMTP " <> app
  next ReceiveGreeting


step ReceiveGreeting = do
  input <- getMessage 
  if T.isPrefixOf "HELO" input
    then next Accepted
    else next Rejected


step Accepted = do
  domain <- _domain <$> R.ask 
  sendMessage $ "250 " <> domain <> ", I hope this day finds you well."
  next Accept


step Accept = do
  input <- getMessage
  if T.isPrefixOf "MAIL FROM:" input
    then do sendMessage "250 OK"
            S.modify (\st -> st { _message = (_message st) { _from = input }})
    else if T.isPrefixOf "RCPT TO:" input
    then do sendMessage "250 OK"
            S.modify (\st -> let message = _message st in
                        st { _message = message { _to = input : (_to message) } })
    else if T.isPrefixOf "DATA" input
    then do sendMessage "354 End data with <CR><LF>.<CR><LF>"
            next AcceptData
    else if T.isPrefixOf "QUIT" input
    then next End
    else if input == ""
    then next End
    else pure ()


step AcceptData = do
  input <- getMessage
  let datums = T.unlines $ takeWhile (/= ".") $ T.lines input

  S.modify $ addDataLine datums

  let remainder = dropWhile (/= ".") $ trimNewLines <$> T.lines input
  if remainder == ["."]
    then do sendMessage "250 Ok: queued as plork"
            next Accept
    else return ()
  

step Rejected = do
  sendMessage "Boo"
  next End


step End = 
  sendMessage "221 Bye"


trimNewLines :: T.Text -> T.Text
trimNewLines = T.dropWhile isNewLine . T.dropWhileEnd isNewLine 
  where
    isNewLine c = c == '\n' || c == '\r'

addDataLine :: T.Text -> State -> State
addDataLine line st = 
  let message = _message st in
    st { _message = message { _data = T.append (_data message) line  } }
