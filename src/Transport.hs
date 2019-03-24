-- | 

module Transport where

import Control.Monad.Freer
import Control.Monad.Freer.TH
import qualified Control.Monad.Freer.Error as E
import qualified Control.Monad.Freer.Reader as R
import qualified Control.Monad.Freer.State as S 
import qualified Control.Monad.Freer.Writer as W
import qualified Data.ByteString as S
import qualified Data.Text as T
import Effects.Client (Client(..), getMessage, sendMessage)
import State (Env(..), State(..), Message(..), Current(..))

-- | Handles the various stages of the SMTP protocol.
-- The current state is held in the State effect.
step :: Member (R.Reader Env) effs
     => Member (S.State State) effs
     => Member Client effs
     => Current
     -> Eff effs ()
step SendGreeting = do
  domain <- R.asks _domain
  app <- R.asks _app
  sendMessage $ "220 " <> domain <> " ESMTP " <> app
  S.modify (\st -> st { _current = ReceiveGreeting })


step ReceiveGreeting = do
  input <- getMessage 
  if T.isPrefixOf "HELO" input
    then S.modify (\st -> st { _current = Accepted })
    else S.modify (\st -> st { _current = Rejected })


step Accepted = do
  domain <- R.asks _domain
  sendMessage $ "250 " <> domain <> ", I am glad to meet you"
  S.modify (\st -> st { _current = Accept })


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
            S.modify (\st -> st { _current = AcceptData })
    else if T.isPrefixOf "QUIT" input
    then S.modify (\st -> st { _current = End })
    else if input == ""
    then S.modify (\st -> st { _current = End })
    else pure ()


step AcceptData = do
  input <- getMessage
  let datums = T.unlines $ takeWhile (/= ".") $ T.lines input

  S.modify $ addDataLine datums

  let remainder = dropWhile (/= ".") $ T.lines input
  if remainder == ["."]
    then do sendMessage "250 Ok: queued as plork"
            S.modify (\st -> st { _current = Accept })
    else return ()
  

step Rejected = do
  sendMessage "Boo"
  S.modify (\st -> st { _current = End})


step End = 
  sendMessage "221 Bye"


addDataLine :: T.Text -> State -> State
addDataLine line st = 
  let message = _message st in
    st { _message = message { _data = T.append (_data message) line  } }
