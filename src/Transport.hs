{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- | 

module Transport where

import Control.Monad.Reader (MonadReader(..), ReaderT(..), runReaderT, asks, lift)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.IO.Unlift (MonadUnliftIO(..), UnliftIO(..), unliftIO, withUnliftIO)
import Control.Monad.State.Strict (MonadState(..), StateT(..), evalStateT, gets, modify')
import qualified Data.ByteString as S
import qualified Data.Text as T
import Client (ClientM(..))
import State (Env(..), State(..), Message(..), Current(..))


step :: ( MonadReader Env m
        , MonadState State m
        , ClientM m )
     => Current
     -> m ()
step SendGreeting = do
  domain <- asks _domain
  app <- asks _app
  sendMessage $ "220 " <> domain <> " ESMTP " <> app
  modify' (\st -> st { _current = ReceiveGreeting })


step ReceiveGreeting = do
  input <- getMessage 
  if T.isPrefixOf "HELO" input
    then modify' (\st -> st { _current = Accepted })
    else modify' (\st -> st { _current = Rejected })


step Accepted = do
  domain <- asks _domain
  sendMessage $ "250 " <> domain <> ", I am glad to meet you"
  modify' (\st -> st { _current = Accept })


step Accept = do
  input <- getMessage
  if T.isPrefixOf "MAIL FROM:" input
    then do sendMessage "250 OK"
            modify' (\st -> st { _message = (_message st) { _from = input }})
    else if T.isPrefixOf "RCPT TO:" input
    then do sendMessage "250 OK"
            modify' (\st -> let message = _message st in
                        st { _message = message { _to = input : (_to message) } })
    else if T.isPrefixOf "DATA" input
    then do sendMessage "354 End data with <CR><LF>.<CR><LF>"
            modify' (\st -> st { _current = AcceptData })
    else if T.isPrefixOf "QUIT" input
    then modify' (\st -> st { _current = End })
    else if input == ""
    then modify' (\st -> st { _current = End })
    else pure ()


step AcceptData = do
  input <- getMessage
  let datums = T.unlines $ takeWhile (/= ".") $ T.lines input

  modify' $ addDataLine datums

  let remainder = dropWhile (/= ".") $ T.lines input
  if remainder == ["."]
    then do sendMessage "250 Ok: queued as plork"
            modify' (\st -> st { _current = Accept })
    else return ()
  

step Rejected = do
  sendMessage "Boo"
  modify' (\st -> st { _current = End})


step End = 
  sendMessage "221 Bye"


addDataLine :: T.Text -> State -> State
addDataLine line st = 
  let message = _message st in
    st { _message = message { _data = T.append (_data message) line  } }
