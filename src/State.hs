-- | 

module State where

import Data.Text as T
import Network.Socket hiding (recv)


data Env = Env { _port :: T.Text
               , _output :: FilePath
               , _domain :: T.Text
               , _app :: T.Text
               , _version :: T.Text }


data Current = SendGreeting
             | ReceiveGreeting
             | Accepted
             | Rejected
             | Accept
             | AcceptData
             | End
             deriving (Eq, Show)


data Message =
  Message { _from :: T.Text
          , _to :: [T.Text]
          , _data :: T.Text
          }
  deriving (Show)


data State =
  State { _socket :: Socket
        , _current :: Current
        , _message :: Message
        }

