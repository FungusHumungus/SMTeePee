module Effects.Log where

import Control.Monad.Freer (Member, Eff, interpret, send)
import Control.Monad.Freer.TH (makeEffect)
import qualified Data.Text as T

data Log r where
  LogMessage :: T.Text -> Log ()

makeEffect ''Log
  
runLog :: Member IO effs => Eff (Log ': effs) a -> Eff effs a
runLog =
  interpret $ \case
  LogMessage msg -> send $ ( putStrLn . T.unpack ) msg
