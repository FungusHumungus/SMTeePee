module Effects.Log where

import Polysemy
import qualified Data.Text as T

data Log m a where
  LogMessage :: T.Text -> Log m ()

makeSemantic ''Log
  

runLog :: Member (Lift IO) sems => Semantic (Log ': sems) a -> Semantic sems a
runLog =
  interpret $ \case
  LogMessage msg -> sendM $ ( putStrLn . T.unpack ) msg 
