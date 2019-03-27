module Effects.Log where

import Polysemy
import Polysemy.Effect.TH
import Polysemy.Effect.New
import qualified Data.Text as T

data Log m a where
  LogMessage :: T.Text -> a -> Log m a 
  deriving (Functor, Effect)

makeSemantic ''Log
  

runLog :: Member (Lift IO) sems => Semantic (Log ': sems) a -> Semantic sems a
runLog =
  interpret $ \case
  LogMessage msg k -> sendM $ ( putStrLn . T.unpack ) msg >> return k
