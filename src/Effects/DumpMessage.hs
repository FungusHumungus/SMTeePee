module Effects.DumpMessage where

import Polysemy
import Polysemy.Effect.TH
import Polysemy.Effect.New
import qualified Data.Text as T
import Data.Time.Clock.POSIX as Time
import System.FilePath ((<.>), (</>))

-- | Effect to output the whole email message to whereever it is 
-- supposed to go.
data DumpMessage m a where
  DumpMessage :: FilePath -> T.Text -> a -> DumpMessage m a
  deriving (Functor, Effect)

makeSemantic ''DumpMessage

  
runDumpMessage :: Member (Lift IO) effs => Semantic (DumpMessage ': effs) a -> Semantic effs a
runDumpMessage =
  interpret $ \case
  DumpMessage path msg k -> do
    ts <- sendM Time.getPOSIXTime
    let path' = path </> show ts <.> "eml"
    sendM . writeFile path' $ T.unpack msg
    sendM . putStrLn $ T.unpack msg
    return k

