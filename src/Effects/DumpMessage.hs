module Effects.DumpMessage where

import Polysemy
import qualified Data.Text as T
import Data.Time.Clock.POSIX as Time
import System.FilePath ((<.>), (</>))

-- | Effect to output the whole email message to whereever it is 
-- supposed to go.
data DumpMessage m a where
  DumpMessage :: FilePath -> T.Text -> DumpMessage m ()

makeSemantic ''DumpMessage

  
runDumpMessage :: Member (Lift IO) effs => Semantic (DumpMessage ': effs) a -> Semantic effs a
runDumpMessage =
  interpret $ \case
  DumpMessage path msg -> do
    ts <- sendM Time.getPOSIXTime
    let path' = path </> show ts <.> "eml"
    sendM . writeFile path' $ T.unpack msg
    sendM . putStrLn $ T.unpack msg

