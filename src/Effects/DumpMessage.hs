module Effects.DumpMessage where

import Control.Monad.Freer (Member, Eff, interpret, send)
import Control.Monad.Freer.TH (makeEffect)
import qualified Data.Text as T
import Data.Time.Clock.POSIX as Time
import System.FilePath ((<.>), (</>))

-- | Effect to output the whole email message to whereever it is 
-- supposed to go.
data DumpMessage r where
  DumpMessage :: FilePath -> T.Text -> DumpMessage ()

makeEffect ''DumpMessage

  
runDumpMessage :: Member IO effs => Eff (DumpMessage ': effs) a -> Eff effs a
runDumpMessage =
  interpret $ \case
  DumpMessage path msg -> do
    ts <- send Time.getPOSIXTime
    let path = path </> show ts <.> "eml"
    send $ writeFile path $ T.unpack msg

