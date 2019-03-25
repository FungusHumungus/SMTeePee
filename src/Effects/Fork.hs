{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

-- | 

module Effects.Fork where

import Control.Monad.Freer
import qualified Control.Monad.Freer as FR
import Control.Monad.Freer.TH (makeEffect)
import qualified Control.Concurrent as C
import GHC.Exception.Type (SomeException)


data Fork r where
  ForkFinally :: (forall a . ( Eff effs a -> IO a ) ) -> Eff effs a -> (Either SomeException a -> Eff effs ()) -> Fork C.ThreadId

makeEffect ''Fork

runFork :: Member IO effs => Eff (Fork ': effs) a -> Eff effs a
runFork = interpret $ \case
  ForkFinally unlift run excpt -> do
    send $ C.forkFinally (unlift run) (unlift . excpt)
