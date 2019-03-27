{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

-- | 

module Effects.Fork where

import Polysemy
import Polysemy.Effect.TH
import Polysemy.Effect.New
import qualified Control.Concurrent as C
import GHC.Exception.Type (SomeException)


data Fork m a where
  ForkFinally :: (forall a . ( Semantic effs a -> IO a ) )
              -> Semantic effs a
              -> ( Either SomeException a -> Semantic effs () )
              -> ( C.ThreadId -> a )
              -> Fork m a
  deriving ( Effect)

deriving instance Functor (Fork m)

makeSemantic ''Fork

runFork :: Member IO effs => Eff (Fork ': effs) a -> Eff effs a
runFork = interpret $ \case
  ForkFinally unlift run excpt -> do
    send $ C.forkFinally (unlift run) (unlift . excpt)
