{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

-- | 

module Effects.Fork where

import           Polysemy
import qualified Control.Concurrent as C
import           Control.Monad (void)
import           GHC.Exception.Type (SomeException)

data Fork m a where
  ForkFinally :: m a -> ( Either SomeException a -> m () ) -> ( C.ThreadId -> a ) -> Fork m a

makeSemantic ''Fork 


runFork :: forall r a
           . Member (Lift IO) r
        => (forall x. Semantic r x -> IO x)
        -> Semantic (Fork ': r) a
        -> Semantic r a
runFork unlift = interpretH $ \case
  ForkFinally action and_then k -> do
    action' <- runT action
    and_then' <- bindT and_then
    
    thread <- sendM . C.forkFinally (runIt action') $ \case
      Right val -> void . runIt . and_then' $ Right <$> val
      Left err -> putStrLn $ show err
     
    pureT $ k thread

      where
        runIt :: Semantic (Fork ': r) x -> IO x
        runIt = unlift .@ runFork 


