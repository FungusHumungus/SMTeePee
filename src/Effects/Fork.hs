{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

-- | 

module Effects.Fork where

import Polysemy
import Polysemy.Effect.TH
import Polysemy.Effect.New
import qualified Control.Concurrent as C
import           Control.Monad (void)
import GHC.Exception.Type (SomeException)

import qualified Control.Exception as X

import Data.Coerce

data Fork m a 
  = forall r. ForkFinally ( m r ) ( Either SomeException r -> m () ) ( C.ThreadId -> a )

deriving instance Functor (Fork m)

ook_and_then :: ( Functor s, Functor m, Functor n)
             => s ()
             -> ( forall x. s (m x) -> n (s x) )
             -> ( Either SomeException r -> m () )
             -> Either SomeException (s r)
             -> n () 
ook_and_then s f and_then ( Right sr ) =
  void $ f $ fmap ( and_then . Right ) sr

ook_and_then s f and_then ( Left z ) =
  void $ f $ (<$ s) $ and_then ( Left z )
  

ook_k :: ( Functor s )
      => s ()
      -> ( C.ThreadId -> a )
      -> C.ThreadId
      -> (s a) 
ook_k s k st = k <$> (st <$ s)


instance Effect Fork where
  weave s f (ForkFinally action and_then k) = 
    ForkFinally
    ( f $ action <$ s )
    ( ook_and_then s f and_then )
    ( ook_k s k )
    

  hoist  = defaultHoist
    

makeSemantic ''Fork 


runFork :: forall r a
           . Member (Lift IO) r
        => (forall x. Semantic r x -> IO x)
        -> Semantic (Fork ': r) a
        -> Semantic r a
runFork unlift = interpret $ \case
  ForkFinally action and_then k ->  do
    thread <- sendM $ C.forkFinally (runIt action) (runIt . and_then)
    return $ k thread
      where
        runIt :: Semantic (Fork ': r) x -> IO x
        runIt = unlift . runFork unlift
