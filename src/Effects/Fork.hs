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
  -- = forall r. ForkFinally ( m r ) ( Either SomeException r -> m () ) ( C.ThreadId -> a )
  = forall r. ForkFinally ( m r ) ( r -> m () ) ( C.ThreadId -> a )

deriving instance Functor (Fork m)

ook_and_then :: ( Functor s, Functor m, Functor n)
             => s ()
             -> (forall x. s (m x) -> n (s x))
             -> ( Either SomeException r -> m () )
             -> Either SomeException (s r)
             -> n () 
ook_and_then s f and_then sr = 
  undefined
  -- This is a failure of a function...
  

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
    ( void . f . fmap and_then )
    -- ( ook_and_then s f and_then )
    ( ook_k s k )
    

  hoist f (ForkFinally action and_then k) =
    ForkFinally
    ( f action )
    ( fmap f and_then )
    k
    

makeSemantic ''Fork 


runFork :: forall r a
           . Member (Lift IO) r
        => (forall x. Semantic r x -> IO x)
        -> Semantic (Fork ': r) a
        -> Semantic r a
runFork unlift = interpret $ \case
  ForkFinally action and_then k ->  do
    thread <- sendM $ C.forkFinally (runIt action) right_and_then
    return $ k thread
      where
        runIt :: Semantic (Fork ': r) x -> IO x
        runIt = unlift . runFork unlift

        right_and_then ( Right x ) = ( runIt . and_then ) x
        right_and_then ( Left _ ) = return ()

