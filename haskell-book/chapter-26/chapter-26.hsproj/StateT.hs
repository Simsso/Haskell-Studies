{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE InstanceSigs #-}

module StateT where
  
import Control.Arrow (first)
import Control.Monad (liftM)
import Control.Monad.IO.Class
import MonadTrans

newtype State s a = State { runState :: s -> (a, s) }

-- Structure: StateT function > Monad > Tuple
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT a) = StateT $ \s -> first f <$> a s
  
--instance (Monad m) => Applicative (StateT s m) where
--  pure a = StateT $ \s -> pure (a,s) -- is the s required?
--  (<*>) (StateT smfs) (StateT smas) = StateT $ \s -> smas s >>= mf s where
--    mf s (a,_) = do 
--      (f,_) <- smfs s
--      return (f a,s)
    --StateT $ (<*>) <$> smfs <*> smas
    --StateT $ \s -> (<*>) <$> (second const <$> smfs s) <*> (smas s)
    -- :t smfs :: s -> m (a -> b, s)
    
instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  StateT g <*> StateT h = StateT $ \s -> do
    (f, s') <- g s
    (x, s'') <- h s'
    return (f x, s'')
    
instance (Monad m) => Monad (StateT s m) where
  return = pure
  StateT sma >>= f = StateT $ \s -> sma s >>= \(a, s') -> runStateT (f a) s'
  
instance MonadTrans (StateT s) where  lift m = StateT $ \s -> liftM (flip (,)  s) m
  
instance (MonadIO m) => MonadIO (StateT s m) where  liftIO = lift . liftIO