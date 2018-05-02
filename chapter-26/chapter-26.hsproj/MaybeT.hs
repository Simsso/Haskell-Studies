{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE InstanceSigs #-}

module MaybeT where
  
import MonadTrans
import Control.Monad (liftM)
import Control.Monad.IO.Class

-- Structure: MaybeT > Monad > Maybe > Value
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
  pure x = MaybeT (pure (pure x))
  (MaybeT fab) <*> (MaybeT mma) = MaybeT $ (<*>) <$> fab <*> mma

instance (Monad m) => Monad (MaybeT m) where
  return = pure
  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (>>=) (MaybeT ma) f = MaybeT $ ma >>= notjus where
    notjus Nothing = return Nothing
    notjus (Just x) = runMaybeT (f x)
    
instance MonadTrans MaybeT where
  lift = MaybeT . liftM Just
  
instance (MonadIO m) => MonadIO (MaybeT m) where  liftIO io = lift . liftIO $ io