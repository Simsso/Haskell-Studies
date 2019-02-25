{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE InstanceSigs #-}

module EitherT where
  
import Control.Monad (liftM)
import MonadTrans

-- Structure: EitherT > Monad > Either
newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT a) = EitherT $ (fmap . fmap) f a
  
instance Applicative m => Applicative (EitherT e m) where
  pure = pure
  (EitherT f) <*> (EitherT a) = EitherT $ (<*>) <$> f <*> a
  
instance Monad m => Monad (EitherT e m) where
  return = pure
  (>>=) (EitherT ea) f = EitherT $ ea >>= bind where
    bind (Right x) = runEitherT (f x)
    bind (Left x) = return $ Left x
  
swapEither :: Either a b -> Either b a
swapEither (Left a) = Right a
swapEither (Right a) = Left a

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT a) = EitherT $ fmap swapEither a

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT amc bmc (EitherT x) = x >>= either amc bmc

instance MonadTrans (EitherT e) where  lift m = EitherT $ liftM Right m