{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE InstanceSigs #-}

module MonadTrans where

class MonadTrans t where
  lift :: (Monad m) => m a -> t m a