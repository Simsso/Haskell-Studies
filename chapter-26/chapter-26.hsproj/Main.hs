{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE InstanceSigs #-}

import Control.Monad.Except
import MaybeT
import ReaderT

-- Exercise: Wrap It Up
-- PDF page 1058
embedded :: MaybeT 
              (ExceptT String (ReaderT () IO))
              Int
embedded = MaybeT $ ExceptT $ ReaderT $ (const $ pure (Right (Just 1)))