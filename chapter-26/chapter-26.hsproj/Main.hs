{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE InstanceSigs #-}

import Control.Monad.Except
import MaybeT
import ReaderT

import qualified Light2

-- Exercise: Wrap It Up
-- PDF page 1058
embedded :: MaybeT 
              (ExceptT String (ReaderT () IO))
              Int
embedded = MaybeT $ ExceptT $ ReaderT $ (const $ pure (Right (Just 1)))

main :: IO ()
main = Light2.main