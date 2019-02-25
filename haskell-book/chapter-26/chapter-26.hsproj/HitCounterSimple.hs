{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module HitCounterSimple where
  
import Control.Monad.Reader (ReaderT, asks, lift, runReaderT)
import Data.IORef
import Data.Text.Lazy (Text, pack)
import Web.Scotty.Trans

data Config = Config { counts :: IORef Integer }

-- atomic increment
aInc :: Integer -> (Integer, Integer)
aInc = (,) <*> id <$> id . (+1)

app :: ScottyT Text (ReaderT Config IO) ()
app = get "/:key" $ do
  i <- lift $ asks counts -- read counter
  i' <- lift . lift $ atomicModifyIORef' i aInc -- increment counter
  text $ pack $ show i' -- respond with new value

main :: IO ()
main = do
  counter <- newIORef 0
  let config = Config counter
      runR m = runReaderT m config
  scottyT 3000 runR app