{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE InstanceSigs #-}

module HitCounter where
  
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT, asks, lift, runReaderT)
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

data Config = Config {
  counts :: IORef (M.Map Text Integer)
, prefix :: Text
}

type Scotty = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m = let stored = fromMaybe 0 $ M.lookup k m
                    new = stored + 1
                in  (M.insert k new m, new)

app :: Scotty ()
app = get "/:key" $ do
  unprefixed <- param "key"
  prefixval <- lift $ asks prefix
  iorefmap <- lift $ asks counts
  let key' = mappend prefixval unprefixed
  c <- liftIO $ atomicModifyIORef' iorefmap $ bumpBoomp key'
  html $ mconcat [
    "<h1>Success! Count was: ", 
    TL.pack $ show c , 
    "</h1>"]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter (TL.pack prefixArg)
      runR m = runReaderT m config
  scottyT 3000 runR app
  
