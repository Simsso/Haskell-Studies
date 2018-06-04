{-# OPTIONS_GHC -Wall #-}

module Light2 where
  
data Light = On | Off

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT mas) = StateT $ \s -> tuplfst <$> (mas s) where
    tuplfst (ta, tb) = (f ta, tb)

instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  StateT g <*> StateT h = StateT $ \s -> do
    (f, s') <- g s
    (x, s'') <- h s'
    return (f x, s'')
    
instance (Monad m) => Monad (StateT s m) where
  return = pure
  StateT sma >>= f = StateT $ \s -> sma s >>= \(a, s') -> runStateT (f a) s'
  
lift :: (Monad m) => m a -> StateT s m a
lift m = StateT $ \s -> flip (,) s <$> m

light2bool :: Light -> Bool
light2bool On = True
light2bool  _ = False

lightT :: StateT Light IO Bool
lightT = StateT $ \s -> pure (light2bool s, s)

turnOnT :: StateT Light IO ()
turnOnT = StateT $ \_ -> pure ((), On)

turnOffT :: StateT Light IO ()
turnOffT = StateT $ \_ -> pure ((), Off)

switchT :: StateT Light IO ()
switchT = StateT $ \s -> 
  pure $ case s of
    On -> ((), Off)
    Off -> ((), On)
    
process :: StateT Light IO ()
process = do
  s <- lift getLine
  case s of
    "on"   -> turnOnT
    "off"  -> turnOffT
    "s"    -> switchT
    "l"    -> do
      state <- lightT -- read a :: Bool
      lift . putStrLn . show $ state
    "exit" -> return ()
    _      -> lift . putStrLn $ "invalid input"
  if s == "exit" then return () else process
    

main :: IO ()
main = do
  _ <- runStateT process Off
  return ()
  
