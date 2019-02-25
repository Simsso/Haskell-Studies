-- Dominik's exercise #1

{-# OPTIONS_GHC -Wall #-}

module Light where
  
import Control.Monad

data Light = On | Off

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f (State g) = State $ \s0 ->
    let (a, s1) = g s0
    in (f a, s1)

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  (State f) <*> (State g) = State $ \s0 ->
    let (ab, s1) = f s0
        (a, s2) = g s1
    in (ab a, s2)

instance Monad (State s) where
  (State f) >>= g = State $ \ s0 ->
    let (a, s1) = f s0
    in runState (g a) s1
      
process :: State Light Bool
process = do
  turnOn
  turnOff
  switch
  b <- light
  if b == True then turnOn else turnOff
  return b
  

light2bool :: Light -> Bool
light2bool On = True
light2bool  _ = False
  

light :: State Light Bool
light = State $ \s -> (light2bool s, s)

turnOn :: State Light ()
turnOn = State $ \_ -> ((), On)

turnOff :: State Light ()
turnOff = State $ \_ -> ((), Off)

switch :: State Light ()
switch = State $ \s -> 
  case s of
    On -> ((), Off)
    Off -> ((), On)


--main :: IO ()
--main = do
--  let finalState = fst $ runState process Off
--  putStrLn $ show finalState


-- Dominik's exercise #2

-- Structure: StateT function > Monad > Tuple
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
  
eventloop :: StateT Light IO ()
eventloop = do
  s <- readLn
  return $ processInput s
  
processInput :: String -> StateT Light IO ()
processInput "light on" = turnOnT
processInput "ligt off" = turnOffT
processInput "switch"   = switchT
processInput "light"    = do 
                            state <- lightT
                            liftM . putStrLn . show $ state
processInput _ = pure

main :: IO ()
main = do
  s <- readLn
  return ()







