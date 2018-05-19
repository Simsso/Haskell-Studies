-- Dominik's exercise #1

{-# OPTIONS_GHC -Wall #-}

module Light where

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
    
main :: IO ()
main = do
  let finalState = fst $ runState process Off
  putStrLn $ show finalState
  
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
