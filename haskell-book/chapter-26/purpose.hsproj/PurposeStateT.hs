{-# OPTIONS_GHC -Wall #-}

module PurposeStateT where
  
-- StateT ermöglicht es, den State Monad mit einem beliebigen weiteren Monad (z.B. IO) zu einem
-- grösseren Monad zu kombinieren. Im Folgenden Beispiel kann morra übersichtlich aufgebaut werden,
-- weil die gemeinsame Funktionalität (get, put, putStrLn, getLine) zur Verfügung steht.
-- Beispielsweise könnte askMike (für eine bessere ai) auf den bisherigen Spielverlauf zugreifen.
-- Bei StateT s m a ist StateT s m der Monad, der Werte vom Typ a enthält. Das Beispiel zeigt schön,
-- dass a variabel ist, nämlich mal (), mal Maybe Choice, mal Choice.

import System.Random

data Choice = One | Two
  deriving Eq

data Opponent = User | Mike
  deriving (Eq, Show)

type Choices = (Choice, Choice)
type History = [Choices]

winner :: Choices -> Opponent
winner (x, y) = if x == y then User else Mike

main :: IO ()
main = do
  runStateT morra []
  return ()

morra :: StateT History IO ()
morra = do
  lift . putStrLn $ "play morra by picking 1 or 2 (anything else to stop)"
  answer <- askUser
  case answer of
    (Just x) -> do
      y <- askMike
      let choices = (x, y)
      lift . putStrLn $ "winner: " ++ show (winner choices)
      history <- get
      put (choices : history)
      morra
    Nothing -> do
      lift . putStrLn $ "stopped"
      history <- get
      let user = length $ filter (\c -> winner c == User) history
      let mike = length $ filter (\c -> winner c == Mike) history
      lift . putStrLn $ "User vs Mike: " ++ show user ++ ":" ++ show mike

askUser :: StateT History IO (Maybe Choice)
askUser = do
  lift . putStr $ "User: "
  line <- lift getLine
  case line of
    "1" -> return (Just One)
    "2" -> return (Just Two)
    _   -> return Nothing

askMike :: StateT History IO Choice
askMike = do
  r <- lift (randomRIO (1, 2) :: IO Int)
  lift . putStrLn $ "Mike: " ++ show r
  return $ if r == 1 then One else Two


newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
  fmap f (StateT smas) = StateT $ \s -> fmap g (smas s)
    where g = \(x, y) -> (f x, y)

instance Monad m => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  (StateT smfs) <*> (StateT smas) = StateT $ \s0 -> do
    (f, s1) <- smfs s0
    (a, s2) <- smas s1
    return (f a, s2)

instance Monad m => Monad (StateT s m) where
  (StateT smas) >>= aSmbs = StateT $ \s0 -> do
    (a, s1) <- smas s0
    runStateT (aSmbs a) s1

get :: Monad m => StateT s m s
get = StateT $ \s -> return (s, s)

put :: Monad m => s -> StateT s m ()
put s = StateT . const . return $ ((), s)

modify :: Monad m => (t -> t) -> StateT t m ()
modify f = do
  s <- get
  put $ f s

lift :: Monad m => m a -> StateT s m a
lift ma = StateT $ \s -> do
  a <- ma
  return (a, s)
