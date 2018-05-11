module PurposeState where

main' :: IO ()
main' = print $ go' 0

go' :: Int -> Int
go' s0 = s8 where
  s1 = updateSucc' s0
  s2 = updateWrite' s1
  s3 = updateDouble' s2
  s4 = updateWrite' s3
  s5 = updateDouble' s4
  s6 = updateRead' s5
  s7 = updateSucc' s6
  s8 = updateRead' s7

updateRead' :: Int -> Int
updateRead' s = s

updateWrite' :: Int -> Int
updateWrite' s = 10

updateSucc' :: Int -> Int
updateSucc' s = s+1

updateDouble' :: Int -> Int
updateDouble' s = 2*s


-- Anstatt wie im obigen Beispiel den Int Wert immer weiter zu reichen, kann man State verwenden.
-- Und weil das ein Monad ist, geht das ganz elegant mit der do-notation. Die Funktionen dürfen
-- sich abhängig vom aktuellen Zustand verhalten, dafür sorgt s ->, genau wie beim Reader r ->.
-- Im Kontrast zum Reader darf hier ein neuer Zustand entstehen, dafür sorgt das Paar (a, s).
-- Ein "Paar ohne Pfeil" hat übrigens auch eine Bedeutung, nämlich "Schreiben ohne Lesen".

-- Monad:                                                  Lesen:   Schreiben:
-- newtype Identity a = Identity { runIdentity :: a }      nein     nein
-- newtype Reader r a = Reader { runReader :: r -> a }     ja       nein
-- newtype Writer w a = Writer { runWriter :: (a, w) }     nein     ja
-- newtype State s a = State { runState :: s -> (a, s) }   ja       ja


main :: IO ()
main = print $ runState go 0

go :: State Int ()
go = do
  x <- updateSucc
  updateWrite x
  updateDouble
  updateWrite x
  updateDouble
  updateRead
  updateSucc
  updateRead

updateRead :: State Int ()
updateRead = do
  s <- get
  return ()

updateWrite :: Double -> State Int ()
updateWrite x = do
  s <- get
  put $ floor x + s

updateSucc :: State Int Double
updateSucc = do
  s <- get
  put (s+1)
  return 2.4

updateDouble :: State Int ()
updateDouble = do
  s <- get
  put (2*s)


newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f (State g) = State $ \s0 ->
    let (a, s1) = g s0
    in (f a, s1)

instance Applicative (State s) where
  pure a = State $ \ s0 -> (a, s0)
  (State f) <*> (State g) = State $ \s0 ->
    let (ab, s1) = f s0
        (a, s2) = g s1
    in (ab a, s2)

instance Monad (State s) where
  (State f) >>= g = State $ \ s0 ->
    let (a, s1) = f s0
    in runState (g a) s1

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ const ((), s)
