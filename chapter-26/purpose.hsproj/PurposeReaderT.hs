module PurposeReaderT where

type Name = String


main'' :: IO ()
main'' = go'' "Timo"

go'' :: Name -> IO ()
go'' name = do
  age <- inputAge'' name
  outputAge'' age name

inputAge'' :: Name -> IO Int
inputAge'' name = do
  putStrLn $ "How old are you, " ++ name ++ "?"
  line <- getLine
  return $ read line

outputAge'' :: Int -> Name -> IO ()
outputAge'' age name = putStrLn $ name ++ ", you are " ++ show age


-- In diesem Beispiel gibt es wieder einen Wert ("Timo"), der weitergereicht wird.
-- Neu ist, dass es hier bereits einen Monad (IO) gibt. Wenn man Reader einsetzt,
-- entsteht ein Problem: go' müsste den Int von inputAge' durch zwei Monadschichten
-- ziehen, um dann damit outputAge' aufzurufen. Im Allgemeinen ist die Verkettung
-- von zwei Monads aber kein Monad. (Ich kann go' nicht implementieren, und du?)


main' :: IO ()
main' = runReader go' "Timo"

go' :: Reader Name (IO ())
go' = undefined

inputAge' :: Reader Name (IO Int)
inputAge' = do
  name <- ask'
  return $ do
    putStrLn $ "How old are you, " ++ name ++ "?"
    line <- getLine
    return $ read line

outputAge' :: Int -> Reader Name (IO ())
outputAge' age = do
  name <- ask'
  return . putStrLn $ name ++ ", you are " ++ show age


data Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure = Reader . const
  (Reader rab) <*> (Reader ra) = Reader $ \r -> (rab r) (ra r)

instance Monad (Reader r) where
  (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb $ ra r) r

ask' :: Reader r r
ask' = Reader id


-- Der Reader Monad hat die Eigenschaft, dass seine Verkettung mit einem beliebigen anderen
-- Monad (z.B. IO) wieder ein Monad ist. So kommt ReaderT ins Spiel und löst das Problem.
-- Die folgenden do-Blöcke beziehen sich alle auf diesen einen (doppelschichtigen) Monad.
-- Darin steht uns gleichzeitig die Funktionalität vom Reader (ask) und die Funktionalität
-- von IO (putStrLn, getLine) zur Verfügung. Um an die Zwischenschicht (IO) ranzukommen,
-- braucht es lift. Die Implementierungen von ask und lift sind verblüffend elegant.


main :: IO ()
main = runReaderT go "Timo"

go :: ReaderT Name IO ()
go = do
  age <- inputAge
  outputAge age

inputAge :: ReaderT Name IO Int
inputAge = do
  name <- ask
  lift . putStrLn $ "How old are you, " ++ name ++ "?"
  line <- lift getLine
  return $ read line

outputAge :: Int -> ReaderT Name IO ()
outputAge age = do
  name <- ask
  lift . putStrLn $ name ++ ", you are " ++ show age


data ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ \r -> (fmap f) (rma r)

instance (Applicative m) => Applicative (ReaderT r m) where
  pure = ReaderT . const . pure
  (ReaderT rmab) <*> (ReaderT rma) = ReaderT $ \r -> (rmab r) <*> (rma r)

instance (Monad m) => Monad (ReaderT r m) where
  (ReaderT rma) >>= aRmb = ReaderT $ \r -> do
    a <- rma r
    runReaderT (aRmb a) r

ask :: Monad m => ReaderT r m r
ask = ReaderT return

lift :: Monad m => m a -> ReaderT r m a
lift = ReaderT . const
