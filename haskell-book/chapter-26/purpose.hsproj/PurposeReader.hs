module PurposeReader where
  
type Name = String


message' :: String
message' = create' "Timo"

create' :: Name -> String
create' name = partA' name ++ partB' name ++ partC' name

partA' :: Name -> String
partA' name = "Hello " ++ name ++ ". "

partB' :: Name -> String
partB' name = name ++ ", I show you the purpose of Reader. "

partC' :: Name -> String
partC' name = "Bye " ++ name ++ ". "


-- Reader verwendet man immer dann, wenn es einen Wert (z.B. "Timo") gibt, der an mehreren Orten benötigt wird.
-- Der Wert ist konstant, man möchte ihn aber nicht hardcoden, weil andere Konstanten auch sinnvoll sind.
-- Anstatt den Wert immer weiter zu reichen, nutzt man die Tatsache, dass Reader r ein Monad ist.


message :: String
message = runReader create "Timo"

create :: Reader Name String
create = do
  a <- partA
  b <- partB
  c <- partC
  return $ a ++ b ++ c

partA :: Reader Name String
partA = do
  name <- ask
  return $ "Hello " ++ name ++ ". "

partB :: Reader Name String
partB = do
  name <- ask
  return $ name ++ ", I show you the purpose of Reader. "

partC :: Reader Name String
partC = do
  name <- ask
  return $ "Bye " ++ name ++ ". "


data Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure = Reader . const
  (Reader rab) <*> (Reader ra) = Reader $ \r -> (rab r) (ra r)

instance Monad (Reader r) where
  (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb $ ra r) r

ask :: Reader r r
ask = Reader id
