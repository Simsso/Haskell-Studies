intId = id :: Integer -> Integer
x = intId 5

class (TypeclassB a, TypeclassC a) => TypeclassA a where
  identity :: a -> a

class (TypeclassC a) => TypeclassB a where
  identity2 :: a -> a

class TypeclassC a where
  identity3 :: a -> a
  

class Test a where
  giveString :: a -> String
  
data TestA = TestA
instance Test TestA where
  giveString _ = "a"
  
data TestB = TestB
instance Test TestB where
  giveString _ = "b"
  
