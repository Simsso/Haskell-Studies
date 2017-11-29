import Foreign.Storable
import GHC.Int

-- pattern matching
-- the first implementation of customNot will be called if the parameter is True
-- the other one in any other case
customNot :: Bool -> Bool
customNot True = False
customNot    _ = True



-- parameter typeclass constraints
-- here the parameters a must implement Eq (equality)
same :: Eq a => a -> a -> Bool
same x y = x == y