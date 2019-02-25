{-# OPTIONS_GHC -Wall #-}

module MaybeAnotherMonoid where
  
import Test.QuickCheck
import Data.Monoid
import MonoidTest

data Optional a = Nada | Only a
  deriving (Eq, Show)

newtype First' a = First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance (Monoid a) => Monoid (First' a) where
  mempty           = First' Nada
  mappend 
    (First' { getFirst' = (Only a) }) 
    (First' { getFirst' = (Only b) })
      = First' { getFirst' = Only $ a <> b }
  mappend
    (First' (Only a)) _ = First' $ Only a
  mappend
    _ _ = First' Nada
  
  
instance (Arbitrary a) => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    frequency 
      [ (1, return (First' (Only a))), 
        (1, return (First' Nada)) ]
  

type FirstMappend = First' String 
  -> First' String
  -> First' String
  -> Bool

type FstId = First' String -> Bool


main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)