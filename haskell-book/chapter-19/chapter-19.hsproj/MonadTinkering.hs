{-# OPTIONS_GHC -Wall #-}

module MonadTinkering where
  
import System.Random

newRand :: IO Int
newRand = randomIO

main :: IO Int
main = do
  r1 <- newRand
  return $ condition r1
  
main2 :: IO Int
main2 = fmap condition newRand


condition :: Int -> Int
condition x = if x > 0 then 100 else -100


-- f = 5
-- f = 6

-- f = IO 5
-- f = IO 6