module PrintTests where

main :: IO ()
main = do
  putStrLn "Count to four for me:"
  putStr "one, two"
  putStr ", three, and"
  putStrLn " four!"
  

-- top-level (global)
manipulate :: Integer -> Integer
manipulate x = add2 x where
  -- local
  add2 :: Integer -> Integer
  add2 x = x + 2
  

-- sub list with length l of list x starting at s
sublst :: Int -> Int -> [a] -> [a]
sublst s l x = take l (drop s x)


thirdLetter :: String -> String
thirdLetter x = (x !! 2) : ""

area d = pi * (r * r)  -- top level
  where r = d / 2  -- local
  

times5 = (5 *)
times4 = (4 *) 
times20 = times5 . times4 -- composition