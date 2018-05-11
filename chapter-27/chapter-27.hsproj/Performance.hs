module Performance where

f :: Integer -> Integer
f 0 = 0
f n = 1 + f (n-1)

main :: IO ()
main = do
  putStrLn "starting"
  let x = f 10000000
    in do
      putStrLn $ show x
      putStrLn $ show $ f 10000001
      

memoized_fib :: Int -> Integer
memoized_fib = (map fib [0 ..] !!)
   where fib 0 = 0
         fib 1 = 1
         fib n = memoized_fib (n-2) + memoized_fib (n-1)
