memoizedFib :: Int -> Integer
memoizedFib = (map fib [0 ..] !!)
   where fib 0 = 0
         fib 1 = 1
         fib n = memoizedFib (n-2) + memoizedFib (n-1)

main :: IO ()
main = putStrLn . show $ memoizedFib 1000
