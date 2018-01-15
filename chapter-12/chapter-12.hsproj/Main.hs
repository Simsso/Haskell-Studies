{-# OPTIONS_GHC -Wall #-}

matchTest :: Char -> Bool
matchTest 'c' = True
matchTest _ = False

matchTest2 :: String -> Bool
--matchTest2 'c' : _ = True
matchTest2 _ = False