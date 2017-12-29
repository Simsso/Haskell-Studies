cuthead :: [a] -> Maybe (a, [a])
cuthead []       = Nothing
cuthead (x : []) = Just (x, [])  -- single element (for clarification)
cuthead (x : xs) = Just (x, xs)

longlength :: [a] -> Int
longlength []         = 0
longlength (x:[])     = 1
longlength (x1:x2:[]) = 2
longlength _          = 10000000


removeVocals s = [c | c <- s, not (elem c ['a','e','i','o','u'])]