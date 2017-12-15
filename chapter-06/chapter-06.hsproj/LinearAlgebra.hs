module LinearAlgebra where


-- computes the dot product of two vectors
-- throws a "Dimension missmatch" error if the list lengths do not match
dotp :: Num a => [a] -> [a] -> a
dotp [] [] = 0
dotp _ [] = error "Dimension missmatch"
dotp [] _ = error "Dimension missmatch"
dotp a b = head a * head b + dotp (tail a) (tail b)


-- multiplies a matrix [[a]] with a vector [a]
-- each element in [[a]] is a row vector of the matrix
matxvec :: Num a => [[a]] -> [a] -> [a]
matxvec [] x = []
matxvec a x = dotp (head a) x : (matxvec (tail a) x)


-- returns the transpose of a matrix or vector
transpose :: Num a => [[a]] -> [[a]]
transpose [] = []
transpose x = map head x : transpose (rempty (map tail x)) where
  allempty :: [[a]] -> Bool
  allempty [] = True
  allempty x = length (head x) == 0 && allempty (tail x)
  rempty :: Num a => [[a]] -> [[a]]  -- remove empty list entries
  rempty x = filter (not . null) x


-- computes the product of two matrices
-- each element in the result is a row vector of the matrix
-- each element in [[a]] is a row vector of the matrix
matmul :: Num a => [[a]] -> [[a]] -> [[a]]
matmul a b = transpose $ matmul2 a (transpose b) where
  -- the first matrix is a list of row vectors
  -- the second is a list of column vectors
  matmul2 :: Num a => [[a]] -> [[a]] -> [[a]]
  matmul2 a [] = []
  matmul2 a b = matxvec a (head b) : matmul2 a (tail b)


-- misc
vecsquaredsum :: Num a => [a] -> a
vecsquaredsum x = sum (map (^2) x)
frobenius :: Floating b => [[b]] -> b
frobenius x = sqrt $ sum (map vecsquaredsum x)