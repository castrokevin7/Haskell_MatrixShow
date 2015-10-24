module MatrixShow where
import Matrix
import Data.List

instance Show a => Show (Matrix a)
  where
  	show matrix = concat (intersperse "\n" (rows matrix))

rowToString :: Show a => [a] -> String
rowToString row = concat (intersperse " " (map (show) row))

rowAt :: (Matrix a) -> Int -> [a]
rowAt (Mat ((i, j), f)) n = map f $ ([(x, y) | x <- [n .. n], y <- [1 .. j]])

rows :: Show a => (Matrix a) -> [String]
rows matrix = [(rowToString(rowAt  matrix i)) | i <- [1 .. numRows matrix]]