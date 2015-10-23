module MatrixShow where
import Matrix

instance (Show a) => Show (Matrix a) where 

coords :: (Int, Int) -> [(Int, Int)]
coords (w, h) = [(x, y) | x <- [0 .. w], y <- [0 .. h]]

elems :: (Matrix a) -> [a]
elems (Mat (size, f)) = map f $ coords size