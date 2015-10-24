module MatrixShowTests where
import Matrix
import MatrixShow
import Control.Monad (forM_)
import Test.QuickCheck

main :: IO ()
main = do heading
		run_tests tests

heading :: IO ()
heading = do startTesting "MatrixShowTests $Revision: 1.2 $"
debugIt :: IO ()
debugIt = do startTesting "MatrixShowTests $Revision: 1.2 $"
	forM_ matrices (\(title), mat) -> do putStrLn title print mat)

matrices :: [(String, Matrix Int)] matrices =
	[("mat11", fillWith (1, 1) 75),
	 ("mat43", fromRule (4, 3) (\(i, j) -> 10 * i + j)),
	 ("mat26", fromRule (2, 6) (\(i, j) -> 100 * i * i + j)),
	 ("mat53", fromRule (5, 3) (\(i, j) -> (sine i) + (cosine j))),]

mbv :: Maybe b -> b

mbv mb = case mb of	 
			Just x -> x
			Nothing -> undefined run2string :: String -> String
run2string title = show (mbv (lookup title matrices))	
tests :: [TestCase String]
tests = [eqTest (show (fromRule (2, 3) (\(i, j) -> (i, j))))
		"==" ( "(1, 1) (1, 2) (1, 3) \n"
			++ "(2, 1) (2, 2) (2, 3) \n"),
		eqTest (run2string "mat11")
		"==" ("75 \n"),
		eqTest (run2string "mat43")
		"==" ("11 12 13 \n"
		  ++  "21 22 23 \n"
		  ++  "31 32 33 \n"
		  ++  "41 42 43 \n"),
		eqTest (run2string "mat26")
		"==" ("101 102 103 104 105 106\n"
		  ++  "401 402 403 404 405 406\n")]

sine : : Int -> Int
sine x = truncate (10 ∗ (sin (fromIntegral x))) cosine :: Int -> Int
cosine x = truncate (10 ∗ (cos (fromIntegral x)))

