module MatrixShowTests where
import Matrix
import MatrixShow
import Control.Monad (forM_)
import Testing

main :: IO ()
main = do heading
		run_tests tests

heading :: IO ()
heading = do startTesting "MatrixShowTests $Revision: 1.2 $"
