import           Control.Monad
import           System.Exit
import           System.IO

import qualified Test.Trident.Static.BLAS

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering 
  hSetBuffering stderr LineBuffering 

  results <- sequence [ 
      Test.Trident.Static.BLAS.tests
    ] 

  unless (and results) $ 
    exitFailure 