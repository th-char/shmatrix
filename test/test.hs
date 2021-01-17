import           Control.Monad
import           System.Exit
import           System.IO

import Hedgehog

import qualified Tests.Numeric.Static.BLAS

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering 
  hSetBuffering stderr LineBuffering 

  results <- sequence [ 
      Tests.Numeric.Static.BLAS.tests
    ] 

  unless (and results) $ 
    exitFailure 