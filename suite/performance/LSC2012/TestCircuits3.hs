module LSC2012.TestCircuits3 where

import Test.LazySmallCheck2012
import Benchmarks.Sad
import System.Environment

bench d =  depthCheck (read d) prop_binSad
