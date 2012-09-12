module LSC2012.TestCircuits1 where

import Test.LazySmallCheck2012
import Benchmarks.Mux
import System.Environment

bench d =  depthCheck (read d) prop_encDec
