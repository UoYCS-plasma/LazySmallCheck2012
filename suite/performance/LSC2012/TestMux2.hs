module LSC2012.TestMux2 where

import Test.LazySmallCheck2012
import Benchmarks.Mux
import System.Environment

bench d =  depthCheck (read d) prop_encode
