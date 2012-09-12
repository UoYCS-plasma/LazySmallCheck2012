module LSC2008.TestMux2 where

import LazySmallCheck
import Benchmarks.Mux
import System.Environment

bench d =  depthCheck (read d) prop_encode
