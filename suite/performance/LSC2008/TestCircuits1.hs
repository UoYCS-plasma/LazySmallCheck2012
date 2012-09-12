module LSC2008.TestCircuits1 where

import LazySmallCheck
import Benchmarks.Mux
import System.Environment

bench d =  depthCheck (read d) prop_encDec
