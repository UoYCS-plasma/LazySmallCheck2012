module LSC2008.TestCircuits3 where

import LazySmallCheck
import Benchmarks.Sad
import System.Environment

bench d =  depthCheck (read d) prop_binSad
