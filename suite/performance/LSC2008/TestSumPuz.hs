module LSC2008.TestSumPuz where

import LazySmallCheck
import Benchmarks.SumPuz
import System.Environment

bench d =  depthCheck (read d) prop_Sound
