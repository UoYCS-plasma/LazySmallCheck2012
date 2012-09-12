module LSC2012.TestSumPuz where

import Test.LazySmallCheck2012
import Benchmarks.SumPuz
import System.Environment

bench d =  depthCheck (read d) prop_Sound
