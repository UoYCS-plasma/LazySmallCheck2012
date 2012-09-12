module LSC2012.TestListSet1 where

import Test.LazySmallCheck2012
import Benchmarks.ListSet
import System.Environment

bench d =  depthCheck (read d) prop_insertSet
