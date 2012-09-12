module LSC2008.TestListSet1 where

import LazySmallCheck
import Benchmarks.ListSet
import System.Environment

bench d =  depthCheck (read d) prop_insertSet
