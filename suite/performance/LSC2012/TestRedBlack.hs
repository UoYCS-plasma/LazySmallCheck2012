module LSC2012.TestRedBlack where

import Data.Data
import Test.LazySmallCheck2012
import Benchmarks.RedBlack
import System.Environment


instance Serial Colour where
  series = cons0 R \/ cons0 B

instance Serial a => Serial (Tree a) where
  series = cons0 E \/ cons4 T

bench d =  depthCheck (read d) prop_insertRB
