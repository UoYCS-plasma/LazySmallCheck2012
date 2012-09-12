module LSC2012.TestCircuits2 where

import Test.LazySmallCheck2012
import Benchmarks.Mux
import System.Environment

prop_mux_osc (sel, xs) =
      (oneHot sel
  &&  length sel == length xs
  &&  all ((== length (head xs)) . length) xs)
  *==>* (mux sel xs == xs !! n)
  where
    n = length (takeWhile not sel)

bench d =  depthCheck (read d) prop_mux

bench_osc d = depthCheck (read d) prop_mux_osc
