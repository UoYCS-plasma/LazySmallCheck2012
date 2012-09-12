module LSC2012.TestTurner where

import Test.LazySmallCheck2012 hiding (Nat, Seq)
import Benchmarks.Turner
import System.Environment

instance Serial Var where
  series = cons0 V0 \/ cons0 V1

instance Serial Exp where
  series = cons2 (:@) \/ cons2 L \/ (cons1 V <.> (+1))

bench d =  depthCheck (read d) prop_abstr
