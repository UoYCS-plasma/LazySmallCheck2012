module LSC2012.TestRegExp where

import Test.LazySmallCheck2012 hiding (Nat, Seq)
import Benchmarks.RegExp
import System.Environment

instance Serial Nat where
  series = cons0 Zer \/ cons1 Suc

instance Serial Sym where
  series = cons0 N0 \/ cons1 N1

instance Serial RE where
  series = cons1 Sym
        \/ cons2 Or
        \/ cons2 Seq
        \/ cons2 And
        \/ cons1 Star
        \/ cons0 Empty

bench d =  depthCheck (read d) prop_regex
