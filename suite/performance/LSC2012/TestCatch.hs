module LSC2012.TestCatch where

import Data.Data
import Test.LazySmallCheck2012
import Benchmarks.Catch
import System.Environment

instance Serial Value where
  series = cons0 Bottom \/ cons2 Value
  
instance Serial CtorName where
  series = cons0 Ctor \/ cons0 CtorN \/ cons0 CtorR \/ cons0 CtorNR
  
instance Serial Val where
  series = cons2 (:*) \/ cons0 Any
  
instance Serial Pattern where
  series = cons2 Pattern
  
bench d =  depthCheck (read d) prop
