module LSC2012.TestCountdown1 where

import Test.LazySmallCheck2012
import Benchmarks.Countdown
import System.Environment

bench d =  depthCheck (read d) prop_lemma3
