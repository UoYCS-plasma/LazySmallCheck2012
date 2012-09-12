module LSC2008.TestCountdown1 where

import LazySmallCheck
import Benchmarks.Countdown
import System.Environment

bench d =  depthCheck (read d) prop_lemma3
