module LSC2012.TestHuffman1 where

import Data.Data
import Test.LazySmallCheck2012
import Benchmarks.Huffman
import System.Environment

instance Serial a => Serial (BTree a) where
  series = cons1 Leaf \/ cons2 Fork

bench d =  depthCheck (read d) prop_decEnc
