module LSC2008.TestHuffman2 where

import LazySmallCheck
import Benchmarks.Huffman
import System.Environment

instance Serial a => Serial (BTree a) where
  series = cons1 Leaf \/ cons2 Fork

bench d =  depthCheck (read d) prop_optimal
