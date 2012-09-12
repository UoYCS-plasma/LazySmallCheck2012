module LSC2012.TestMate where

import Test.LazySmallCheck2012
import Benchmarks.Mate
import System.Environment

instance Serial Kind where
  series = cons0 King
      \/ cons0 Queen
      \/ cons0 Rook
      \/ cons0 Bishop
      \/ cons0 Knight
      \/ cons0 Pawn
      
instance Serial Colour where
  series = cons0 Black \/ cons0 White

instance Serial Board where
  series = cons2 Board

bench d =  depthCheck (read d) prop_checkmate
