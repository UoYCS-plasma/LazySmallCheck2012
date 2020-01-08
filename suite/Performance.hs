import Control.Exception
import Criterion.Main
import Prelude hiding (catch)

import qualified LSC2008.TestCatch
import qualified LSC2008.TestCircuits1
import qualified LSC2008.TestCircuits2
import qualified LSC2008.TestCircuits3
import qualified LSC2008.TestCountdown1
import qualified LSC2008.TestCountdown2
import qualified LSC2008.TestHuffman1
import qualified LSC2008.TestHuffman2
import qualified LSC2008.TestListSet1
import qualified LSC2008.TestMate
import qualified LSC2008.TestMux2
import qualified LSC2008.TestRedBlack
import qualified LSC2008.TestRegExp
import qualified LSC2008.TestSumPuz
import qualified LSC2008.TestTurner

import qualified LSC2012.TestCatch
import qualified LSC2012.TestCircuits1
import qualified LSC2012.TestCircuits2
import qualified LSC2012.TestCircuits3
import qualified LSC2012.TestCountdown1
import qualified LSC2012.TestCountdown2
import qualified LSC2012.TestHuffman1
import qualified LSC2012.TestHuffman2
import qualified LSC2012.TestListSet1
import qualified LSC2012.TestMate
import qualified LSC2012.TestMux2
import qualified LSC2012.TestRedBlack
import qualified LSC2012.TestRegExp
import qualified LSC2012.TestSumPuz
import qualified LSC2012.TestTurner

ignore :: IO a -> Benchmarkable
ignore x = whnfIO $ (x >> return ()) `catch` (\ e -> print (e :: SomeException))

lsc2008 = 
  [ bench "TestCatch" $ ignore $ LSC2008.TestCatch.bench "4"
  , bench "TestCircuits1" $ ignore $ LSC2008.TestCircuits1.bench "10"
  , bench "TestCircuits2" $ ignore $ LSC2008.TestCircuits2.bench "7"
  , bench "TestCircuits3" $ ignore $ LSC2008.TestCircuits3.bench "4"
  , bench "TestCountdown1" $ ignore $ LSC2008.TestCountdown1.bench "5"
  , bench "TestCountdown2" $ ignore $ LSC2008.TestCountdown2.bench "4"
  , bench "TestHuffman1" $ ignore $ LSC2008.TestHuffman1.bench "8"
  , bench "TestHuffman2" $ ignore $ LSC2008.TestHuffman2.bench "5"
  , bench "TestListSet1" $ ignore $ LSC2008.TestListSet1.bench "11"
  , bench "TestMate" $ ignore $ LSC2008.TestMate.bench "4"
  , bench "TestMux2" $ ignore $ LSC2008.TestMux2.bench "10"
  , bench "TestRedBlack" $ ignore $ LSC2008.TestRedBlack.bench "4"
  , bench "TestRegExp" $ ignore $ LSC2008.TestRegExp.bench "2"
  , bench "TestSumPuz" $ ignore $ LSC2008.TestSumPuz.bench "4"
  , bench "TestTurner" $ ignore $ LSC2008.TestTurner.bench "3" ]
  
lsc2012 = 
  [ bench "TestCatch" $ ignore $ LSC2012.TestCatch.bench "4"
  , bench "TestCircuits1" $ ignore $ LSC2012.TestCircuits1.bench "10"
  , bench "TestCircuits2" $ ignore $ LSC2012.TestCircuits2.bench "7"
  , bench "TestCircuits3" $ ignore $ LSC2012.TestCircuits3.bench "4"
  , bench "TestCountdown1" $ ignore $ LSC2012.TestCountdown1.bench "5"
  , bench "TestCountdown2" $ ignore $ LSC2012.TestCountdown2.bench "4"
  , bench "TestHuffman1" $ ignore $ LSC2012.TestHuffman1.bench "8"
  , bench "TestHuffman2" $ ignore $ LSC2012.TestHuffman2.bench "5"
  , bench "TestListSet1" $ ignore $ LSC2012.TestListSet1.bench "11"
  , bench "TestMate" $ ignore $ LSC2012.TestMate.bench "4"
  , bench "TestMux2" $ ignore $ LSC2012.TestMux2.bench "10"
  , bench "TestRedBlack" $ ignore $ LSC2012.TestRedBlack.bench "4"
  , bench "TestRegExp" $ ignore $ LSC2012.TestRegExp.bench "2"
  , bench "TestSumPuz" $ ignore $ LSC2012.TestSumPuz.bench "4"
  , bench "TestTurner" $ ignore $ LSC2012.TestTurner.bench "3" ]
  
main = defaultMain [ bgroup "LSC2008" lsc2008, bgroup "LSC2012" lsc2012]