{-# LANGUAGE ParallelListComp #-}
module Test.LazySmallCheck2012( 
  -- * Depth-bounded, demand-driven property testing
  depthCheck, pruneStats, PruneStats(..), test, Testable(),
  -- ** Property language
  Property(), PropertyLike(),
  tt, ff, inv, (*&&*), (*==>*), (==>), (|&&|),
  forAll, exists, forAllDeeperBy, existsDeeperBy, 
  -- * Serial and Series definition
  Serial(series), Series(), seriesSize,
  -- * Series construction
  module Control.Applicative, (\/), (><), applZC, 
  deeperBy, zeroCost, drawnFrom, (<.>),
  -- ** cons\<N\> combinators,
  cons, cons0, cons1, cons2, cons3, cons4, cons5,
  -- * Argument construction,
  Argument(..), BaseCast(), isoIntNat, fromBaseCast, toBaseCast,
  -- * Default instances for 'Serial' and 'Argument'
  module Test.LazySmallCheck2012.Instances,
  module Test.LazySmallCheck2012.FunctionalValues.Instances
  ) where

import Control.Applicative
import Control.Monad
import Data.Data
import Data.Generics.Instances
import Data.Monoid
import Data.Typeable
import System.Exit
import Data.IORef
import Control.Exception
import System.CPUTime

import Test.LazySmallCheck2012.Core
import Test.LazySmallCheck2012.FunctionalValues hiding (Sum)
import Test.LazySmallCheck2012.Instances
import Test.LazySmallCheck2012.FunctionalValues.Instances
import Test.LazySmallCheck2012.Stats

-- | Check a `Testable` `Property` to a specified depth.
depthCheck :: (Data a, Typeable a, Testable a) => Depth -> a -> IO ()
depthCheck d p = case counterexample d (mkTestWithCtx $ pure p) of
  (C ct Nothing)   -> putStrLn $ "LSC: Property holds after "
                                 ++ show ct ++ " tests."
  (C ct (Just cx)) -> do putStrLn $ "LSC: Counterexample found after "
                                    ++ show ct ++ " tests."
                         print cx
                         exitFailure

-- | Machine readable output
data PruneStats = PruneStats { psTests :: Integer, psIsTrue :: Integer }
  deriving Show
                          
pruneStats :: (Data a, Typeable a, Testable a) => Depth -> a -> IO ()
pruneStats d p = do let space = mkTestWithCtx $ pure p
                    pgrs <- getCPUTime >>= newIORef . (,) 0 . (,) (seriesSize d space)
                    result <- evaluate $ allSat (Just pgrs) 0 d space
                    let C (ct, cp) cx = either (error "LSC: Unresolved expansion") id 
                                           `fmap` result
                    print $ PruneStats (getSum ct) (getSum cp)

seriesSize :: Depth -> Series a -> Integer
seriesSize d = tSize . mergeTerms . ($ d) . runSeries

-- | Check a `Testable` `Property` for all depths. Runs forever.
test :: (Data a, Typeable a) => Testable a => a -> IO ()
test p = sequence_ [ do putStrLn $ "LSC: Depth " ++ show d ++ ":"
                        depthCheck d p
                   | d <- [0..] ]

-- | Simulates composition in SC & LSC2008.
(<.>) :: Series a -> (Depth -> Depth) -> Series a
(<.>) = flip deeperBy