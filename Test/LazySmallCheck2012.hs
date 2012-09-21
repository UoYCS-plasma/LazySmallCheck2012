{-# LANGUAGE ParallelListComp #-}
module Test.LazySmallCheck2012( 
  -- * Depth-bounded, demand-driven property testing
  depthCheck, test, Testable(),
  -- ** Property language
  Property(), PropertyLike(),
  tt, ff, inv, (*&&*), (*==>*), (==>), (|&&|),
  forAll, exists, forAllDeeperBy, existsDeeperBy, 
  -- * Serial and Series definition
  Serial(series), Series(),
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
import Data.Data
import Data.Generics.Instances
import Data.Monoid
import Data.Typeable
import System.Exit

import Test.LazySmallCheck2012.Core
import Test.LazySmallCheck2012.FunctionalValues hiding (Sum)
import Test.LazySmallCheck2012.Instances
import Test.LazySmallCheck2012.FunctionalValues.Instances

-- | Check a `Testable` `Property` to a specified depth.
depthCheck :: (Data a, Typeable a) => Testable a => Depth -> a -> IO ()
depthCheck d p = case counterexample d (mkTestWithCtx $ pure p) of
  (C n Nothing)   -> putStrLn $ "LSC: Property holds after "
                              ++ show n ++ " tests."
  (C n (Just cx)) -> do putStrLn $ "LSC: Counterexample found after "
                                 ++ show n ++ " tests.\n"
                        print cx
                        exitFailure

-- | Check a `Testable` `Property` for all depths. Runs forever.
test :: (Data a, Typeable a) => Testable a => a -> IO ()
test p = sequence_ [ do putStrLn $ "LSC: Depth " ++ show d ++ ":"
                        depthCheck d p
                   | d <- [0..] ]

-- | Simulates composition in SC & LSC2008.
(<.>) :: Series a -> (Depth -> Depth) -> Series a
(<.>) = flip deeperBy