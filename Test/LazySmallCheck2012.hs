{-# LANGUAGE ParallelListComp #-}
module Test.LazySmallCheck2012 where

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

depthCheck :: (Data a, Typeable a) => Testable a => Depth -> a -> IO ()
depthCheck d p = case counterexample d (mkTestWithCtx $ pure p) of
  (Sum n, Nothing) -> putStrLn $ "LSC: Property holds after "
                              ++ show n ++ " tests."
  (Sum n, Just cx) -> do putStrLn $ "LSC: Counterexample found after "
                                 ++ show n ++ " tests.\n"
                         print cx
                         exitFailure
                     
test p = mapM (flip depthCheck p) [0..]