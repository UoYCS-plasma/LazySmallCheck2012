{-# LANGUAGE ParallelListComp #-}
module Test.LazySmallCheck2012 where

import Control.Applicative
import Data.Data
import Data.Generics.Instances
import Data.Monoid
import Data.Typeable
import System.Exit

import Test.LazySmallCheck2012.Core
import Test.LazySmallCheck2012.Instances

depthCheck :: (Data a, Typeable a) => Testable a => Depth -> a -> IO ()
depthCheck d p = case counterexample d (mkTestWithCtx $ pure p) of
  (Sum n, Nothing) -> putStrLn $ "LSC: Property holds after "
                              ++ show n ++ " tests."
  (Sum n, Just cx) -> do putStrLn $ "LSC: Counterexample found after "
                                 ++ show n ++ " tests.\n"
                         sequence_ [ putStrLn $ "VAR " ++ show i ++ ": " ++ var
                                   | var <- cx | i <- [0..] ]
                         exitFailure
                     
test p = mapM (flip depthCheck p) [0..]