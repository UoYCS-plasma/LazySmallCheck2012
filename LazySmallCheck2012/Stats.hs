module Test.LazySmallCheck2012.Stats where

import Control.Applicative
import Control.Arrow
import Control.DeepSeq
import Control.Monad
import Data.Fixed
import Data.IORef
import Data.Monoid
import System.CPUTime
import System.IO
import System.IO.Unsafe

import Test.LazySmallCheck2012.Core
import Test.PartialValues

unsafeAdd :: Maybe (IORef (Integer, (Integer, Integer))) -> Integer -> a -> a
unsafeAdd Nothing _ = id
unsafeAdd (Just ref) c = seq $ unsafePerformIO $ do
  (space, oldTime) <- snd <$> readIORef ref
  modifyIORef' ref (first (+ c))
  newTime <- getCPUTime
  when (abs (oldTime - newTime) > 10 ^ 10) $ do    
    seen <- fst <$> readIORef ref
    modifyIORef' ref (second $ second $ const newTime)
    let s = showFixed False (ratio seen space :: Micro)
    putStr (' ' : ' ' : s ++ "  \r") >> hFlush stdout

ratio x y = fromIntegral x * 100 / fromIntegral y

allSat :: Maybe (IORef (Integer, (Integer, Integer))) -> 
          Nesting -> Depth -> Series Property ->
          Counter (Sum Integer, Sum Integer) (Either LSC Bool)
allSat pgrs n d xs = terms $ runSeries xs d
  where
    terms = force . foldr reduce (pure $ Right False) . map term
    reduce xs ys = (||) `fmap2` xs `appl2` ys
    term (TTerm v) = refineWith 1 (const []) 
                     $ join2 $ C (Sum 1, Sum 0) $ Right $ fmap2 qcVal
                     $ fmap sinkQC $ sinkQC $ fmap prop v
    term (PTerm v es pr) = refineWith pr es $ fmap2 qcVal $ join2 
      (C (Sum 1, Sum 0) $ fmap2 sinkQC $ fmap sinkQC $ sinkQC $ 
             fmap peek $ fmap2 prop $ v (n, id))
    prop (Lift     v)    = pure2 v
    prop (Not      p)    = not   `fmap2` prop p
    prop (And      p q)  = (&&)  `fmap2` prop p `appl2` prop q
    prop (Implies  p q)  = (==>) `fmap2` prop p `appl2` prop q
    prop (PAnd     p q)  = pand (prop p) (prop q)
    prop (ForAll   f xs) = allSat Nothing (n + 1) (f d) xs
    prop (Exists   f xs) = allSat Nothing (n + 1) (f d) (fmap Not xs)
    refineWith _  es (C ct (Left (Expand (n', ps))) )
      | n == n' = C ct id <*> terms (es ps)
    refineWith pr _  (C (m, n) x@(Right True)) = unsafeAdd pgrs pr $ C (m, n `mappend` Sum pr) x
    refineWith pr  es x = unsafeAdd pgrs pr $ x