import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.List
import Debug.Trace
import System.Directory

import Text.CSV

eitherToMaybe :: IO (Either a b) -> MaybeT IO b
eitherToMaybe action = lift action >>= either (const mzero) return

grabNames :: FilePath -> MaybeT IO [String]
grabNames = fmap (filter (not.null) . map head . tail) .
            eitherToMaybe . parseCSVFromFile
  
grabTimes :: [a] -> FilePath -> MaybeT IO [Double]
grabTimes zs path = do
  input <- eitherToMaybe (parseCSVFromFile path)
  return $ map (safeRead . head . tail) $ zipWith const (map (++ repeat "0") (tail input)) zs

safeRead str | null str  = 0
             | otherwise = read str

main = runMaybeT $ do
  paths <- filterM (lift . doesFileExist) [ "N" ++ show n ++ ".csv" | n <- [1..16] ]
  names <- map (drop $ length "LSC2012/Test") `fmap` grabNames (head paths)
  times <- mapM (grabTimes names) paths
  let normalised = map (map (subtract 1) . zipWith (/) (head times)) times
  let output = (:) ("":paths) $ transpose $ names : map (map show) normalised
  lift $ writeFile "Summary.csv" $ printCSV output