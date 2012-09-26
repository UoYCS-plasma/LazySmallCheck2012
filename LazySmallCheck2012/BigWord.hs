module Test.LazySmallCheck2012.BigWord where

import Control.DeepSeq
import Data.Monoid
import Data.Word

data BigWord = BWNothing | BWJust !Word64 deriving (Eq)

instance Show BigWord where
  show BWNothing  = "<large>"
  show (BWJust n) = show n

bigword :: a -> (Word64 -> a) -> BigWord -> a
bigword n j BWNothing  = n
bigword n j (BWJust x) = j x

wordOrder :: Word64 -> Float
wordOrder = logBase 2 . fromInteger . toInteger

instance Num BigWord where
  fromInteger = BWJust . fromInteger
  signum = bigword BWNothing (BWJust . signum)
  abs = id
  BWJust m + BWJust n  
   | m < (maxBound `div` 2) || 
     n < (maxBound `div` 2) = BWJust $ m + n
  _        + _        = BWNothing
  BWJust m - BWJust n  
   | m <= n = BWJust $ m - n
  _        - _        = BWNothing
  BWJust m * BWJust n  
   | wordOrder m + wordOrder n <= wordOrder maxBound
   = BWJust $ m * n
  _        * _        = BWNothing

instance NFData BigWord where
  rnf BWNothing  = ()
  rnf (BWJust n) = rnf n

instance Monoid BigWord where
  mempty  = BWJust 0
  mappend = (+)
  
bwlength :: [a] -> BigWord
bwlength = foldr (const $ (+) 1) 0