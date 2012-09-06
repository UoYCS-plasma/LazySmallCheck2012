{-# LANGUAGE DeriveFunctor, DeriveDataTypeable #-}
module Test.LazySmallCheck2012.Instances where

import Control.Applicative
import Data.Data
import Data.Typeable

import Test.LazySmallCheck2012.Core

instance Serial () where
  series = cons0 ()
    
instance (Serial a, Serial b) => Serial (a, b) where
  series = zeroCost $ cons2 (,)
  
instance (Serial a, Serial b, Serial c) => Serial (a, b, c) where
  series = zeroCost $ cons3 (,,)
                
instance (Serial a, Serial b, Serial c, Serial d) 
         => Serial (a, b, c, d) where
  series = zeroCost $ cons4 (,,,)
  
instance (Serial a, Serial b, Serial c, Serial d, Serial e)
         => Serial (a, b, c, d, e) where
  series = zeroCost $ cons5 (,,,,)
  
instance Serial a => Serial [a] where
  series = cons0 [] <|> cons2 (:)
  
-- | List where each element is of equal depth
newtype Seq a = Seq { unSeq :: [a] }
              deriving (Functor, Eq, Ord, Data, Typeable)

instance Show a => Show (Seq a) where
  show = show . unSeq
  
newtype Seq1 a = Seq1 { unSeq1 :: [a] }
              deriving (Functor, Eq, Ord, Data, Typeable)

instance Show a => Show (Seq1 a) where
  show = show . unSeq1
                       
instance Serial a => Serial (Seq a) where
  series = pure Seq `applZC` seqSeries (id <$> series)
  
instance Serial a => Serial (Seq1 a) where
  series = pure Seq1 `applZC` (pure (:) `applZC` elem `applZC` (id <$> seqSeries (deeperBy (+1) elem)))
    where elem = id <$> series

seqSeries :: Series a -> Series [a]
seqSeries elem = pure []
             <|> (pure (:) `applZC` elem `applZC` (id <$> seqSeries (deeperBy (+1) elem)))
          
instance Serial Bool where
  series = pure False <|> pure True

instance Serial Int where
  series = drawnFrom $ \d -> [(-d)..d]
  
instance Serial Integer where
  series = drawnFrom $ \d -> map toInteger [(-d)..d]

newtype Nat = Nat { unNat :: Int } deriving (Eq,Ord, Data, Typeable)

newtype Natural = Natural { unNatural :: Integer } deriving (Eq,Ord, Data, Typeable)

instance Show Nat where show = show . unNat
instance Show Natural where show = show . unNatural

instance Serial Nat where
  series = drawnFrom $ \d -> map Nat [0..d]
  
instance Serial Natural where
  series = drawnFrom $ \d -> map (Natural . toInteger) [0..d]

instance Serial Char where
  series = drawnFrom $ \d -> take (d + 1) ['a'..]
  
instance Serial a => Serial (Maybe a) where
  series = pure Nothing <|> cons1 Just

instance (Serial a, Serial b) => Serial (Either a b) where
  series = cons1 Left <|> cons1 Right