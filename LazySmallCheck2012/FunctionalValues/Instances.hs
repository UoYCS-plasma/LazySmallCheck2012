{-# LANGUAGE TypeFamilies, DeriveFunctor, DeriveDataTypeable #-}
module Test.LazySmallCheck2012.FunctionalValues.Instances where

import Control.Applicative
import Data.Data
import Data.Typeable

import Test.LazySmallCheck2012.Instances
import Test.LazySmallCheck2012.FunctionalValues

instance Argument () where
  type Base () = ()
  toBase = id
  fromBase = id

instance (Argument a, Argument b) => Argument (a, b) where
  type Base (a, b) = (BaseThunk a, BaseThunk b)
  toBase (i, j)   = (toBaseThunk i, toBaseThunk j)
  fromBase (i, j) = (fromBaseThunk i, fromBaseThunk j)
  
instance (Argument a, Argument b, Argument c) => Argument (a, b, c) where
  type Base (a, b, c) = (BaseThunk a, (BaseThunk b, BaseThunk c))
  toBase (i, j, k)   = (toBaseThunk i, (toBaseThunk j, toBaseThunk k))
  fromBase (i, (j, k)) = (fromBaseThunk i, fromBaseThunk j, fromBaseThunk k)
                
instance (Argument a, Argument b, Argument c, Argument d) 
         => Argument (a, b, c, d) where
  type Base (a, b, c, d) = (BaseThunk a, (BaseThunk b, (BaseThunk c, BaseThunk d)))
  toBase (i, j, k, l)   = (toBaseThunk i, (toBaseThunk j, (toBaseThunk k, toBaseThunk l)))
  fromBase (i, (j, (k, l))) = (fromBaseThunk i, fromBaseThunk j, fromBaseThunk k, fromBaseThunk l)
  


instance (Argument a, Argument b, Argument c, Argument d, Argument e)
         => Argument (a, b, c, d, e) where
  type Base (a, b, c, d, e) = (BaseThunk a, (BaseThunk b, (BaseThunk c, (BaseThunk d, BaseThunk e))))
  toBase (i, j, k, l, m)   = (toBaseThunk i, (toBaseThunk j, (toBaseThunk k, (toBaseThunk l, toBaseThunk m))))
  fromBase (i, (j, (k, (l, m)))) = (fromBaseThunk i, fromBaseThunk j, fromBaseThunk k, fromBaseThunk l, fromBaseThunk m)

instance Argument a => Argument [a] where
  type Base [a] = Either () (BaseThunk a, BaseThunk [a])
  toBase []        = Left ()
  toBase (x:xs)    = Right (toBaseThunk x, toBaseThunk xs)
  fromBase (Left  ())      = []
  fromBase (Right (x, xs)) = (fromBaseThunk x:fromBaseThunk xs)

instance Argument Bool where
  type Base Bool = Either () ()
  toBase False = Left  ()
  toBase True  = Right ()
  fromBase (Left  ()) = False
  fromBase (Right ()) = True

instance Argument a => Argument (Maybe a) where
  type Base (Maybe a) = Either () (BaseThunk a)
  toBase Nothing  = Left ()
  toBase (Just x) = Right (toBaseThunk x)
  fromBase (Left  ()) = Nothing
  fromBase (Right x)  = Just (fromBaseThunk x)

instance (Argument a, Argument b) => Argument (Either a b) where
  type Base (Either a b) = Either (BaseThunk a) (BaseThunk b)
  toBase   = either (Left . toBaseThunk) (Right . toBaseThunk)
  fromBase = either (Left . fromBaseThunk) (Right . fromBaseThunk)

instance Argument Nat where
  type Base Nat = Prim
  toBase   = Prim . unNat
  fromBase = Nat . unPrim

instance Argument Int where
  type Base Int = Prim
  toBase = fst $ isoIntPrim 0
  fromBase = snd $ isoIntPrim 0

instance Argument Char where
  type Base Char = Prim
  toBase = (fst $ isoIntPrim (fromEnum 'a')) . fromEnum
  fromBase = toEnum . (snd $ isoIntPrim (fromEnum 'a'))

