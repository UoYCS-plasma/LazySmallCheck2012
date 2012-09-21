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
  type Base (a, b) = (BaseCast a, BaseCast b)
  toBase (i, j)   = (toBaseCast i, toBaseCast j)
  fromBase (i, j) = (fromBaseCast i, fromBaseCast j)
  
instance (Argument a, Argument b, Argument c) => Argument (a, b, c) where
  type Base (a, b, c) = (BaseCast a, (BaseCast b, BaseCast c))
  toBase (i, j, k)   = (toBaseCast i, (toBaseCast j, toBaseCast k))
  fromBase (i, (j, k)) = (fromBaseCast i, fromBaseCast j, fromBaseCast k)
                
instance (Argument a, Argument b, Argument c, Argument d) 
         => Argument (a, b, c, d) where
  type Base (a, b, c, d) = (BaseCast a, (BaseCast b, (BaseCast c, BaseCast d)))
  toBase (i, j, k, l)   = (toBaseCast i, (toBaseCast j, (toBaseCast k, toBaseCast l)))
  fromBase (i, (j, (k, l))) = (fromBaseCast i, fromBaseCast j, fromBaseCast k, fromBaseCast l)
  


instance (Argument a, Argument b, Argument c, Argument d, Argument e)
         => Argument (a, b, c, d, e) where
  type Base (a, b, c, d, e) = (BaseCast a, (BaseCast b, (BaseCast c, (BaseCast d, BaseCast e))))
  toBase (i, j, k, l, m)   = (toBaseCast i, (toBaseCast j, (toBaseCast k, (toBaseCast l, toBaseCast m))))
  fromBase (i, (j, (k, (l, m)))) = (fromBaseCast i, fromBaseCast j, fromBaseCast k, fromBaseCast l, fromBaseCast m)

instance Argument a => Argument [a] where
  type Base [a] = Either () (BaseCast a, BaseCast [a])
  toBase []        = Left ()
  toBase (x:xs)    = Right (toBaseCast x, toBaseCast xs)
  fromBase (Left  ())      = []
  fromBase (Right (x, xs)) = (fromBaseCast x:fromBaseCast xs)

instance Argument Bool where
  type Base Bool = Either () ()
  toBase False = Left  ()
  toBase True  = Right ()
  fromBase (Left  ()) = False
  fromBase (Right ()) = True

instance Argument a => Argument (Maybe a) where
  type Base (Maybe a) = Either () (BaseCast a)
  toBase Nothing  = Left ()
  toBase (Just x) = Right (toBaseCast x)
  fromBase (Left  ()) = Nothing
  fromBase (Right x)  = Just (fromBaseCast x)

instance (Argument a, Argument b) => Argument (Either a b) where
  type Base (Either a b) = Either (BaseCast a) (BaseCast b)
  toBase   = either (Left . toBaseCast) (Right . toBaseCast)
  fromBase = either (Left . fromBaseCast) (Right . fromBaseCast)

instance Argument Nat where
  type Base Nat = Nat
  toBase   = id
  fromBase = id

instance Argument Int where
  type Base Int = Nat
  toBase = fst $ isoIntNat 0
  fromBase = snd $ isoIntNat 0

instance Argument Char where
  type Base Char = Nat
  toBase = (fst $ isoIntNat (fromEnum 'a')) . fromEnum
  fromBase = toEnum . (snd $ isoIntNat (fromEnum 'a'))

