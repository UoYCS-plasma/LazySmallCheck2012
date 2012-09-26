% Partial Values
% Dealing with Haskell values that we expect to contain exceptions.
% Jason S. Reich
% 6th June 2012

> {-# LANGUAGE DeriveFunctor, Rank2Types, ScopedTypeVariables #-}

This module provides an interface for coping with values that you
know may contain exceptions, perhaps because you put them there (like
in LSC). The techniques used largely taken from 'Chasing Bottoms'
(Danielsson and Jansson, 2004) but with different formulations for my
purposes.

> module Test.PartialValues(
>   -- * Partial Values
>   Partial, isException, inject, peek, unsafePeek, peekAll,
>   -- * Explicitly Partial Functors
>   ExplicitF(..), ExplicitF2(..), BT(..), consBT, MaybePair(..),
>   toMList_BT, toMaybe_MP) where

A few neccessary imports and hides.

> import Control.Applicative
> import Control.DeepSeq
> import Control.Exception
> import Data.Data
> import Data.List
> import Data.Typeable
> import GHC.Show (appPrec)
> import System.IO.Unsafe
> import Prelude hiding (catch)

Partial Values
============

Partial values are just normal Haskell values that we know something
about. We represent this as a boring functor that is phantom-indexed
by an exception type.

> -- | 'Partial' values --- Values into which we may have deliberately
> -- injected exceptions.
> newtype Partial e a = Partial { unsafePeek :: a }
>   deriving Functor

Name a mathematical abstraction, it probably holds over the 'Partial'
functor.

> instance Monad (Partial e) where
>   return = Partial
>   Partial x >>= f = f x
>
> instance Applicative (Partial e) where
>   pure = return
>   f <*> x = unwrapMonad $ WrapMonad f <*> WrapMonad x

inject creates partial values.

> -- | Throws an exception and wraps it as a 'Partial' value.
> inject :: Exception e => e -> Partial e a
> inject = Partial . throw

Peek all the way inside a 'Partial' value, catching the exception.

> peek :: (NFData a, Exception e) => Partial e a -> Either e a
> peek value = unsafePerformIO $
>   (Right <$> evaluate (force (unsafePeek value)))
>   `catch` (return . Left)

> peekAll :: (NFData a, Exception e) => Partial e a 
>         -> Either (Either e SomeException) a
> peekAll value = unsafePerformIO $
>   (Right <$> evaluate (force (unsafePeek value)))
>   `catches` [ Handler $ \err -> return $ Left $ Left err 
>             , Handler $ \err -> return $ Left $ Right err ]

'isException' tests for really partial values, i.e. ones that match
an exception predicate at their head.

> -- | Is the head of a Partial value an exception?
> isException :: Exception e => Partial e a -> Bool
> isException v = unsafePerformIO $
>   (evaluate (unsafePeek v) >> return False) `catch` aux v
>   where aux :: Partial e a -> e -> IO Bool
>         aux _ _ = return True

'show' will display '_' for values that are really partial.
*Requires an AugmentedShow instance*.

> instance (Exception e, Data a) => Show (Partial e a) where
>   showsPrec = showsPrecData

> showsPrecData :: forall e a. (Exception e, Data a) => Int -> Partial e a -> ShowS
> showsPrecData p x | isException x = ('_':)
> showsPrecData p (Partial t)
>   -- Is a tuple
>   | (isPrefixOf "(," . show . toConstr) t 
>   = showParen True 
>   $ foldr (.) id . intersperse (showChar ',')
>   . gmapQ (showsPrecData appPrec . mkPartial) $ t
>   -- Is a cons
>   | ((== "(:)") . show . toConstr) t
>   = showParen (p > 5)
>   $ gmapQi 0 (showsPrecData (5+1) . mkPartial) t
>   . showChar ':'
>   . gmapQi 1 (showsPrecData 5 . mkPartial) t
>   -- Is to be displayed prefix
>   | otherwise
>   = showParen (constrArity t > 0 && p > appPrec)
>   $ (showString . showConstr . toConstr $ t)
>     . (foldr (.) id . gmapQ ( (showChar ' ' .)
>                             . showsPrecData (appPrec + 1)
>                             . mkPartial ) $ t)
>   where 
>     mkPartial :: forall a. a -> Partial e a
>     mkPartial = Partial

> constrArity :: Data d => d -> Int
> constrArity = length . gmapQ (const ())

Explicitly Partial Functors
=========================

Sometimes I want to make the undefined at the head of a structure
explicit. I don't have a safe way of doing this yet, but until then
we have Explictly Partial Functors; make Partial structure explicit.

The basic idea is there exists some function such that;

> class ExplicitF f where
>   absorb :: Exception e => Partial e (f a) -> f (Partial e a)

> class ExplicitF2 f where
>   absorb2 :: Exception e => Partial e (f a b) -> f (Partial e a) (Partial e b)

Binary trees
------------

With binary trees, we can transform Really Partial tree structure into
empty nodes.

> data BT a = Empty | Leaf a | Branch (BT a) (BT a) deriving Functor

> consBT x xs = Leaf x `Branch` xs
> toMList_BT = flip aux []
>   where aux Empty        = (Nothing:)
>         aux (Leaf x)     = (Just x :)
>         aux (Branch l r) = aux l . aux r

> instance Monad BT where
>   return = Leaf
>   Empty      >>= _ = Empty
>   Leaf x     >>= f = f x
>   Branch l r >>= f = Branch (l >>= f) (r >>= f)

> instance ExplicitF BT where
>   absorb xs | isException xs  = Empty
>   absorb (Partial Empty)        = Empty
>   absorb (Partial (Leaf a))     = Leaf (Partial a)
>   absorb (Partial (Branch l r)) = (absorb . Partial $ l) `Branch` (absorb . Partial $ r)

Maybe Pairs
-----------

With Maybe enclosing Tuples, we turn Really Partial maybe and tuple
structure into Nothings.

> data MaybePair a b = NothingPair | JustPair (a, b)
> toMaybe_MP NothingPair  = Nothing
> toMaybe_MP (JustPair x) = Just x

> instance ExplicitF2 MaybePair where
>   absorb2 xs | isException xs       = NothingPair
>   absorb2 (Partial (NothingPair))     = NothingPair
>   absorb2 (Partial (JustPair (a, b))) = JustPair (Partial a, Partial b)
