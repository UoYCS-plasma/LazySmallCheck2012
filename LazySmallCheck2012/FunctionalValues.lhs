> {-# LANGUAGE TypeOperators, GADTs, TypeFamilies, FlexibleContexts,
>              ScopedTypeVariables #-}
> module Test.LazySmallCheck2012.FunctionalValues where
>
> import Control.Applicative
> import Control.Arrow (first, second)
> import Control.Monad
> import Data.Data
> import Data.Generics.Instances
> import Data.Maybe
> import Data.Typeable
> import Text.Show.Functions
>
> import Test.LazySmallCheck2012.Core
> import Test.LazySmallCheck2012.Instances (Nat(..))
> import Test.PartialValues
  
Functional values
=================

The key to generating functional values is the ability to represent
the data-types of arguments as *tries*, also known as prefix
trees.  Lazy SmallCheck can then generate an appropriate trie and
convert it into the requried function.

Trie structure
--------------

First we shall describe tries on a known subset of Haskell datatypes.
Notationally, `k :->: v` is a trie where keys are of type `k` and
values are of type `v`.

> type (:->:) = Level1

The tries used are structured in two levels. `Level1` determines if
a value is immediately returned without inspecting the key --- `Wild`,
or if some case analysis is required on the key --- `Case`.

> data Level1 k v where
>   Wild :: v -> Level1 k v
>   Case :: Level2 k v -> Level1 k v

`Level2` is a trie that inspects the head of the key. Five types of
key are handled;

*   `Valu` -- Keys that are of type unit. The trie is just a value.
*   `Sum` -- Keys that are a tagged union of other key types. The trie
    is a pair of subtries for each key in the union.
*   `Prod` -- Keys that are a product of other key types. The trie    
    is a subtrie for the first component resulting in a subtrie for
    the second component.
*   `Natu` -- Keys that are natural numbers. The trie consists of a
    list of values representing each number and a default value for
    those that are out of the range of the list.
*   `Cast` -- Keys that are wrapped up in a type-level thunk. This
    will be explained in further detail, later.

> data Level2 k v where
>   Valu :: v -> Level2 () v
>   Sum  :: Level2 j v -> Level2 k v -> Level2 (Either j k) v
>   Prod :: Level2 j (Level2 k v) -> Level2 (j, k) v
>   Natu :: [v] -> v -> Level2 Nat v
>   Cast :: Argument k => Level1 (Base k) v -> Level2 (BaseCast k) v

The `applyT` function is a trie equivalent of the explicit application
function `($)`. It is used to lookup a key and return a value.

> applyT :: (k :->: v) -> k -> v
> applyT = applyL1
>
> applyL1 :: Level1 k v -> k -> v
> applyL1 (Wild v) = const v
> applyL1 (Case t) = applyL2 t
>
> applyL2 :: Level2 k v -> k -> v
> applyL2 (Valu v)   _             = v
> applyL2 (Sum  t _) (Left k)      = t `applyL2` k
> applyL2 (Sum  _ t) (Right k)     = t `applyL2` k
> applyL2 (Prod t)   (j, k)        = t `applyL2` j `applyL2` k
> applyL2 (Natu m d) (Nat k)       = foldr const d $ drop k m
> applyL2 (Cast t)   (BaseCast k) = t `applyL1` k

The `tabulateT` function converts a trie into an
pseudo-association-list. Binary trees are used instead of an list to
ensure that the undefinedness of any particular subtrie does not
prevent the extraction of defined others.

> tabulateT :: (k :->: v) -> BT (Partial LSC k, v)
> tabulateT = tabulateL1
>
> wild = inject $ Expand (0, [])
>
> tabulateL1 :: Level1 k v -> BT (Partial LSC k, v)
> tabulateL1 (Wild v) = Leaf (wild, v)
> tabulateL1 (Case t) = tabulateL2 t
>
> tabulateL2 :: Level2 k v -> BT (Partial LSC k, v)
> tabulateL2 (Valu v)   = Leaf (pure (), v)
> tabulateL2 (Sum  l r) = fmap (first $ fmap Left) (tabulateL2 l)
>                            `Branch`
>                         fmap (first $ fmap Right) (tabulateL2 r)
> tabulateL2 (Prod t)   = do (j, t') <- tabulateL2 t
>                            (k, v)  <- tabulateL2 t'
>                            return ((,) <$> j <*> k, v)
> tabulateL2 (Natu m d) = foldr Branch (Leaf (wild, d)) 
>                         [ Leaf (pure (Nat k), v) 
>                         | (k, v) <- zip [0..] m ]
> tabulateL2 (Cast t)   = fmap (first $ fmap BaseCast) (tabulateL1 t)

Serial instances for tries
-------------------------- 

The function `seriesT` generates a trie for keys `k` and values `v`
when those type variables belong to appropriate classes.

> seriesT :: (SerialL2 k, Serial v) => Series (k :->: v)
> seriesT = seriesL1 series
>
> seriesL1 :: SerialL2 k => Series v -> Series (Level1 k v)
> seriesL1 srs = (pure Wild `applZC` srs) 
>            <|> (pure Case <*> seriesL2 srs)

The `SerialL2` class is used to eliminate type-checking problems of
doing it all as one `Serial` instance. It is a class of key types that
if, given a series for the values, can construct a trie based on that
key and values.

> class SerialL2 k where
>   seriesL2 :: Series v -> Series (Level2 k v)

In these instances, the zero cost application combinator is used to
prevent wasted depth. Most are standard ADT series instances.

> instance SerialL2 () where
>   seriesL2 srs = pure Valu `applZC` srs
>
> instance (SerialL2 j, SerialL2 k) => SerialL2 (Either j k) where
>   seriesL2 srs = pure Sum `applZC` seriesL2 srs `applZC` seriesL2 srs
>
> instance (SerialL2 j, SerialL2 k) => SerialL2 (j, k) where
>   seriesL2 srs = pure Prod `applZC` seriesL2 (seriesL2 srs)

The  `SerialL2 Nat` definition uses a special formulation of list
series to ensure that the list length is always the maximum the depth
allows  and that all elements are of the same depth.

> instance SerialL2 Nat where
>   seriesL2 srs = pure Natu `applZC` fullSize 0 `applZC` srs
>     where fullSize o = onlyZero [] <|> 
>                        ((:) <$> deeperBy (+ o) srs <*> fullSize (o + 1))
>           onlyZero x = Series $ \d -> [ pure x | d == 0 ]
>
> instance Argument k => SerialL2 (BaseCast k) where
>   seriesL2 srs = pure Cast `applZC` seriesL1 srs

Non-base types
--------------

For key types that are not of these standard forms, the `Argument`
type-class provides a method of presenting an isomorphism to these 
`Base` types.

> class (SerialL2 (Base k), Typeable k, Data k) => Argument k where
>   type Base k
>   toBase   :: k -> Base k
>   fromBase :: Base k -> k

The `BaseCast` wrapped is a type-level promise that a type can be
translated into a `Base` type. It is useful for describing recursive
definitions without provoking the infinite type error.

> data BaseCast a = BaseCast { forceBase :: Base a }

Standard combinators are supplied for constructing these isomorphisms.

> toBaseCast :: Argument k => k -> BaseCast k
> toBaseCast = BaseCast . toBase
> fromBaseCast :: Argument k => BaseCast k -> k
> fromBaseCast = fromBase  . forceBase

The `isoIntNat` combinator provites a translation from integer-like
types to `Nat`urals.

> isoIntNat :: Int -> (Int -> Nat, Nat -> Int)
> isoIntNat pivot = (to . (+) (- pivot), (+ pivot) . from)
>   where to n | 0 <= n    = Nat (n * 2)
>              | otherwise = Nat ((negate n * 2) + 1)
>         from (Nat n) | even n = n `div` 2
>                       | odd n  = negate ((n - 1) `div` 2)

See Test.LazySmallCheck2012.FunctionalValues.Instances for examples
of these.

The Serial instance for functional values
-----------------------------------------

These are used to produce the `Serial` instance for function types.
The `series` function is very easy to understand --- create the
appropriate trie and then convert it to an appropriate Haskell 
function.

The `seriesWithCtx` is more complex. It retrieves the appropriate
pretty-printed version of values from the context and uses that
to construct and pretty printed version of the trie *before* it is
converted into a Haskell function.

> instance (Argument a, Serial b) => Serial (a -> b) where
>
>   series = pure (\t k -> applyT t (toBase k)) `applZC` seriesT
>
>   seriesWithCtx = pure (\t k -> applyT t (toBase k)) 
>                            `applZC` 
>                   Series (fmap2 storeShow $ runSeries $ seriesL1 seriesWithCtx)
>     where
>       storeShow (TTerm v) = TTerm
>         ((\(QC ctx t) -> QC [combine ctx $ pure t] t) v)
>       storeShow (PTerm v es) = PTerm
>         ((fmap $ \(QC ctx t) -> QC [combine ctx t] t) v)
>         ((fmap . fmap) storeShow es)
>       combine ctx t = Braces $
>                       [ (show ((fromBase <$> k) :: Partial LSC a)
>                             ++  " -> ") `Append` v
>                       | (Just (k, _), v) <- trieToList t `zip` ctx
>                       , show v /= "_" ]
>
> trieToList :: Partial LSC (k :->: v) -> [Maybe (Partial LSC k, Partial LSC v)]
> trieToList = fmap2 (first join) . fmap join
>            . fmap2 (toMaybe_MP . absorb2 . fmap JustPair)
>            . toMList_BT . absorb . fmap tabulateT
