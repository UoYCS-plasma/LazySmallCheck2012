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
*   `Thnk` -- Keys that are wrapped up in a type-level thunk. This
    will be explained in further detail, later.

> data Level2 k v where
>   Valu :: v -> Level2 () v
>   Sum  :: Level2 j v -> Level2 k v -> Level2 (Either j k) v
>   Prod :: Level1 j (Level1 k v) -> Level2 (j, k) v
>   Natu :: [v] -> v -> Level2 Nat v
>   Thnk :: Level2 (Base k) v -> Level2 (BaseThunk k) v

The `applyT` function is a trie equivalent of the explicit application
function `($)`. It is used to lookup a key and return a value.

> applyT :: (k :->: v) -> k -> v
> applyT = applyL1
>
> applyL1 :: (Level1 k v) -> k -> v
> applyL1 (Wild v) = const v
> applyL1 (Case t) = applyL2 t
>
> applyL2 :: Level2 k v -> k -> v
> applyL2 (Valu v)   _             = v
> applyL2 (Thnk t)   (BaseThunk k) = t `applyL2` k
> applyL2 (Sum  t _) (Left k)      = t `applyL2` k
> applyL2 (Sum  _ t) (Right k)     = t `applyL2` k
> applyL2 (Prod t)   (j, k)        = t `applyL1` j `applyL1` k
> applyL2 (Natu m d) (Nat k)       = foldr const d $ drop k m

The `tabulateT` function converts a trie into an
pseudo-association-list. Binary trees are used instead of an list to
ensure that the undefinedness of any particular subtrie does not
prevent the extraction of defined others.

> tabulateT :: (k :->: v) -> BT (Partial LSC k, v)
> tabulateT = tabulateL1
 
> tabulateL1 :: (Level1 k v) -> BT (Partial LSC k, v)
> tabulateL1 (Wild v) = Leaf (wild, v)
> tabulateL1 (Case t) = tabulateL2 t
>
> tabulateL2 :: Level2 k v -> BT (Partial LSC k, v)
> tabulateL2 (Valu v)   = Leaf (pure (), v)
> tabulateL2 (Thnk t)   = fmap (first $ fmap BaseThunk) (tabulateL2 t)
> tabulateL2 (Sum  l r) = fmap (first $ fmap Left) (tabulateL2 l)
>                            `Branch`
>                         fmap (first $ fmap Right) (tabulateL2 r)
> tabulateL2 (Prod t)   = do (j, t') <- tabulateL1 t
>                            (k, v)  <- tabulateL1 t'
>                            return ((,) <$> j <*> k, v)
> tabulateL2 (Natu m d) = foldr Branch (Leaf (wild, d)) 
>                         [ Leaf (pure (Nat k), v) | (k, v) <- zip [0..] m ]



> class L2Serial (Base k) => Argument k where
>   type Base k
>   toBase   :: k -> Base k
>   fromBase :: Base k -> k
>
> data BaseThunk a = BaseThunk { forceBase :: Base a }
>
> toBaseThunk :: Argument k => k -> BaseThunk k
> toBaseThunk = BaseThunk . toBase
> fromBaseThunk :: Argument k => BaseThunk k -> k
> fromBaseThunk = fromBase  . forceBase
>
> isoIntNat :: Int -> (Int -> Nat, Nat -> Int)
> isoIntNat pivot = (to . (+) (- pivot), (+ pivot) . from)
>   where to n | 0 <= n    = Nat (n * 2)
>              | otherwise = Nat ((negate n * 2) + 1)
>         from (Nat n) | even n = n `div` 2
>                       | odd n  = negate ((n - 1) `div` 2)
>
>
>
>
> wild = inject $ Expand (0, [])
>
> l1series :: L2Serial k => Series v -> Series (Level1 k v)
> l1series srs = (pure Wild `applZC` srs) <|> (pure Case <*> l2series srs)
>
> class L2Serial k where
>   l2series :: Series v -> Series (Level2 k v)
>
> instance L2Serial () where
>   l2series srs = pure Valu `applZC` srs
>
> instance Argument k => L2Serial (BaseThunk k) where
>   l2series srs = pure Thnk `applZC` l2series srs
>
> instance (L2Serial j, L2Serial k) => L2Serial (Either j k) where
>   l2series srs = pure Sum `applZC` l2series srs `applZC` l2series srs
>
> instance (L2Serial j, L2Serial k) => L2Serial (j, k) where
>   l2series srs = pure Prod `applZC` l1series (l1series srs)
>
> instance L2Serial Nat where
>   l2series srs = pure Natu `applZC` fullSize 0 `applZC` srs
>     where fullSize o = onlyZero [] <|> ((:) <$> deeperBy (+ o) srs <*> fullSize (o + 1))
>           onlyZero x = Series $ \d -> [ pure x | d == 0 ]
>
> instance (Argument a, L2Serial (Base a), 
>           Typeable a, Typeable b, Data a, Data b, Serial b) =>
>          Serial (a -> b) where
>   series = pure ((. toBase) . applyT) `applZC` l1series series
>   seriesWithCtx = pure ((. toBase) . applyT) `applZC` early (l1series seriesWithCtx)
>     where early = Series . (fmap . fmap) storeShow . runSeries
>           storeShow (Term v es) = Term
>             ((fmap $ \(QC ctx t) -> QC [combine ctx t] t) v)
>             ((fmap . fmap) storeShow es)
>           combine ctx = Braces . LAlign . map (\(v, Just (k, _)) -> Append (show ((fromBase <$> join k) :: Partial LSC a) ++ " -> ") v) .
>                         filter ((/= "_") . show . fst) . filter (isJust . snd) . zip ctx . 
>                         map (maybe Nothing (toMaybe_MP . absorb2 . fmap JustPair)) .
>                         toMList_BT . absorb . fmap tabulateT