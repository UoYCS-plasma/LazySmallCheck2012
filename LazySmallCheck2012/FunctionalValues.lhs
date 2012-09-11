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
> import Test.PartialValues
>
> type (:->:) = Level1
>
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
> data Prim = Prim { unPrim :: Int }
>
> isoIntPrim :: Int -> (Int -> Prim, Prim -> Int)
> isoIntPrim pivot = (to . (+) (- pivot), (+ pivot) . from)
>   where to n | 0 <= n    = Prim (n * 2)
>              | otherwise = Prim ((negate n * 2) + 1)
>         from (Prim n) | even n = n `div` 2
>                       | odd n  = negate ((n - 1) `div` 2)
>
>
> data Level1 k v where
>   Wild :: v -> Level1 k v
>   Case :: Level2 k v -> Level1 k v
>
> data Level2 k v where
>   Valu :: v -> Level2 () v
>   Thnk :: Level2 (Base k) v -> Level2 (BaseThunk k) v
>   Sum  :: Level2 j v -> Level2 k v -> Level2 (Either j k) v
>   Pro  :: Level1 j (Level1 k v) -> Level2 (j, k) v
>   Assc :: [v] -> v -> Level2 Prim v
>
> applyT :: (k :->: v) -> k -> v
> applyT (Wild v) = const v
> applyT (Case t) = applyT' t
>
> applyT' :: Level2 k v -> k -> v
> applyT' (Valu v)   ()            = v
> applyT' (Thnk t)   (BaseThunk k) = t `applyT'` k
> applyT' (Sum  t _) (Left k)      = t `applyT'` k
> applyT' (Sum  _ t) (Right k)     = t `applyT'` k
> applyT' (Pro  t)   (j, k)        = t `applyT`  j `applyT` k
> applyT' (Assc m d) (Prim k)      = foldr const d $ drop k m
>
> wild = inject $ Expand (0, [])
>
> tabulateT :: (k :->: v) -> BT (Partial LSC k, v)
> tabulateT (Wild v) = Leaf (wild, v)
> tabulateT (Case t) = tabulateT' t
>
> tabulateT' :: Level2 k v -> BT (Partial LSC k, v)
> tabulateT' (Valu v)   = Leaf (pure (), v)
> tabulateT' (Thnk t)   = fmap (first $ fmap BaseThunk) (tabulateT' t)
> tabulateT' (Sum  l r) = fmap (first $ fmap Left) (tabulateT' l)
>                            `Branch`
>                         fmap (first $ fmap Right) (tabulateT' r)
> tabulateT' (Pro  t)   = do (j, t') <- tabulateT t
>                            (k, v)  <- tabulateT t'
>                            return ((,) <$> j <*> k, v)
> tabulateT' (Assc m d) = foldr Branch (Leaf (wild, d)) 
>                         [ Leaf (pure (Prim k), v) | (k, v) <- zip [0..] m ]
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
>   l2series srs = pure Pro `applZC` l1series (l1series srs)
>
> instance L2Serial Prim where
>   l2series srs = pure Assc `applZC` fullSize 0 `applZC` srs
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