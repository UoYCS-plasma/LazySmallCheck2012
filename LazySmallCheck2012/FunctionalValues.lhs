> {-# LANGUAGE TypeOperators, GADTs, TypeFamilies, 
>              Rank2Types, UndecidableInstances, ScopedTypeVariables #-}
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
> class Argument k where
>   type Base k
>   toBase   :: k -> Base k
>   fromBase :: Base k -> k
>
> data BaseThunk a = BaseThunk { forceBase :: Base a }
>
> instance Argument Bool where
>   type Base Bool = Either () ()
>   toBase False = Left  ()
>   toBase True  = Right ()
>   fromBase (Left  ()) = False
>   fromBase (Right ()) = True

> instance Argument a => Argument [a] where
>   type Base [a] = Either () (BaseThunk a, BaseThunk [a])
>   toBase []     = Left ()
>   toBase (x:xs) = Right (BaseThunk $ toBase x, BaseThunk $ toBase xs)
>   fromBase (Left ())       = []
>   fromBase (Right (x, xs)) = (fromBase (forceBase x): fromBase (forceBase xs))
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
>
> tabulateT :: (k :->: v) -> BT (Partial LSC k, v)
> tabulateT (Wild v) = Leaf (inject $ Expand (0, []), v)
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
>
> l1series :: Level2Serial k => Series v -> Series (Level1 k v)
> l1series srs = (pure Wild `applZC` srs) <|> (pure Case <*> l2series srs)
>
> class Level2Serial k where
>   l2series :: Series v -> Series (Level2 k v)
>
> instance Level2Serial () where
>   l2series srs = pure Valu `applZC` srs
>
> instance Level2Serial (Base k) => Level2Serial (BaseThunk k) where
>   l2series srs = pure Thnk `applZC` l2series srs
>
> instance (Level2Serial j, Level2Serial k) => Level2Serial (Either j k) where
>   l2series srs = pure Sum `applZC` l2series srs `applZC` l2series srs
>
> instance (Level2Serial j, Level2Serial k) => Level2Serial (j, k) where
>   l2series srs = pure Pro `applZC` l1series (l1series srs)
>
> instance (Argument a, Level2Serial (Base a), 
>           Typeable a, Typeable b, Data a, Data b, Serial b) =>
>          Serial (a -> b) where
>   series = pure ((. toBase) . applyT) `applZC` l1series series
>   seriesWithCtx = pure ((. toBase) . applyT) `applZC` early (l1series seriesWithCtx)
>     where early = Series . (fmap . fmap) storeShow . runSeries
>           storeShow (Term v es) = Term
>             ((fmap $ \(QC ctx t) -> QC [combine ctx t] t) v)
>             ((fmap . fmap) storeShow es)
>           combine ctx = Braces . LAlign . map (\(v, Just (k, _)) -> Append (show ((fromBase <$> join k) :: Partial LSC a) ++ " -> ") v) .
>                         filter (isJust . snd) . zip ctx . 
>                         map (maybe Nothing (toMaybe_MP . absorb2 . fmap JustPair)) .
>                         toMList_BT . absorb . fmap tabulateT