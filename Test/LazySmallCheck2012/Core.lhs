> {-# LANGUAGE DeriveDataTypeable #-}
> module Test.LazySmallCheck2012.Core where

> import Control.Applicative
> import Control.Arrow
> import Control.DeepSeq
> import Control.Exception
> import Data.Data
> import Data.Maybe
> import Data.Monoid
> import Data.Typeable
> import System.Exit (exitFailure)
>
> import Test.PartialValues

Special Lazy SmallCheck exceptions
----------------------------------

> type Location = (Nesting, Path)
> type Nesting = Int
> type Path = [Bool]
> data LSC = Expand Location deriving (Show, Typeable)
> instance Exception LSC

Quantification contexts
-----------------------

The quantification context is simply a (co)monad container that
carries information about quantifiers.

> type QuantInfo = [String]
>
> data QuantCtx a = QC { qcCtx :: QuantInfo, qcVal :: a }
>
> instance Functor QuantCtx where
>   fmap f (QC ctx val) = QC ctx (f val)
>
> instance Applicative QuantCtx where
>   pure = QC []
>   QC ctx0 f <*> QC ctx1 x = QC (ctx0 ++ ctx1) (f x)

QuantInfo simply holds the pretty-printed representations of
instantiated quantification variable values.


Test data terms
---------------

A `Term` is a pairing of possibly partial values with their possible
expansions. The `tValue` component takes in a root path values and
returns a possibly `Partial` value (containing exceptions of type
`LSC`), wrapping in a quantification context (`QuantCtx`) holding
pretty-printed representations of instantiated quantification
variables. The `tExpand` component returns a list of Terms that are
expansions at the path provided.

> data Term a = Term { tValue  :: Location -> QuantCtx (Partial LSC a)
>                    , tExpand :: Path     -> [Term a] }
>
> instance Functor Term where
>   fmap f (Term v es) = Term (fmap3 f v) (fmap3 f es)
>
> instance Applicative Term where
>   pure x = Term (pure3 x) (pure [])
>   fs <*> xs = Term
>     (\(n, ps) -> (<*>) <$> tValue fs (n, ps ++ [False])
>                        <*> tValue xs (n, ps ++ [True]))
>     (\(p:ps)  -> if p then fmap (fs <*>) (tExpand xs ps)
>                       else fmap (<*> xs) (tExpand fs ps))


Series and Serial generators
----------------------------

Generators of Lazy SmallCheck values are defined by the `Series`
functor. Instances of `Functor`, `Applicative` and `Alternative` are
provided such that the depth-bounding and partiality functionality is
introduced and preserved.

> type Depth = Int
> newtype Series a = Series { runSeries :: Depth -> [Term a] }
>
> instance Functor Series where
>   fmap f xs = pure f <*> xs
>
> instance Applicative Series where
>   pure = Series . pure3
>   Series fs <*> Series xs = Series $ \d ->
>     [ f <*> x | d > 0, f <- fs d, let x = mergeTerms $ xs (d - 1) ]
>
> mergeTerms :: [Term a] -> Term a
> mergeTerms []  = error "LSC: Cannot merge empty terms."
> mergeTerms [x] = x
> mergeTerms xs  = Term (pure . inject . Expand) (const xs)
>
> instance Alternative Series where
>   empty = Series $ pure []
>   Series xs <|> Series ys = Series $ (++) <$> xs <*> ys

Using this interface, we can define series generators for types. For
example, a series generator for Peano numerals could be represented as;

< peanoSeries :: Series Peano
< peanoSeries = pure Zero <|> (pure Succ <∗> peanoSeries)

When instantiating quantification variables, the QuantInfo
representation is stored. The instantiation is performed
automatically for types satisfying the Serial type-class.

> class (Data a, Typeable a) => Serial a where
>   series :: Series a
>
>   seriesWithCtx :: Series a
>   seriesWithCtx = Series $ (fmap . fmap) storeShow $ runSeries series
>     where storeShow (Term v es) = Term 
>             ((fmap $ \(QC _ x) -> QC [show x] x) v)
>             ((fmap . fmap) storeShow es)

The `storeShow` value uses the Partial instance of `Show` to store a
pretty-printed representation of each `Term`'s value in its
quantification context. Now that we have the Serial type-class,
the `cons<N>` family of combinators can be constructed as described by
Runciman et al. (2008).

> cons, cons0 :: o -> Series o
> cons  = pure
> cons0 = pure
>
> cons1 :: Serial a => 
>          (a -> o) -> Series o
> cons1 f = f <$> series
>
> cons2 :: (Serial a, Serial b) => 
>          (a -> b -> o) -> Series o
> cons2 f = f <$> series <*> series
>
> cons3 :: (Serial a, Serial b, Serial c) => 
>          (a -> b -> c -> o) -> Series o
> cons3 f = f <$> series <*> series <*> series
>
> cons4 :: (Serial a, Serial b, Serial c, Serial d) => 
>          (a -> b -> c -> d -> o) -> Series o
> cons4 f = f <$> series <*> series <*> series <*> series
>
> cons5 :: (Serial a, Serial b, Serial c, Serial d, Serial e) => 
>          (a -> b -> c -> d -> e -> o) -> Series o
> cons5 f = f <$> series <*> series <*> series <*> series <*> series
>
> (\/) :: Series a -> Series a -> Series a
> (\/) = (<|>)
>
> (><) :: Series (a -> b) -> Series a -> Series b
> (><) = (<*>)
>
> deeperBy :: (Int -> Int) -> Series a -> Series a
> deeperBy f xs = Series $ \d -> runSeries xs (f d)
>
> zeroCost = deeperBy (+1)
>
> drawnFrom xs = Series $ map pure . xs

Properties
----------

> data Property = Lift Bool | Not Property | And Property Property
>               | Implies Property Property
>               | ForAll (Depth -> Depth) (Series Property)
>               | Exists (Depth -> Depth) (Series Property)

> class PropertyLike a where
>   mkProperty :: a -> Property
>
> instance PropertyLike Bool where mkProperty = Lift
> instance PropertyLike Property where mkProperty = id

Zero-cost Series application

> applZC :: Series (a -> b) -> Series a -> Series b
> applZC (Series fs) (Series xs) = Series $ \d ->
>   [ f <*> x | d > 0, f <- fs d, let x = mergeTerms $ xs d ]

> class Testable a where
>   mkTest :: Series a -> Series Property
>   mkTestWithCtx :: (Data a, Typeable a) => Series a -> Series Property
>
> instance Testable Bool where
>   mkTest = applZC (pure Lift)
>   mkTestWithCtx = applZC (pure Lift)
>
> instance Testable Property where
>   mkTest = id
>   mkTestWithCtx = id
>
> instance (Serial a, Data b, Typeable b, Testable b) => Testable (a -> b) where
>   mkTest srs = mkTest $ srs `applZC` series
>   mkTestWithCtx srs = mkTestWithCtx $ srs `applZC` seriesWithCtx

Smart constructors for `Property`s.

> tt = Lift True
> ff = Lift False
>
> inv :: PropertyLike a => a -> Property
> inv = Not . mkProperty
>
> xs *&&* ys  = mkProperty xs `And` mkProperty ys
> xs *==>* ys = mkProperty xs `Implies` mkProperty ys
>
> exists, forAll :: Testable a => a -> Property
> exists = Exists id . mkTest . pure
> forAll = ForAll id . mkTest . pure
>
> existsDeeperBy, forAllDeeperBy :: Testable a => (Depth -> Depth) -> a -> Property
> existsDeeperBy f = Exists f . mkTest . pure
> forAllDeeperBy f = ForAll f . mkTest . pure

Refute
------

The `Counter` comonad holds the number of tests performed.

> type Counter a = (Sum Int, a)

A function that searches for `counterexample`s assuming no higher
level quantification.

> counterexample d xs = either (error "LSC: Unresolved expansion") id 
>                       `fmap` refute 0 d xs

The algorithm for refuting properties (finding counterexamples) is
defined using using auxiliary functions, making use of the Partial
values library. At various points, exceptions are made explicit
through the `runPartial` function.

> refute :: Nesting -> Depth -> Series Property ->
>           Counter (Either LSC (Maybe QuantInfo))
> refute n d xs = terms $ runSeries xs d
>   where
>     terms = foldr reduce (Sum 0, Right Nothing) . map term
>     reduce (n, Right Nothing) (n', x) = (n `mappend` n', x)
>     reduce (n, x)             _       = (n, x)
>     term :: Term Property -> Counter (Either LSC (Maybe QuantInfo))
>     term (Term v es) = refineWith es $ fmap2 qcToMaybe $ join2 
>       (Sum 1, fmap2 sinkQC $ fmap sinkQC $ sinkQC $ 
>               fmap peek $ fmap2 prop $ v (n, []))
>     prop :: Property -> Counter (Either LSC Bool)
>     prop (Lift     v)    = pure2 v
>     prop (Not      p)    = not   `fmap2` prop p
>     prop (And      p q)  = (&&)  `fmap2` prop p `appl2` prop q
>     prop (Implies  p q)  = (==>) `fmap2` prop p `appl2` prop q
>     prop (ForAll   f xs) = isNothing `fmap2` refute (n + 1) (f d) xs
>     prop (Exists   f xs) = isJust `fmap2` refute (n + 1) (f d) (fmap Not xs)
>     refineWith es (Sum m, Left (Expand (n', ps))) 
>       | n == n' = first (mappend $ Sum m) (terms (es ps))
>     refineWith es x = x

> (==>) :: Bool -> Bool -> Bool
> False ==> _ = True
> True  ==> x = x


> join2 :: Counter (Either a (Counter (Either a b))) -> Counter (Either a b)
> join2 (m, Left x) = (m, Left x)
> join2 (m, Right (n, x)) = (m `mappend` n, x)

> instance NFData LSC where
>   rnf (Expand x) = rnf x

> instance NFData a => NFData (Sum a) where
>   rnf (Sum x) = rnf x

> sinkQC :: Functor f => QuantCtx (f a) -> f (QuantCtx a)
> sinkQC (QC ctx val) = fmap (QC ctx) val

> qcToMaybe :: QuantCtx Bool -> Maybe QuantInfo
> qcToMaybe (QC ctx False) = Just ctx
> qcToMaybe (QC ctx True)  = Nothing

Composing functors
------------------

> fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
> fmap2 f = (fmap . fmap) f
>
> pure2 :: (Applicative f, Applicative g) => a -> f (g a)
> pure2 = (pure . pure)
>
> appl2 :: (Applicative f, Applicative g) => 
>          f (g (a -> b)) -> f (g a) -> f (g b)
> appl2 fs xs = (<*>) <$> fs <*> xs

> fmap3 :: (Functor f, Functor g, Functor h) => 
>          (a -> b) -> f (g (h a)) -> f (g (h b))
> fmap3 f = (fmap2 . fmap) f
> 
> pure3 :: (Applicative f, Applicative g, Applicative h) => 
>          a -> f (g (h a))
> pure3 = (pure2 . pure)
> 
> appl3 :: (Applicative f, Applicative g, Applicative h) => 
>          f (g (h (a -> b))) -> f (g (h a)) -> f (g (h b))
> appl3 fs xs = appl2 <$> fs <*> xs