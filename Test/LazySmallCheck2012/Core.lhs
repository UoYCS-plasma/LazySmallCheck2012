> {-# LANGUAGE DeriveDataTypeable #-}
> module Test.LazySmallCheck2012.Core where

> import Control.Applicative
> import Control.Arrow
> import Control.DeepSeq
> import Control.Exception
> import Control.Monad
> import Control.Parallel.Strategies
> import Data.Data
> import Data.Maybe
> import Data.Monoid
> import Data.Typeable
> import System.Exit (exitFailure)
>
> import Test.PartialValues

> infixr 3 *&&*, |&&|
> infixl 1 *==>*, ==>

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

> type QuantInfo = [AlignedString]
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

> data AlignedString = LAlign [AlignedString]
>                    | Append String AlignedString
>                    | Braces AlignedString
>
> instance NFData AlignedString where
>   rnf (LAlign xs)    = rnf xs
>   rnf (Append str x) = rnf str `seq` rnf x
>   rnf (Braces x)     = rnf x
>
> string = (`Append` LAlign [])
>
> showDoc n (LAlign []) = ""
> showDoc n (LAlign (x:xs))
>   = init (unlines (showDoc n x : map ((replicate n ' ' ++) . showDoc 0) xs))
> showDoc n (Append str astr) = str ++ showDoc (n + length str) astr
> showDoc n (Braces astr) = "{ " ++ showDoc (n + 2) astr ++ " }"
>
> instance Show AlignedString where
>   show     = showDoc 0
>   showList = shows . LAlign . zipWith (\i -> Append ("Var " ++ show i ++ ": ")) [0..]

Test data terms
---------------

A `Term` is a pairing of possibly partial values with their possible
expansions. The `tValue` component takes in a root path values and
returns a possibly `Partial` value (containing exceptions of type
`LSC`), wrapping in a quantification context (`QuantCtx`) holding
pretty-printed representations of instantiated quantification
variables. The `tExpand` component returns a list of Terms that are
expansions at the path provided.

> type DLocation = (Nesting, Path -> Path)
>
> data Term a = Partial { ptValue  :: DLocation -> QuantCtx (Partial LSC a)
>                       , ptExpand :: Path      -> [Term a] }
>             | Total   { ttValue :: QuantCtx a }
>
> tValue :: Term a -> DLocation -> QuantCtx (Partial LSC a)
> tValue (Total   v)   = pure $ fmap pure v
> tValue (Partial v _) = v
>
> tExpand :: Term a -> Path -> [Term a]
> tExpand (Total  _)     = pure []
> tExpand (Partial _ es) = es
>
> instance Functor Term where
>   fmap f (Total v)      = Total (fmap f v)
>   fmap f (Partial v es) = Partial (fmap3 f v) (fmap3 f es)
>
> instance Applicative Term where
>   -- pure x = Partial (pure3 x) (pure [])
>   pure = Total . pure
>   Total f <*> Total x = Total (f <*> x)
>   fs      <*> xs      = Partial
>     (\(n, ps) -> (<*>) <$> tValue fs (n, ps . (False:))
>                        <*> tValue xs (n, ps . (True:)))
>     (\(p:ps)  -> if p then fmap (fs <*>) (tExpand xs ps)
>                       else fmap (<*> xs) (tExpand fs ps))


Series and Serial generators
----------------------------

Generators of Lazy SmallCheck values are defined by the `Series`
functor. Instances of `Functor`, `Applicative` and `Alternative` are
provided such that the depth-bounding and partiality functionality is
introduced and preserved.

> type Depth = Int
>
> -- | A depth-bounded generator of type `a`.
> newtype Series a = Series { runSeries :: Depth -> [Term a] }
>
> instance Functor Series where
>   fmap f xs = pure f <*> xs
>
> instance Applicative Series where
>   pure = Series . pure3
>   Series fs <*> Series xs = Series $ \d ->
>     [ f <*> mergeTerms x | d > 0, f <- fs d
>                          , let x = xs (d - 1), (not.null) x ]
>
> mergeTerms :: [Term a] -> Term a
> mergeTerms []  = error "LSC: Cannot merge empty terms."
> mergeTerms [x] = x
> mergeTerms xs  = Partial (QC [string "_"] . inject . Expand . second ($ [])) (const xs)
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

> -- | The depth-bounded generator of type 'a'.
> class (Data a, Typeable a, Show a) => Serial a where
>   -- | Return the 'Series' for this type.
>   series :: Series a
>
>   -- | Return the 'Series' for this type, storing the pretty-printed
>   -- value in the quantification context.
>   seriesWithCtx :: Series a
>   seriesWithCtx = Series $ (fmap . fmap) storeShow $ runSeries series
>     where storeShow (Total v) = Total
>             ((\(QC _ x) -> QC [string $ show x] x) v)
>           storeShow (Partial v es) = Partial 
>             ((fmap $ \(QC _ x) -> QC [string $ show x] x) v)
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
> -- | Series union. Synonym for '<|>'.
> (\/) :: Series a -> Series a -> Series a
> (\/) = (<|>)
>
> -- | Series application. Synonym for '<*>'.
> (><) :: Series (a -> b) -> Series a -> Series b
> (><) = (<*>)
>
> -- | Adjust the depth of a series by pre-composing a depth function.
> deeperBy :: (Int -> Int) -> Series a -> Series a
> deeperBy f xs = Series $ \d -> runSeries xs (f d)
>
> -- | Offset the depth cost of an application.
> zeroCost :: Series a -> Series a
> zeroCost = deeperBy (+1)
>
> -- | Zero-cost Series application.
> applZC :: Series (a -> b) -> Series a -> Series b
> applZC (Series fs) (Series xs) = Series $ \d ->
>   [ f <*> mergeTerms x | f <- fs d, let x = xs d, (not.null) x ]
>
> -- | Build a series from a depth-determined list of elements.
> drawnFrom :: (Depth -> [a]) -> Series a
> drawnFrom f = Series $ map pure . f

Properties
----------

> data Property = Lift Bool | Not Property | And Property Property
>               | PAnd    Property Property
>               | Implies Property Property
>               | ForAll (Depth -> Depth) (Series Property)
>               | Exists (Depth -> Depth) (Series Property)
>  deriving (Typeable)

> instance Data Property where
>   toConstr _   = error "toConstr"
>   gunfold _ _  = error "gunfold"
>   dataTypeOf _ = error "datatypeOf"
>   dataCast2 _  = error "dataCast2"

> class PropertyLike a where
>   mkProperty :: a -> Property
>
> instance PropertyLike Bool where mkProperty = Lift
> instance PropertyLike Property where mkProperty = id

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

> -- | 'Property' equivalent to 'True'.
> tt :: Property
> tt = Lift True
> -- | 'Property' equivalent to 'False'.
> ff :: Property
> ff = Lift False
>
> -- | 'Property' equivalent to 'not'.
> inv :: PropertyLike a => a -> Property
> inv = Not . mkProperty
>
> -- | Boolean lazy implication.
> (==>) :: Bool -> Bool -> Bool
> False ==> _ = True
> True  ==> x = x
>
> -- | 'Property' equivalent to '&&'.
> (*&&*) :: (PropertyLike a, PropertyLike b) => a -> b -> Property
> xs *&&* ys  = mkProperty xs `And` mkProperty ys
> -- | 'Property' equivalent to implication, '==>'.
> (*==>*) :: (PropertyLike a, PropertyLike b) => a -> b -> Property
> xs *==>* ys = mkProperty xs `Implies` mkProperty ys
>
> -- | Parallel conjunction. If left is undefined, try right.
> (|&&|) :: (PropertyLike a, PropertyLike b) => a -> b -> Property
> xs |&&| ys  = mkProperty xs `PAnd` mkProperty ys
>
> -- | Universal quantification. Space searched is bounded by the
> -- global depth.
> forAll :: Testable a => a -> Property
> forAll = ForAll id . mkTest . pure
> -- | Existential quantification. Space searched is bounded by the
> -- global depth.
> exists :: Testable a => a -> Property
> exists = Exists id . mkTest . pure
>
> -- | Universal quantification. Space searched is 
> -- global depth changed by some depth function.
> forAllDeeperBy :: Testable a => (Depth -> Depth) -> a -> Property
> forAllDeeperBy f = ForAll f . mkTest . pure
> -- | Existential quantification. Space searched is 
> -- global depth changed by some depth function.
> existsDeeperBy :: Testable a => (Depth -> Depth) -> a -> Property
> existsDeeperBy f = Exists f . mkTest . pure

Refute
------

The `Counter` comonad holds the number of tests performed.

> data Counter a = C { ctrCount :: !Int, ctrValue :: a }
> 
> instance Functor Counter where
>   fmap f (C c v) = C c (f v)
>
> instance Applicative Counter where
>   pure = C 0
>   C fc fv <*> C xc xv = C (fc + xc) (fv xv)
>
> instance NFData a => NFData (Counter a) where
>   rnf (C c x) = rnf c `seq` rnf x 

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
>     terms = foldr reduce (pure $ Right Nothing) . parMap rdeepseq term
>     reduce (C n (Right Nothing)) y = C (n + ctrCount y) (ctrValue y)
>     reduce x                     _ = x
>     term :: Term Property -> Counter (Either LSC (Maybe QuantInfo))
>     term (Total v) = join2 $ C 1 $ Right $ fmap2 qcToMaybe 
>                      $ fmap sinkQC $ sinkQC $ fmap prop v
>     term (Partial v es) = refineWith es $ fmap2 qcToMaybe $ join2 
>       (C 1 $ fmap2 sinkQC $ fmap sinkQC $ sinkQC $ 
>              fmap peek $ fmap2 prop $ v (n, id))
>     prop :: Property -> Counter (Either LSC Bool)
>     prop (Lift     v)    = pure2 v
>     prop (Not      p)    = not   `fmap2` prop p
>     prop (And      p q)  = (&&)  `fmap2` prop p `appl2` prop q
>     prop (Implies  p q)  = (==>) `fmap2` prop p `appl2` prop q
>     prop (PAnd     p q)  = parcomm (&&) (prop p) (prop q)
>     prop (ForAll   f xs) = isNothing `fmap2` refute (n + 1) (f d) xs
>     prop (Exists   f xs) = isJust `fmap2` refute (n + 1) (f d) (fmap Not xs)
>     refineWith es (C m (Left (Expand (n', ps))) )
>       | n == n' = C m id <*> terms (es ps)
>     refineWith es x = x
>
> -- | Parallel application of commutative binary Boolean functions.
> parcomm :: (Bool -> Bool -> Bool) -> 
>            Counter (Either LSC Bool) -> Counter (Either LSC Bool) ->
>            Counter (Either LSC Bool)
> parcomm f p q = query (force $ ctrValue p) (force $ ctrValue q)
>   where force = join . fmap (peek . pure)
>         query (Left _) (Right _) = f `fmap2` q `appl2` p
>         query _        _         = f `fmap2` p `appl2` q

> join2 :: Counter (Either a (Counter (Either a b))) -> Counter (Either a b)
> join2 (C m (Left x))        = C m (Left x)
> join2 (C m (Right (C n x))) = C (m + n) x

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