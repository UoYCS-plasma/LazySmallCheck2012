{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DefaultSignatures, 
             FlexibleContexts, FlexibleInstances, TypeOperators, GADTs, 
             Rank2Types, MonadComprehensions, ParallelListComp #-}
module Test.LazySmallCheck where

import Control.Applicative
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import Data.Typeable
import GHC.Generics
import GHC.Show (appPrec)
import Text.Show.Functions
import System.IO.Unsafe

-- Figure 2
type Location = (Nesting, Path -> Path)
type Nesting = Int
type Path = [Bool]

data Refine = RefineAt Location deriving Typeable
instance Show Refine where
  show (RefineAt (n, ps)) = "Refine@(" ++ show n ++ "," ++ show (ps [])
instance Exception Refine
instance NFData Refine where
  rnf (RefineAt xs) = rnf xs

-- Figure 3
newtype Partial a = Partial { unsafePartial :: a }
instance Functor Partial where
  fmap f (Partial x) = Partial (f x)
instance Applicative Partial where
  pure = Partial
  Partial f <*> Partial x = Partial (f x)
  
runPartial :: NFData a => Partial a -> Either Refine a
runPartial value = unsafePerformIO $
  (Right <$> evaluate (force (unsafePartial value)))
    `catch` (return . Left)
  
refineAt :: Location -> Partial a
refineAt = Partial . throw . RefineAt

isException :: Partial a -> Bool
isException value = unsafePerformIO $
  (const False <$> evaluate (unsafePartial value))
    `catch` (\(RefineAt _) -> return True)

-- Figure 4
data Term a = TTerm (TVE a)
            | PTerm (Location -> TVE (Partial a)) (Path -> [Term a]) Integer

tValue :: Term a -> Location -> TVE (Partial a)
tValue (TTerm v)      = pure $ fmap pure v
tValue (PTerm v _ _)  = v

tExpand :: Term a -> Path -> [Term a]
tExpand (TTerm _)       = pure []
tExpand (PTerm _ es _)  = es

tSize :: Term a -> Integer
tSize (TTerm _)      = 1
tSize (PTerm _ _ n)  = n

instance Functor Term where
  fmap f (TTerm v)       = TTerm (fmap f v)
  fmap f (PTerm v es s)  = PTerm (fmap (fmap f) . v) (map (fmap f) . es) s
instance Applicative Term where
  pure = TTerm . pure
  TTerm f  <*> TTerm x  = TTerm (f <*> x)
  fs       <*> xs       = PTerm
    (\(n,ps) -> (<*>)  <$> tValue fs (n, ps `snoc` True)
                       <*> tValue xs (n, ps `snoc` False))
    (\(p:ps) -> if p  then map (<*> xs) (tExpand fs ps)
                      else map (fs <*>) (tExpand xs ps))
    (tSize fs * tSize xs)
    
snoc xs x = xs . (x:)

mergeTerms :: [Term a] -> Term a
mergeTerms xs = PTerm (TVE [string "_"] . refineAt) (const xs) (toInteger $ length xs)

-- Absent from paper
type AlignedString = String
string :: String -> AlignedString
string = id

-- Figure 5
data TVE a = TVE { tveEnv :: TVInfo, tveVal :: a }
type TVInfo = [AlignedString]
  
instance Functor TVE where
  fmap f (TVE env val) = TVE env (f val)
instance Applicative TVE where
  pure = TVE []
  TVE env0 f <*> TVE env1 x = TVE (env0 ++ env1) (f x)
  
-- Figure 6
type Depth = Int
newtype Series a = Series { runSeries :: Depth -> [Term a] }
instance Functor Series where
  fmap f xs = pure f <*> xs
instance Applicative Series where
  pure = Series . pure . pure . pure
  Series fs <*> Series xs = Series $ \d ->
    [ f <*> mergeTerms x  | d > 0, f <- fs d
                          , let x = xs (d - 1), (not.null) x ]
instance Alternative Series where
  empty = Series $ pure []
  Series xs <|> Series ys = Series $ (++) <$> xs <*> ys
fmap0 :: (a -> b) -> Series a -> Series b
fmap0 f xs = pure f `ap0` xs

ap0 :: Series (a -> b) -> Series a -> Series b
ap0 (Series fs) (Series xs) = Series $ \d ->
    [ f <*> mergeTerms x  | d > 0, f <- fs d
                          , let x = xs d, (not.null) x ]
deeperBy :: (Depth -> Depth) -> Series a -> Series a
deeperBy f srs = Series $ runSeries srs . f

-- Figure 7 but adjusted for GHC.Generics
class Show a => Serial a where
  series :: Series a
  partialShowsPrec :: Int -> Partial a -> ShowS
  
  seriesWithEnv :: Series a
  seriesWithEnv = Series $ fmap storeShow <$> runSeries series
    where storeShow (TTerm v)       = TTerm (TVE [string $ show $ tveVal v] (tveVal v))
          storeShow (PTerm v es s)  = PTerm
            ((fmap $ \(TVE _ x) -> TVE [string $ partialShow x] x) v)
            (fmap storeShow <$> es) s

  
  default series :: (Generic a, GSerial (Rep a)) => Series a
  series = fmap to $ gseries
  
  default partialShowsPrec :: (Generic a, GSerial (Rep a)) => Int -> Partial a -> ShowS
  partialShowsPrec p = gpartialShowsPrec ' ' p . fmap from

partialShow x = partialShowsPrec appPrec x ""

class GSerial f where
  gseries :: Series (f a)
  gpartialShowsPrec :: Char -> Int -> Partial (f a) -> ShowS
  
instance GSerial U1 where
  gseries = pure U1
  gpartialShowsPrec _ _ _ = id

instance GSerial f => GSerial (M1 D a f) where
  gseries = fmap0 M1 gseries
  gpartialShowsPrec c p = gpartialShowsPrec c p . fmap unM1
  
instance (GSerial f, GSerial g) => GSerial (f :+: g) where
  gseries = fmap0 L1 gseries <|> fmap0 R1 gseries
  gpartialShowsPrec c p x | isException x = ('_':)
  gpartialShowsPrec c p (Partial (L1 x)) = gpartialShowsPrec c p $ Partial x
  gpartialShowsPrec c p (Partial (R1 x)) = gpartialShowsPrec c p $ Partial x

instance (Serial b) => GSerial (K1 a b) where
  gseries = fmap K1 series
  gpartialShowsPrec _ p = partialShowsPrec p . fmap unK1
  
instance (GSerial f, Constructor c) => GSerial (M1 C c f) where
  gseries = fmap0 M1 gseries
  gpartialShowsPrec _ _ x | isException x = ('_':)
  gpartialShowsPrec c p (Partial t)
    -- Is a tuple
    | (isPrefixOf "(," . conName) t
    = showParen True $ gpartialShowsPrec ',' appPrec $ Partial (unM1 t)
    -- Is a cons
    | ((== ":") . conName) t
    = showParen (p > 5) $ gpartialShowsPrec ':' 5 $ Partial (unM1 t)
    -- Is to be displayed prefix
    | otherwise
    = showParen (p > appPrec) $ showString (conName t) . 
      gpartialShowsPrec ' ' (appPrec + 1) (Partial (unM1 t))

instance (GSerial f) => GSerial (M1 S c f) where
  gseries = fmap0 M1 gseries
  gpartialShowsPrec c p = gpartialShowsPrec c p . fmap unM1
  
instance (GSerial f, GSerial g) => GSerial (f :*: g) where
  gseries = pure (:*:) `ap0` gseries `ap0` gseries
  gpartialShowsPrec _ _ x | isException x = ('_':)
  gpartialShowsPrec c p (Partial (x :*: y)) 
    = gpartialShowsPrec c (succ p) (Partial x) 
    . (c:) . gpartialShowsPrec c p (Partial y)

instance Serial ()
instance Serial Bool
instance (Serial a, Serial b) => Serial (a, b)
instance (Serial a, Serial b, Serial c) => Serial (a, b, c)
instance (Serial a, Serial b, Serial c, Serial d) => Serial (a, b, c, d)
instance (Serial a) => Serial [a]
instance (Serial a) => Serial (Maybe a)
instance (Serial a, Serial b) => Serial (Either a b)

primpartialShowsPrec :: Show a => Int -> Partial a -> ShowS
primpartialShowsPrec p x | isException x = ('_':)
primpartialShowsPrec p (Partial x) = showsPrec p x

drawnFrom :: (Depth -> [a]) -> Series a
drawnFrom f = Series $ map pure . f

instance Serial Char where
  series = drawnFrom $ \d -> take (d + 1) ['a'..]
  partialShowsPrec = primpartialShowsPrec
  
instance Serial Int where
  series = drawnFrom $ \d -> [(-d)..d]
  partialShowsPrec = primpartialShowsPrec
  
instance Serial Integer where
  series = drawnFrom $ \d -> [(-toInteger d)..toInteger d]
  partialShowsPrec = primpartialShowsPrec
  
-- Figure 8
data Property = Lift Bool | Not Property 
              | And Property Property | Implies Property Property
              | ForAll (Depth -> Depth) (Series Property)
              | Exists (Depth -> Depth) (Series Property)
                
-- Figure 9
counterexample :: Depth -> Series Property -> Maybe TVInfo
counterexample d xs = either (error "counterexample: Unhandled refinement") id $
                      refute 0 d xs
                      
refute :: Nesting -> Depth -> Series Property -> Either Refine (Maybe TVInfo)
refute n d xs = terms (runSeries xs d)
  where
    terms :: [Term Property] -> Either Refine (Maybe TVInfo)
    terms []      = Right Nothing
    terms (t:ts)  = case (join . runPartial . fmap prop) <$> tValue t (n, id) of
      TVE _     (Left (RefineAt (m, ps))) | m == n     -> terms  $ tExpand t (ps []) ++ ts
                                          | otherwise  -> Left   $ RefineAt (m, ps)
      TVE info  (Right False)                          -> Right  $ Just info
      TVE _     (Right True)                           -> terms  $ ts
    prop :: Property -> Either Refine Bool
    prop (Lift     v)     = pure       v
    prop (Not      p)     = not        <$> prop p
    prop (And      p q)   = (&&)       <$> prop p <*> prop q
    prop (Implies  p q)   = (==>)      <$> prop p <*> prop q
    prop (ForAll   f xs)  = isNothing  <$> refute (succ n) (f d) xs
    prop (Exists   f xs)  = isJust     <$> refute (succ n) (f d) (fmap0 Not xs)
    
(==>) :: Bool -> Bool -> Bool
False  ==> _ = True
True   ==> x = x

class Testable a where
  mkTest :: Series a -> Series Property
  
instance Testable Bool where
  mkTest  = fmap0 Lift
  
instance Testable Property where
  mkTest  = id
  
instance (Serial a, Testable b) => Testable (a -> b) where
  mkTest  srs = mkTest $ srs `ap0` seriesWithEnv
  
depthCheck :: Testable a => Depth -> a -> IO ()
depthCheck d x = case counterexample d (mkTest $ pure x) of
  Nothing   -> putStrLn $ "Passed!"
  Just env  -> putStrLn $ "Failed!\n" ++ show env

-- Figure 10
type (:->:) = Level1

data Level1 k v where
  Wild  :: v           -> Level1 k v
  Case  :: Level2 k v  -> Level1 k v
  
data Level2 k v where
  Valu  :: v                                       -> Level2 U1 v
  Sum   :: Level2 j v  -> Level2 k v               -> Level2 (j :+: k) v
  Prod  :: Level2 j (Level2 k v)                   -> Level2 (j :*: k) v
  Natu  :: [v] -> v                                -> Level2 Nat1 v
  Cast  :: (forall a. k -> k' a) -> (forall a. k' a -> k) ->
           (Level1 k' v)                           -> Level2 (K1 a k) v
  Meta  :: (Level2 k v)                            -> Level2 (M1 i c k) v

newtype Nat1 a = Nat1 { unNat1 :: Int }

applyT :: (k :->: v) -> k a -> v
applyT (Wild v) = const v
applyT (Case t) = applyL2 t

applyL2 :: Level2 k v -> k a -> v
applyL2 (Valu v)      _          = v
applyL2 (Sum t _)     (L1 k)     = t `applyL2` k
applyL2 (Sum _ t)     (R1 k)     = t `applyL2` k
applyL2 (Prod t)      (j :*: k)  = t `applyL2` j `applyL2` k
applyL2 (Natu m d)    (Nat1 k)   = foldr const d $ drop k m
applyL2 (Cast f _ t)  (K1 k)     = t `applyT` f k
applyL2 (Meta t)      (M1 k)     = t `applyL2` k

seriesT :: SeriesL2 k => Series v -> Series (k :->: v)
seriesT srs = (Wild `fmap0` srs) <|> (Case `fmap` seriesL2 srs)

class SeriesL2 k where
  seriesL2 :: Series v -> Series (Level2 k v)
  
instance SeriesL2 V1 where
  seriesL2 _ = empty

instance SeriesL2 U1 where
  seriesL2 = fmap0 Valu
  
instance (SeriesL2 j, SeriesL2 k) => SeriesL2 (j :+: k) where
  seriesL2 srs = Sum `fmap0` seriesL2 srs `ap0` seriesL2 srs
  
instance (SeriesL2 j, SeriesL2 k) => SeriesL2 (j :*: k) where
  seriesL2 srs = Prod `fmap0` seriesL2 (seriesL2 srs)
  
instance SeriesL2 Nat1 where
  seriesL2 srs = Natu `fmap0` fullSize 0 `ap0` srs
    where  fullSize o =  onlyZero [] <|> 
                         ((:) <$> deeperBy (+ o) srs <*> fullSize (o + 1))
           onlyZero x =  Series $ \d -> [ pure x | d == 0 ]

instance Argument k => SeriesL2 (K1 a k) where
  seriesL2 srs = case iso of
    Isomorphism fromIso toIso -> fmap0 (Cast fromIso toIso) (seriesT srs)

instance SeriesL2 f => SeriesL2 (M1 i c f) where
  seriesL2 srs = Meta `fmap0` seriesL2 srs

data Isomorphism source = forall generic. SeriesL2 generic => Isomorphism {
  fromIso :: forall a. source     -> generic a,
  toIso   :: forall a. generic a  -> source }
                      
class Argument a where
  iso :: Isomorphism a
  
  default iso :: (Generic a, SeriesL2 (Rep a)) => Isomorphism a
  iso = Isomorphism from to
  
isoPrim :: (a -> Int) -> (Int -> a) -> Isomorphism a
isoPrim f f' = Isomorphism (Nat1 . f) (f' . unNat1)

instance Argument ()
instance (Argument a, Argument b) => Argument (a, b)
instance Argument Char where
  iso = isoPrim fromEnum toEnum
  
data Tree a = Leaf a | Branch (Tree a) (Tree a)

instance Functor Tree where
  fmap f (Leaf x)     = Leaf (f x)
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)
  
instance Monad Tree where
  return = Leaf
  Leaf x      >>= f = f x
  Branch l r  >>= f = Branch (l >>= f) (r >>= f)

tabulateT :: (k :->: v) -> Tree (Partial (k a), v)
tabulateT (Wild v) = Leaf (refineAt undefined, v)
tabulateT (Case t) = tabulateL2 t

tabulateL2 :: (Level2 k v) -> Tree (Partial (k a), v)
tabulateL2 (Valu v)      = Leaf (pure U1, v)
tabulateL2 (Sum  l r)    = [ (L1 <$> k, v) | (k, v) <- tabulateL2 l ]
                              `Branch`
                           [ (R1 <$> k, v) | (k, v) <- tabulateL2 r ]
tabulateL2 (Prod t)      = [ ((:*:) <$> j <*> k, v)
                           | (j, t') <- tabulateL2 t 
                           , (k, v)  <- tabulateL2 t' ]
tabulateL2 (Natu m d)    = foldr Branch (Leaf (refineAt undefined, d)) 
                           [ Leaf (pure (Nat1 k), v) 
                           | (k, v) <- zip [0..] m ]
tabulateL2 (Cast _ f t)  = [ (K1 . f <$> k, v) | (k, v) <- tabulateT t ]
tabulateL2 (Meta t)      = [ (M1 <$> k, v) | (k, v) <- tabulateL2 t ]

flatten :: Partial (Tree a) => [a]
flatten x | isException x = []
flatten (Partial (Leaf x))     = [x]
flatten (Partial (Branch l r)) = flatten (Partial l) ++ flatten (Partial r)

instance (Argument a, Serial a, Serial b) => Serial (a -> b) where
  series = case iso of
    Isomorphism fromIso _ -> (\t -> applyT t . fromIso) `fmap0` seriesT series
  partialShowsPrec = error "partialShowsPrec on functions: Must be stored before conversion from trie."
  
  seriesWithEnv = case iso of
    Isomorphism fromIso toIso -> (\t -> applyT t . fromIso) `fmap0` aux (seriesT seriesWithEnv)
        where aux (Series xs) = Series $ fmap storeShow <$> xs
              mkEnv x e = [ partialShow (fmap toIso k) ++ " -> " ++ v
                          | (k, _) <- flatten $ fmap tabulateT x
                          | v <- e ]
              storeShow (TTerm (TVE e x))  = TTerm (TVE (mkEnv (Partial x) e) x)
              storeShow (PTerm v es s)     = PTerm
                ((fmap $ \(TVE e x) -> TVE (mkEnv x e) x) v)
                (fmap storeShow <$> es) s

instance Argument Bool
instance Argument a => Argument [a]

prop_ReduceFold :: ([Bool] -> Bool) -> Property
prop_ReduceFold r = Exists succ $ mkTest $ pure $ \f z -> ForAll id $ mkTest $ pure $ \xs -> r xs == foldr f z xs