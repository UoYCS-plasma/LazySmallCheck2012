{-# LANGUAGE ExistentialQuantification, DeriveGeneric,
             DeriveFunctor, TypeFamilies #-}
{-# OPTIONS_GHC -ignore-dot-ghci #-}

import Control.Exception
import Control.Monad
import GHC.Generics
import System.Exit

import Test.LazySmallCheck

main = do mapM_ runTest suite
          putStrLn "\nSuite: Test suite complete."

runTest (Test str t v d) = do putStrLn $ "\n## Test '" ++ str ++ "': "
                              expect v $ mapM_ (`depthCheck` t) [0..d]
                              putStrLn $ "## Test response correct."

expect :: Bool -> IO () -> IO ()
expect True  = id
expect False = either (\(SomeException _) -> return ()) (const exitFailure) <=< try

data Test = forall a. (Testable a) => 
            Test String a Bool Depth

suite = [ test1, test2, test3, test4, test5, test6, test7, test8
        , test9, test10, test11a, test11b, test11c
        , test12a, test12b, test12c, test13, test14a {- test14b -} ]

------------------------------------------------------------------------------------

-- From Runciman, Naylor and Lindblad 2008

-- isPrefix
isPrefix :: [Int] -> [Int] -> Bool
isPrefix []     _  = True
isPrefix _      [] = False
isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys

test1 = Test "isPrefix" (\xs ys -> isPrefix xs (xs ++ ys)) True 5
test2 = Test "flip isPrefix" (\xs ys -> flip isPrefix xs (xs ++ ys)) False 5

-- Ordered List insert
insert :: Char -> [Char] -> [Char]
insert x []     = [x]
insert x (y:ys) | x <= y    = x : y : ys
                | otherwise = y : insert x ys
                              
ordered :: Ord a => [a] -> Bool
ordered (x:y:zs) = x <= y && ordered (y:zs)
ordered _ = True

test3 = Test "Ordered List insert" 
        (\c s -> ordered s ==> ordered (insert c s)) True 5
        
-- Set insert

allDiff []     = True
allDiff (x:xs) = x `notElem` xs && allDiff xs

isSet :: Ord a => Bool -> [a] -> Property
isSet False xs = ordered xs *&&* allDiff xs
-- isSet True  xs = ordered xs |&&| allDiff xs

setinsert :: Char -> [Char] -> [Char]
setinsert x []     = [x]
setinsert x (y:ys) | x == y    = y : ys
                   | x <= y    = x : y : ys
                   | otherwise = y : setinsert x ys

test14a = Test "Set insert -- sequential conjunction" 
        (\c s -> isSet False s *==>* isSet False (setinsert c s))
        True 5


test14b = Test "Set insert -- parallel conjunction" 
        (\c s -> isSet True s *==>* isSet True (setinsert c s))
        True 5

-- Associativity of Boolean
test4 = Test "Associativity of binary Boolean functions"
        (\f x y z -> let typ = f :: Bool -> Bool -> Bool
                     in f (f x y) z == f x (f y z)) False 5
        
-- isPrefix again
isPrefix_bad :: [Char] -> [Char] -> Bool
isPrefix_bad []     _  = True
isPrefix_bad _      [] = False
isPrefix_bad (x:xs) (y:ys) = x == y || isPrefix_bad xs ys

test5 = Test "isPrefix_bad with existential" 
        (\xs ys -> isPrefix_bad xs ys *==>* 
                   exists (\xs' -> (xs ++ xs') == ys))
        False 5
        
test6 = Test "isPrefix_bad with existential" 
        (\xs ys -> isPrefix xs ys *==>* 
                   exists (\xs' -> (xs ++ xs') == ys))
        True 4
                
------------------------------------------------------------------------------------

-- From Reich, Naylor and Runciman, 2012

-- Reductions to folds
test7 = Test "All reductions are folds"
        (\r -> let typ = r :: [Bool] -> Bool
               in exists $ 
                  \f z -> forAll $ \xs -> r xs == foldr f z xs)
        False 5

data Peano = Zero | Succ Peano 
           deriving (Generic, Eq, Ord, Show)

instance Serial Peano
instance Argument Peano

-- foldr1 f == foldl1 f

test8 = Test "foldr1 is the same as foldl1"
        (\f xs -> let typ = f :: Peano -> Peano -> Peano
                  in (not.null) xs ==> (foldr1 f xs == foldl1 f xs))
        False 5
        
----------------------------------------------------------------------

-- From Claessen and Hughes, 2000

-- (f . g) . h == f . (g . h)

test9 = Test "Associativity of compose."
        (\f g h x -> let typ_f = f :: Peano -> Peano
                         typ_g = g :: Peano -> Peano
                         typ_h = h :: Peano -> Peano
                     in (f . (g . h)) x == ((f . g) . h) x)
        True 7
        
----------------------------------------------------------------------

-- From Claessen, 2012

-- Heaps are safe from fmaps

data Heap a = HEmpty | HNode a (Heap a) (Heap a) 
            deriving (Functor, Show, Generic)
                     
instance Serial a => Serial (Heap a) where 
  series = cons0 HEmpty <|> cons3 HNode

invariant :: Ord a => Heap a -> Bool
invariant HEmpty = True
invariant p@(HNode x _ _) = top x p
  where top x HEmpty = True
        top x (HNode y p q) = x <= y && top y p && top y q
        
test10 = Test "Any fmap over the heap maintains the invariant."
         (\h f -> invariant h *==>* 
                  invariant (fmap (f :: Peano -> Peano) h))
         False 5
         
-- Find a predicate on strings

test13 = Test "Some large string"
         (\p -> p "some long string" ==> p "some other string")
         False 30

-- Clock/Emit is a monad

data ClockEmit a = Step (ClockEmit a) 
                 | Emit a (ClockEmit a)
                 | Stop
                 deriving (Eq, Show, Generic)
                   
instance Serial a => Serial (ClockEmit a) where
  series = cons0 Stop <|> cons2 Emit <|> cons1 Step
  
(+++) :: ClockEmit a -> ClockEmit a -> ClockEmit a
Stop     +++ q        = q
p        +++ Stop     = p
Emit x p +++ q        = Emit x (p +++ q)
p        +++ Emit x q = Emit x (p +++ q)
Step p   +++ Step q    = Step (p +++ q)

instance Monad ClockEmit where
  return x     = Emit x Stop
  Stop     >>= k = Stop
  Step m   >>= k = Step (m >>= k)
  Emit x m >>= k = k x +++ (m >>= k)
  
test11a = Test "ClockBind obeys Return/Bind"
          (\x f -> let typ_f = f :: Bool -> ClockEmit Bool
                   in (return x >>= f) == f x)
          True 5
          
test11b = Test "ClockBind obeys Bind/Return"
          (\xs -> let typ_xs = xs :: ClockEmit Bool
                  in (xs >>= return) == xs)
          True 5
          
test11c = Test "ClockBind obeys Bind/Bind"
          (\xs f g -> let typ_xs = xs :: ClockEmit Bool
                          typ_f  = f  :: Bool -> ClockEmit Bool
                          typ_g  = g  :: Bool -> ClockEmit Bool
                      in (xs >>= (\x -> f x >>=g)) == ((xs >>= f) >>= g))
          False 5
          
----------------------------------------------------------------------

-- Contributed by Domonic Orchard at IFL 2012

-- Is Foo a comonad?

data Foo a = Foo a a a deriving (Show, Generic, Eq)
instance Serial a => Serial (Foo a)
instance Argument a => Argument (Foo a)

{-
Now perfomed using deriveSerial and deriveArgument

instance Serial a => Serial (Foo a) where
  series = cons3 Foo

instance Argument a => Argument (Foo a) where
  type Base (Foo a) = (BaseThunk a, (BaseThunk a, BaseThunk a))
  toBase (Foo x y z) = (toBaseThunk x, (toBaseThunk y, toBaseThunk z))
  fromBase (x, (y, z)) = Foo (fromBaseThunk x) (fromBaseThunk y) (fromBaseThunk z)
-}
  
coreturn (Foo x _ _) = x
cobind f (Foo x y z) = Foo (f $ Foo x y z) (f $ Foo y x z) (f $ Foo z x y)

test12a = Test "Foo obeys Cobind/Coreturn"
          (\xs -> let typ_xs = xs :: Foo Bool
                  in cobind coreturn xs == xs)
          True 5
          
test12b = Test "Foo obeys Coreturn/Cobind"
          (\f xs -> let typ_f = f :: Foo Bool -> Bool 
                    in (coreturn . cobind f) xs == f xs)
          True 5

test12c = Test "Foo obeys Cobind/Cobind"
          (\f g xs -> let typ_f = f :: Foo Bool -> Bool 
                          typ_g = g :: Foo Bool -> Bool 
                      in (cobind f . cobind g) xs == cobind (f . cobind g) xs)
          False 5
