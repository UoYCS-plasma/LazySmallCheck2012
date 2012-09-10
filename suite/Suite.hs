{-# LANGUAGE ExistentialQuantification #-}
import Control.Exception
import Control.Monad
import Data.Data
import Data.Typeable
import System.Exit

import Test.LazySmallCheck2012
import Test.LazySmallCheck2012.Core

main = sequence_ [ do putStrLn $ "\n## Test '" ++ str ++ "': "
                      expect v $ mapM_ (`depthCheck` t) [0..d]
                 | Test str t v d <- suite ]

expect :: Bool -> IO () -> IO ()
expect True  = id
expect False = either (\(SomeException _) -> return ()) (const exitFailure) <=< try

data Test = forall a. (Data a, Typeable a, Testable a) => 
            Test String a Bool Depth

suite = [test1, test2, test3, test4, test5, test6]

------------------------------------------------------------------------------------

-- From Runciman, Naylor and Lindblad 2008

-- isPrefix
isPrefix :: [Int] -> [Int] -> Bool
isPrefix []     _  = True
isPrefix _      [] = False
isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys

test1 = Test "isPrefix" (\xs ys -> isPrefix xs (xs ++ ys)) True 5
test2 = Test "flip isPrefix" (\xs ys -> flip isPrefix xs (xs ++ ys)) False 5

-- Set insert
type Set a = [a]

insert :: Char -> [Char] -> [Char]
insert x []     = [x]
insert x (y:ys) | x <= y    = x:y:ys
                | otherwise = y : insert x ys
                              
ordered :: Ord a => [a] -> Bool
ordered (x:y:zs) = x <= y && ordered (y:zs)
ordered _ = True

test3 = Test "Set insert" (\c s -> ordered s ==> ordered (insert c s)) True 5

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
                   existsDeeperBy (+2) (\xs' -> (xs ++ xs') == ys))
        False 5
        
test6 = Test "isPrefix_bad with existential" 
        (\xs ys -> isPrefix xs ys *==>* 
                   existsDeeperBy (+2) (\xs' -> (xs ++ xs') == ys))
        True 4