LazySmallCheck2012
==================

Lazy SmallCheck with functional values and existentials!

[![Build Status](https://secure.travis-ci.org/UoYCS-plasma/LazySmallCheck2012.png)](http://travis-ci.org/UoYCS-plasma/LazySmallCheck2012)

_This code is mid-clean up. Many features are not yet
included. Watch this space._

A property-based testing library enables users to perform
lightweight verification of software. This repo presents
improvements to the Lazy SmallCheck property-based testing
library. Users can now test properties that quantify over 
first-order functional values and nest universal and
existential quantifiers in properties. When a property 
fails, Lazy SmallCheck now accurately expresses the 
partiality of the counterexample. The necessary
architectural changes to Lazy SmallCheck result in a
performance speed-up. All of these improvements are
demonstrated through several practical examples.

Motivation
----------

Consider the following conjectured property that in Haskell
all reductions on lists of Boolean values to a single
Boolean value can be expressed as a `foldr`. 

``` haskell
prop_ReduceFold :: ([Bool] -> Bool) -> Property
prop_ReduceFold r = existsDeeperBy (+2) $ \f z -> 
                    forAll $ \xs -> r xs == foldr f z xs
```

When this property is tested using our advanced version of *Lazy
SmallCheck*, a small counterexample is found for `r`.

    >>> test prop_ReduceFold
    ...
    LSC: Depth 2:
    LSC: Counterexample found after 374 tests.
    
    Var 0: { [] -> False
           ; _:[] -> False
           ; _:_:_ -> True }
    *** Exception: ExitFailure 1
	   
Reading the output in the style of Haskell's case-expression syntax
in explicit layout, this function tests for a multi-item list. Several
new features of Lazy SmallCheck are demonstrated by this example.
First, note that two of the quantified variables, `r` and `f`,
are *functional values*. Secondly, an *existential quantifier* is used
in the property definition. Thirdly, the property involves *nesting of
universal and existential quantifiers* inside the property.
Finally, the counterexample found for `r` is *concise* and easy to
understand.

The previous version of Lazy SmallCheck did not include any of these
features.


Usage
-----

*Note: Follow this until I upload it to Hackage.*

1) Download this repository.

    $ git clone https://github.com/UoYCS-plasma/LazySmallCheck2012.git
    $ cd LazySmallCheck2012
    
2) Enter the GHC interactive mode.

    $ ghci
    ...
     ##########################################
     #  LazySmallCheck 2012 GHCi Environment  #
     ##########################################
     :suite   --- Run test suite.
     :lsc2012 --- Reset environment.
     :readme  --- View README.md.
     
    *LSC2012>
     
3) Run the test suite to ensure that everything is working correctly.
If it doesn't, please report the issue.

    *LSC2012> :suite
    ...
    Suite: Test suite complete.

    Press <ENTER> to continue...
    
Hit enter to continue and you'll return to the environment from (3).

5) Try some simple properties over `Prelude` types. For example;

    *LSC2012> test $ \x xs -> head (x : (xs :: [Int])) == x
    LSC: Depth 0:
    LSC: Property holds after 1 tests.
    LSC: Depth 1:
    LSC: Property holds after 4 tests.
    LSC: Depth 2:
    LSC: Property holds after 6 tests.
    ...
    LSC: Depth 137:
    LSC: Property holds after 276 tests.
    <CTRL-C>
    
Or a failing property.

    *LSC2012> test $ \xs n -> length (take n (xs :: [Int])) == n
    LSC: Depth 0:
    LSC: Property holds after 1 tests.
    LSC: Depth 1:
    LSC: Counterexample found after 2 tests.

    Var 0: _
    Var 1: -1
    *** Exception: ExitFailure 1
    
    *LSC2012> test $ \xs n -> 0 <= n ==> length (take n (xs :: [Int])) == n
    LSC: Depth 0:
    LSC: Property holds after 1 tests.
    LSC: Depth 1:
    LSC: Counterexample found after 5 tests.
    
    Var 0: []
    Var 1: 1
    *** Exception: ExitFailure 1
    
    *LSC2012> test $ \xs n -> 0 <= n ==> length (take n (xs :: [Int])) <= n
    ...
    <CTRL-C>
