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
