# Contributing to Lazy SmallCheck 2012

Lazy SmallCheck 2012 is open-source software liscenced under BSD 
3-Clause License (see `LICENCE` and the 
[TLDRLegal explaination][BSD3]. We welcome contributions of;

*   Feature requests, bugs and questions to the
    [issue tracker on GitHub][Issues].
*   Well-documented patches from project forks, through the 
    [pull request interface on GitHub][Pull]. *N.b. Please ensure that
    the `suite/Functionality.hs` tests suite passes before submission.*
*   Case studies and examples on the [project wiki on GitHub][Wiki].

## Repository tour

### Preamble

*  `CONTRIBUTING.md` — This file explaining how to contribute to the
   project. 
*  `LazySmallCheck2012.cabal` — A cabal file for building, testing and
   benchmarking the project. 
*  `LICENCE` — BSD 3-Clause License.
*  `README.md` — Description of project with brief usage information.

### Library

*   `Test/LazySmallCheck2012.hs` — Root import for LSC2012. Includes
    all functionality *apart* from Template Haskell deriving. 
*   `Test/PartialValues.lhs`  — Library for handling values that may
    contain exceptions. 
*   `Test/LazySmallCheck2012/Core.lhs`  — Core LSC2012 data structures
    and functions. 
*   `Test/LazySmallCheck2012/FunctionalValues.lhs`  — Functional value
    library for LSC2012. 
*   `Test/LazySmallCheck2012/Instances.hs`  — Instances of Serial
    class for common Haskell 98 datatypes. 
*   `Test/LazySmallCheck2012/Stats.lhs`  — Collects statistics on
    pruning power.
*   `Test/LazySmallCheck2012/TH.hs`  — Template Haskell functions for
    deriving Serial and Argument instances. 
*   `Test/LazySmallCheck2012/FunctionalValues/Instances.hs`  —
    Instances of Argument class for common Haskell 98 datatypes. 

### Test suite

*  `suite/Functionality.hs` — Regression tests for LSC2012.
*  `suite/Performance.hs` — Benchmark tests for LSC2008 and LSC2012.
*  `suite/performance.sh` — Script for running benchmarks and
   cataloguing the results. 
*  `suite/performance/` — Contains auxiliary source and results for
   benchmarks. 
   
[BSD3]: http://www.tldrlegal.com/l/BSD3
[Issues]: https://github.com/UoYCS-plasma/LazySmallCheck2012/issues
[Pull]: https://help.github.com/articles/using-pull-requests
[Wiki]: https://github.com/UoYCS-plasma/LazySmallCheck2012/wiki
