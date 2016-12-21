# speedy-slice

[![Build Status](https://secure.travis-ci.org/jtobin/speedy-slice.png)](http://travis-ci.org/jtobin/speedy-slice)
[![Hackage Version](https://img.shields.io/hackage/v/speedy-slice.svg)](http://hackage.haskell.org/package/speedy-slice)
[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/jtobin/speedy-slice/blob/master/LICENSE)

Speedy slice sampling, as per [Neal, 2003](http://people.ee.duke.edu/~lcarin/slice.pdf).

This implementation of the slice sampling algorithm uses `lens` as a means to
operate over generic indexed traversable functors, so you can expect it to
work if your target function takes a list, vector, map, sequence, etc. as its
argument.

Additionally you can sample over anything that's an instance of both `Num` and
`Variate`, which is useful in the case of discrete parameters.

Exports a `mcmc` function that prints a trace to stdout, a `chain` function for
working with results in memory, and a `slice` transition operator that can be
used more generally.

    import Numeric.MCMC.Slice
    import Data.Sequence (Seq, index, fromList)

    bnn :: Seq Double -> Double
    bnn xs = -0.5 * (x0 ^ 2 * x1 ^ 2 + x0 ^ 2 + x1 ^ 2 - 8 * x0 - 8 * x1) where
      x0 = index xs 0
      x1 = index xs 1

    main :: IO ()
    main = withSystemRandom . asGenIO $ mcmc 10000 1 (fromList [0, 0]) bnn

![trace](https://dl.dropboxusercontent.com/spa/u0s6617yxinm2ca/zp-9gl6z.png)

