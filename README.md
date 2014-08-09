sci-ratio [![Build status][ci]][ca]
=================================

This package provides utilities for parsing and pretty-printing exact rational
numbers with arbitrarily large exponents.  Numbers are represented using the
`SciRatio a b` type, which can be thought of as a rational number combined
with a power of ten:

    r .^ e == r * 10 ^^ e

The primary advantage of this data type is that it's capable of storing
extremely large exponents without issues, although *performing arithmetic* on
them, however, is not guaranteed to work in some cases (e.g. adding numbers
with wildly differing exponents).

Numbers are parsed and pretty-printed in a format similar to:

    +0.000e+00/1.000

The sign, decimal point, exponent, and denominator are all optional.

Documentation can be found [here][doc].

Take note of the [versioning policy of this package][pvp].

[ca]:  https://travis-ci.org/Rufflewind/sci-ratio
[ci]:  https://travis-ci.org/Rufflewind/sci-ratio.svg?branch=master
[doc]: http://rufflewind.com/sci-ratio
[pvp]: https://gist.github.com/Rufflewind/03f4e03f7cfa52b8f07d
