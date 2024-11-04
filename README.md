[![Actions Status](https://github.com/lizmat/Sequence-Generator/actions/workflows/linux.yml/badge.svg)](https://github.com/lizmat/Sequence-Generator/actions) [![Actions Status](https://github.com/lizmat/Sequence-Generator/actions/workflows/macos.yml/badge.svg)](https://github.com/lizmat/Sequence-Generator/actions) [![Actions Status](https://github.com/lizmat/Sequence-Generator/actions/workflows/windows.yml/badge.svg)](https://github.com/lizmat/Sequence-Generator/actions)

NAME
====

Sequence::Generator - generate sequences of values from endpoints

SYNOPSIS
========

```raku
use Sequence::Generator;

say 1,2,4 ... 64; # (1 2 4 8 16 32 64)
```

DESCRIPTION
===========

Raku provides a `...` operator (and its friends `...^`, `^...` and `^...^`) to generate values from a given set of endpoints. It is one of the oldest parts of the Raku Programming Language.

During its development, it obtained some quirks and behaviours that are now considered too magical, or considered to be a bug (which is sometimes actually frozen into spectests). On top of that, the development of the `...` operator preceded the occurrence of the `Iterator` role, so it actually does not use any of its benefits.

This module started out as an attempt to make the `...` operator (and friends) much faster by re-implementing it using `Iterator`s, rather than `gather` / `take`. However, it became clear that fixing behaviour considered too magical or buggy, could break existing Raku code. It was therefore decided to turn this work into this module, with the option of incorporating it into a later language version of Raku.

This module is between 4x and 20x faster than the Raku 6.d implementation.

RULES
=====

This describes the rules that are being applied to the begin and end points of these generated sequences.

Meaning of carets
-----------------

The carets `^` on the infix `...` operator are interpreted in a very strict way. On the left-hand side (`^...`) it means that the **initial** value will **not** be produced. On the right-hand side it means that the final value will **not** be produced.

Two Real numbers
----------------

If the end point is larger than the begin point, then the functionality is the same as the `..` infix operator: add **1** for each step until the value is larger than the end point value.

```raku
say 1 ... 10;    # (1 2 3 4 5 6 7 8 9 10)
say 1.5 ... 10;  # (1.5 2.5 3.5 4.5 5.5 6.5 7.5 8.5 9.5)
```

If the end point is smaller than the begin point, then **-1** is added for each step until the value is smaller than the end point value.

```raku
say 10 ... 1;   # (10 9 8 7 6 5 4 3 2 1)
say 10.5 ... 1; # (10.5 9.5 8.5 7.5 6.5 5.5 4.5 3.5 2.5 1.5)
```

If you need other increments or decrements, you must either use elucidation or provide a `Callable` that will do the production of values.

Sequence elucidation
--------------------

If the left hand side just consists of two or more `Real` numbers, then the last three (or two if there are only two) values will be used to try to determine the type of sequence in a process called "elucidation".

If the difference between the values is constant (or if there are only two values), then the sequence is called to be "arithmetic": the difference will then be assumed to the the step to be applied for each subsequent value produced.

```raku
say 1, 3 ... 10;     # (1 3 5 7 9)
say 10, 8, 6 ... 1;  # (10 8 6 4 2)
```

If the division between the values is constant, then the sequence is called to the "geometric": that value will then be used to multiply to produce the next value.

```raku
say 1 2 4 8 16 32 64;  # (1 2 4 8 16 32 64)
say 64, 32, 16 ... 1;  # (64 32 16 8 4 2 1)
```

If elucidation fails, a `Failure` will be returned with as much information possible to indicate the reason of the failure.

Non-numeric endpoints
---------------------

If the sequence has a non-numeric end point, then the sequence will continue to produce values until the generated value smartmatches with the end point.

```raku
say 5, { $_ ?? $_ - 1 !! "liftoff" } ... Str;
# (5 4 3 2 1 0 liftoff)
```

Two strings
-----------

A sequence can only be generated for two strings if they have the same number of characters and all of the characters are either in the range of `a .. z`, `A .. Z` or `0 .. 9`. Furthermore, the range of each character of the begin point needs to be in the same range as the associated end point.

Each character will be incremented / decremented according to its counterpart to generate strings, with the rightmost character being incremented / decremented first.

```raku
say "ac" ... "ba";  # (ac ab aa bc bb ba)
```

Any other combination of strings will return a `Failure`. If you want some other string based sequence semantics, you should probably be using the magic increment / decrementfunctionality on strings, as offered by the `.succ` and `.pred` methods, and use a `Callable` to produce the values.

```raku
say ("zy", *.succ ... *).head(8);   # (zy zz aaa aab aac aad aae aaf)

say ("¹²", *.pred ... *).head(20);  # (¹² ¹¹ ¹⁰ ⁰⁹ ⁰⁸ ⁰⁷ ⁰⁶ ⁰⁵ ⁰⁴ ⁰³ ⁰² ⁰¹ ⁰⁰)
```

BREAKING CHANGES
================

Semantics of multiple endpoints
-------------------------------

The original implementation of the `...` operator allowed a chain of endpoints to be specified:

```raku
say 1 ... 5 ... 1; # (1 2 3 4 5 4 3 2 1)
```

This is no longer supported because of the unclear semantics of an endpoint also serving as a starting point as soon as it is anything more than a numeric value. If you would like to have such a sequence, you will have to use parentheses to indicate meaning:

```raku
say 1 ... (5 ... 1); # (1 2 3 4 5 4 3 2 1)
```

Strict meaning of ^
-------------------

The original implementation of the `...^` operator treated omission of endpoints differently for numeric values:

```raku
say 1 ...^ 5.5; # (1 2 3 4 5)
```

This is generalized to **always** omit the last **generated** value, regardless of whether it actually compared exactly with the endpoint or not.

Using .pred ends sequence if Failure
------------------------------------

The original implementation of the `...` operator would die if `.pred` was being used to generate the next value, and that would return a Failure. This has been changed to ending the sequence.

No longer silently ignores values on LHS after Callable
-------------------------------------------------------

The original implementation of the `...` operator would ignore any values **after** a Callable on the LHS, e.g.:

```raku
1,2,3, * + 1, 7,8,9 ... 100;
```

This now returns a `Failure`.

No longer silently ignores values with RHS list starting with *
---------------------------------------------------------------

The original implementation of the `...` operator would ignore any values **after** a Whatever as the first element of a list on the RHS, e.g.:

```raku
1,2,3 ... *,42,666;
```

This now returns a `Failure`.

LHS elucidation should always have identical types
--------------------------------------------------

This implementation requires all values for sequence elucidation (either 2 elements on the left, or the last three of three or more values) to be either all Real, or of the same type. If they are not, the elucidation will fail. This behaviour makes quite a few edge cases fail that the original implementation of the `...` operator would try to make sense of.

Elucidation of LHS with identical values now fail
-------------------------------------------------

The original implementation of the `...` operator would produce unexplicable results if the 2 or the last 3 values of the LHS list would contain the same values. This will now return `Failure`.

Multiplication factor must be positive
--------------------------------------

In elucidation, any multiplication factor found must be positive. Negative multiplication factors are too magic with regards to determine when the sequence must be ended. Please use a WhateverCode (e.g. `* * -1`) to indicate a negative multiplication factor.

CLASS INTERFACE
===============

The `Sequence::Generator` class can not be instantiated: it merely serves as an entry point to the sequence generating logic. It exposes **one** method:

iterator
--------

The `iterator` method takes two positional arguments: the seed for the sequence (aka the left-hand side of the `...` infix operator) and the end point (aka the right-hand side of the `...` infix operator). It returns an `Iterator` object.

```raku
my $iterator = Sequence::Generator.iterator((1,2,4), 64);
say Seq.new($iterator);  # (1 2 4 8 16 32 64)
```

If you like to exclude the first or last generated value, you can pass the `:no-first` and/or the `:no-last` named arguments.

```raku
my $iterator = Sequence::Generator.iterator((1,2,4), 64, :no-first, :no-last);
say Seq.new($iterator);  # (2 4 8 16 32)
```

AUTHOR
======

Elizabeth Mattijsen <liz@raku.rocks>

Source can be located at: https://github.com/lizmat/Sequence-Generator . Comments and Pull Requests are welcome.

If you like this module, or what I'm doing more generally, committing to a [small sponsorship](https://github.com/sponsors/lizmat/) would mean a great deal to me!

COPYRIGHT AND LICENSE
=====================

Copyright 2020, 2024 Elizabeth Mattijsen

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

