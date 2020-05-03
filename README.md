NAME
====

Sequence::Generator - generate sequences of values from endpoints

SYNOPSIS
========

    use Sequence::Generator;

    say 1,2,4 ... 64; # (1 2 4 8 16 32 64)

DESCRIPTION
===========

Raku provides a `...` operator (and its friends `...^`, `^...` and `^...^`) to generate values from a given set of endpoints. It is one of the oldest parts of the Raku Programming Language. During its development, it obtained some quirks and behaviours that are now considered too magical, or considered to be a bug (which is sometimes actually frozen into spectests). On top of that, the development of the `...` operator preceded the occurrence of the `Iterator` role, so it actually does not use any of its benefits.

This module started out as an attempt to make the `...` operator (and friends) much faster by re-implementing it using `Iterator`s, rather than `gather` / `take`. However, it became clear that fixing behaviour considered too magical or buggy, could break existing Raku code. It was therefore decided to turn this work into this module, with the option of incorporating it into a later language version of Raku.

This module is between 4x and 20x faster than the Raku 6.d implementation.

BREAKING CHANGES
================

Semantics of multiple endpoints
-------------------------------

The original implementation of the `...` operator allowed a chain of endpoints to be specified:

    say 1 ... 5 ... 1; # (1 2 3 4 5 4 3 2 1)

This is no longer supported because of the unclear semantics of an endpoint also serving as a starting point as soon as it is anything more than a numeric value. If you would like to have such a sequence, you will have to use parentheses to indicate meaning:

    say 1 ... (5 ... 1); # (1 2 3 4 5 4 3 2 1)

Strict meaning of ^
-------------------

The original implementation of the `...^` operator treated omission of endpoints differently for numeric values:

    say 1 ...^ 5.5; # (1 2 3 4 5)

This is generalized to **always** omit the last **generated** value, regardless of whether it actually compared exactly with the endpoint or not.

Using .pred ends sequence if Failure
------------------------------------

The original implementation of the `...` operator would die if `.pred` was being used to generate the next value, and that would return a Failure. This has been changed to ending the sequence.

No longer silently ignores values on LHS after Callable
-------------------------------------------------------

The original implementation of the `...` operator would ignore any values **after** a Callable on the LHS, e.g.:

    1,2,3, * + 1, 7,8,9 ... 100;

This now dies.

No longer silently ignores values with RHS list starting with *
---------------------------------------------------------------

The original implementation of the `...` operator would ignore any values **after** a Whatever as the first element of a list on the RHS, e.g.:

    1,2,3 ... *,42,666;

This now dies.

LHS elucidation should always have identical types
--------------------------------------------------

This implementation requires all values for sequence elucidation (either 2 elements on the left, or the last three of three or more values) to be either all Real, or of the same type. If they are not, the elucidation will fail. This behaviour makes quite a few edge cases fail that the original implementation of the `...` operator would try to make sense of.

Elucidation of LHS with identical values now fail
-------------------------------------------------

The original implementation of the `...` operator would produce unexplicable results if the 2 or the last 3 values of the LHS list would contain the same values. This will now die.

Multiplication factor must be positive
--------------------------------------

In elucidation, any multiplication factor found must be positive. Negative multiplication factors are too magic with regards to determine when the sequence must be ended. Please use a WhateverCode (e.g. `* * -1`) to indicate a negative multiplication factor.

AUTHOR
======

Elizabeth Mattijsen <liz@wenzperl.nl>

Source can be located at: https://github.com/lizmat/Sequence-Generator . Comments and Pull Requests are welcome.

COPYRIGHT AND LICENSE
=====================

Copyright 2020 Elizabeth Mattijsen

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

