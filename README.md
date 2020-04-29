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

LHS list with different types must have matching endpoint
---------------------------------------------------------

The original implementation of the `...` operator would try to smart-match the endpoint value with the final value on the LHS. If the types of that final value and the endpoint do not smartmatch, then the values of the final value and the endpoint will most likely also never smartmatch, e.g.:

    "a",1 ... "c";

would never stop producing values. This now dies.

LHS of identical values now assumes implicit .succ
--------------------------------------------------

The original implementation of the `...` operator would produce unexplicable results if the 2 or the last 3 values of the LHS list would contain the same values. This is now made more consistent, by assuming `.succ` to be applied on the last value of the LHS list, and apply the normal endpoint rules. So:

    1,1 ... *;            # 1 1 2 3 4 5 etc.

    1,1,1 ... *;          # 1 1 1 2 3 4 etc.

    1,1 ... 5;            # 1 1 2 3 4 5

    1,1,1 ... 5;          # 1 1 1 2 3 4 5

    1,1 ... 1;            # 1

    1,1,1 ... 1;          # 1

    1,1 ... 0;            #

    1,1,1 ... 0;          #

    "c","c" ... *;        # c c d e f g h i etc.

    "c","c","c" ... *;    # c c c d e f g h etc.

    "c","c" ... "g";      # c c d e f g

    "c","c","c" ... "g";  # c c c d e f g

Non-stepping elucidation
========================

AUTHOR
======

Elizabeth Mattijsen <liz@wenzperl.nl>

Source can be located at: https://github.com/lizmat/Sequence-Generator . Comments and Pull Requests are welcome.

COPYRIGHT AND LICENSE
=====================

Copyright 2020 Elizabeth Mattijsen

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

