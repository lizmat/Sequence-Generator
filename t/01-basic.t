use v6.c;
use Test;

use Sequence::Generator;

my @types = < ... ...^ ^... ^...^ >;

plan @types * 3;

for @types -> $type {

    my $name = "&infix:<$type>";
    ok UNIT::{$name}:exists,
      "is $name imported?";
    nok Sequence::Generator::{$name}:exists,
      "is $name not externally accessible?";
    nok MY::{$name} === CORE::{$name},
      "not the same as the core $name";
}

# vim: ft=perl6 expandtab sw=4
