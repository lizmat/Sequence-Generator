use v6.c;

# This module is intended to be part of the Rakudo core in the
# foreseeable future.

use nqp;

class Sequence::Generator:ver<0.0.1>:auth<cpan:ELIZABETH> {

#- copy of Rakudo::Iterator ----------------------------------------------------

    # Return iterator that produces all but the first value
    method AllButFirst(\iterator) { iterator.skip-one; iterator }

    # Create iterator that produces all values *except* the last of a given
    # iterator.  Returns an empty iterator if the given iterator did not
    # produce any value.
    my role AllButLastRole {
        has $!iterator;
        has $!value;

        method !SET-SELF(\iterator) {
            $!iterator := iterator;
            nqp::eqaddr(($!value := iterator.pull-one),IterationEnd)
              ?? ().iterator
              !! self
        }
        method new(\iterator) { nqp::create(self)!SET-SELF(iterator) }
        method pull-one() is raw {
            my \this := $!value;
            nqp::eqaddr(($!value := $!iterator.pull-one),IterationEnd)
              ?? $!value
              !! this
        }
    }
    my class AllButLast does Iterator does AllButLastRole { }
    my class AllButLastPredictive does PredictiveIterator does AllButLastRole {
        method count-only() { ($!iterator.count-only || 1) - 1 }
        method bool-only()  {  $!iterator.count-only > 1       }
    }

    proto method AllButLast(|) {*}
    multi method AllButLast(PredictiveIterator:D \iterator) {
        AllButLastPredictive.new(iterator)
    }
    multi method AllButLast(Iterator:D \iterator) {
        AllButLast.new(iterator)
    }
#-------------------------------------------------------------------------------

    proto method iterator(|) {*}

    # Return unending iterator for constant numeric increment
    class UnendingStep does Iterator {
        has $!value;
        has $!step;
        method !SET-SELF(\start, \step) {
            $!value := start;
            $!step  := step;
            self
        }
        method new(\start,\step) { nqp::create(self)!SET-SELF(start,step) }
        method pull-one() { $!value := $!value + $!step }
        method is-lazy(--> True) { }
    }

    # Return iterator for constant numeric negative increment to given numeric
    class StepDownto does PredictiveIterator {
        has $!value;
        has $!step;
        has $!downto;
        method !SET-SELF(\start, \step, \downto) {
            $!value  := start;
            $!step   := step;
            $!downto := downto;
            self
        }
        method new(\start,\step,\downto) {
            nqp::create(self)!SET-SELF(start,step,downto)
        }
        method pull-one() {
            ($!value := $!value + $!step) < $!downto
              ?? IterationEnd
              !! $!value
        }
        method count-only(--> Int:D) { (($!downto - $!value) / $!step).Int }
        method bool-only(--> Bool:D) { $!value + $!step >= $!downto }
    }

    # Return iterator for constant numeric increment upto given numeric
    class StepUpto does PredictiveIterator {
        has $!value;
        has $!step;
        has $!upto;
        method !SET-SELF(\start, \step, \upto) {
            $!value := start;
            $!step  := step;
            $!upto  := upto;
            self
        }
        method new(\start,\step,\upto) {
            nqp::create(self)!SET-SELF(start,step,upto)
        }
        method pull-one() {
            ($!value := $!value + $!step) > $!upto
              ?? IterationEnd
              !! $!value
        }
        method count-only(--> Int:D) { (($!upto - $!value) / $!step).Int }
        method bool-only(--> Bool:D) { $!value + $!step <= $!upto }
    }

    # Return iterator for 2 numeric endpoints
    multi method iterator(
      Numeric:D \first, Numeric:D \endpoint, Int:D $no-first, Int:D $no-last
    --> Iterator:D) {
        my \iterator := endpoint < first
          ?? endpoint == -Inf
            ?? UnendingStep.new(first + 1 - $no-first, -1)
            !! StepDownto.new(first + 1 - $no-first, -1, endpoint)
          !! endpoint == Inf
            ?? UnendingStep.new(first - 1 + $no-first, +1)
            !! StepUpto.new(first - 1 + $no-first, +1, endpoint);

        $no-last
          ?? self.AllButLast(iterator)
          !! iterator
    }

    # Return iterator for numeric ... Whatever
    multi method iterator(
      Numeric:D \first, Whatever, Int:D $no-first, Int:D $
    --> Iterator:D) {
        UnendingStep.new(first - 1 + $no-first, 1)
    }

    # Return iterator for stepping for a single codepoint
    class StepCodepoint does PredictiveIterator {
        has int $!codepoint;
        has int $!step;
        has int $!todo;
        method !SET-SELF(
          int $first, int $last, int $no-first, int $no-last
        --> Iterator:D) {
            if $last > $first {
                $!step      = 1;
                $!codepoint = $first - 1 + $no-first;
                $!todo      = $last - $first + 2 - $no-first - $no-last;
                self
            }
            elsif $first > $last {
                $!step      = -1;
                $!codepoint = $first + 1 - $no-first;
                $!todo      = $first - $last + 2 - $no-first - $no-last;
                self
            }
            else {
                $no-first || $no-last
                  ?? ().iterator
                  !! nqp::chr($first).iterator
            }
        }
        method new(\first, \last, int $no-first, int $no-last) {
            nqp::create(self)!SET-SELF(
              nqp::ord(first), nqp::ord(last), $no-first, $no-last
            )
        }
        method pull-one() {
            --$!todo
              ?? nqp::chr($!codepoint = nqp::add_i($!codepoint,$!step))
              !! IterationEnd
        }
        method push-all(\target --> IterationEnd) {
            my int $todo = $!todo;
            my int $cp   = $!codepoint;
            my int $step = $!step;

            nqp::if(
              $todo,
              nqp::while(
                --$todo,
                target.push(nqp::chr($cp = nqp::add_i($cp,$step)))
              )
            );
            $!todo = $todo;  # make sure count/bool-only are correct
        }
        method count-only() { $!todo }
        method bool-only() { nqp::hllbool($!todo) }
    }

    # Helper sub to make a nqp::list_s for 2 codepoints
    sub cps2list_s(int $from, int $to) {
        my int $step = $from < $to ?? 1 !! -1;
        my int $this = $from - $step;

        my $chars := nqp::list_s();
        nqp::until(
          nqp::iseq_i(($this = nqp::add_i($this,$step)),$to),
          nqp::push_s($chars,nqp::chr($this))
        );
        nqp::push_s($chars,nqp::chr($this));

        $chars
    }

    # Class to return an iterator for 2 strings of equal length in chars
    # as well as codepoints.  Each string is split into graphemes, and
    # associated graphemes are built into list_s of all the graphemes
    # basically $a.comb Z.. $b.comb.  The list_s of the last grapheme of
    # each string is then separated and the remaining list_s's are equipped
    # with indices.  The result list_s is seeded with the first element
    # of each of the list_s's.  Pulling a value will increment the pointer
    # for the last grapheme: if it is out of range, then the next list_s's
    # index will be incremented, the result seeded and the pointer for the
    # last grapheme reset.  Until all combinations have been produced.
    class StepMultipleCodepoints does Iterator {
        has int $!i;    # current index
        has $!lists;    # list of list_s with chars (except most iterated)
        has $!indices;  # list_i of indices of $!lists
        has $!lowest;   # lowest list_s with chars (most iterated)
        has $!result;   # list_s of result chars to be joined

        method !SET-SELF(
          str $first, str $last, int $no-first, int $no-last
        --> Iterator:D) {

            # set up source lists
            my $lists  := nqp::create(IterationBuffer);
            my $result := nqp::list_s;
            my int $i  = -1;
            nqp::while(
              nqp::islt_i(++$i,nqp::chars($first)),
              nqp::stmts(
                (my $chars :=
                  cps2list_s(nqp::ordat($first,$i),nqp::ordat($last,$i))),
                nqp::push($lists,$chars),
                nqp::push_s($result,nqp::atpos_s($chars,0))
              )
            );

            # set up attributes
            $!i = -1;
            $!lowest  := nqp::pop($lists);
            $!indices := nqp::setelems(nqp::list_i(),nqp::elems($lists));
            $!lists   := $lists;
            $!result  := $result;

            # set up iterator wrt to endpoints
            self.skip-one if $no-first;
            $no-last
              ?? self.AllButLast(self)
              !! self
        }
        method new(\first, \last, int $no-first, int $no-last) {
            nqp::create(self)!SET-SELF(first, last, $no-first, $no-last)
        }

        method pull-one() {
            nqp::if(
              nqp::islt_i(++$!i,nqp::elems($!lowest)),
              nqp::stmts(
                nqp::bindpos_s($!result,-1,nqp::atpos_s($!lowest,$!i)),
                nqp::join('',$!result)
              ),
              self!lowest-has-run-out
            )
        }

        method !lowest-has-run-out() {

            # reset lowest char and index
            nqp::bindpos_s($!result,-1,nqp::atpos_s($!lowest,($!i = 0)));

            # loop for higher lists
            my int $index = nqp::elems($!indices);
            nqp::while(
              nqp::isge_i(--$index,0),
              nqp::if(         # still lists to check
                nqp::islt_i(
                  (my int $j = nqp::bindpos_i($!indices,$index,
                    nqp::add_i(nqp::atpos_i($!indices,$index),1)
                  )),
                  nqp::elems(nqp::atpos($!lists,$index))
                ),
                nqp::stmts(    # still available in this list, return that
                  nqp::bindpos_s(
                    $!result,$index,nqp::atpos_s(nqp::atpos($!lists,$index),$j)
                  ),
                  (return nqp::join('',$!result))
                ),
                nqp::stmts(    # not available in this list, move to next one
                  nqp::bindpos_s(
                    $!result,$index,nqp::atpos_s(nqp::atpos($!lists,$index),0)
                  ),
                  nqp::bindpos_i($!indices,$index,0)
                )
              )
            );

            IterationEnd
        }
    }

    class SuccPredStr does Iterator {

        method !SET-SELF(\first, \last, int $no-first, int $no-last) {

            # set up iterator wrt to endpoints
            self.skip-one if $no-first;
            $no-last
              ?? self.AllButLast(self)
              !! self
        }
        method new(\first, \last, int $no-first, int $no-last) {
            nqp::create(self)!SET-SELF(first, last, $no-first, $no-last)
        }
        method pull-one() { ... }
    }

    # Return iterator for two string endpoints
    multi method iterator(
      Str:D $first, Str:D $last, Int:D $no-first, Int:D $no-last
    --> Iterator:D) {
        nqp::chars($first) == nqp::chars($last)
          ?? $first eq $last                            # same length
            ?? $no-first || $no-last                     # same string
              ?? ().iterator                              # some end excluded
              !! $first.iterator                          # just the one please
            !! nqp::codes($first) == nqp::codes($last)   # different string
              ?? nqp::codes($first) == 1                  # same # of codepoints
                ?? StepCodepoint.new(                      # just one codepoint
                     $first, $last, $no-first, $no-last)
                !! StepMultipleCodepoints.new(             # multiple codepoints
                     $first, $last, $no-first, $no-last)
              !! SuccPredStr.new(                         # different codepoints
                   $first, $last, $no-first, $no-last)
          !! SuccPredStr.new(                           # not same length
               $first, $last, $no-first, $no-last)
    }
}

#-------------------------------------------------------------------------------
# set up the operator front-end

my sub infix:<...>(Mu \a, Mu \b) is export is equiv(&infix:<...>){
    Seq.new: Sequence::Generator.iterator(a, b, 0, 0)
}
my sub infix:<...^>(Mu \a, Mu \b) is export is equiv(&infix:<...>){
    Seq.new: Sequence::Generator.iterator(a, b, 0, 1)
}
my sub infix:<^...>(Mu \a, Mu \b) is export is equiv(&infix:<...>){
    Seq.new: Sequence::Generator.iterator(a, b, 1, 0)
}
my sub infix:<^...^>(Mu \a, Mu \b) is export is equiv(&infix:<...>){
    Seq.new: Sequence::Generator.iterator(a, b, 1, 1)
}

=begin pod

=head1 NAME

Sequence::Generator - generate sequences of values from endpoints

=head1 SYNOPSIS

  use Sequence::Generator;

  say 1,2,4 ... 64; # (1 2 4 8 16 32 64)

=head1 DESCRIPTION

Raku provides a C<...> operator (and its friends C<...^>, C<^...> and C<^...^>)
to generate values from a given set of endpoints.  It is one of the oldest
parts of the Raku Programming Language.  During its development, it obtained
some quirks and behaviours that are now considered too magical, or considered
to be a bug (which is sometimes actually frozen into spectests).  On top of
that, the development of the C<...> operator preceded the occurrence of the
C<Iterator> role, so it actually does not use any of its benefits.

This module started out as an attempt to make the C<...> operator (and friend)
much faster by re-implementing it using C<Iterator>s, rather than C<gather> /
C<take>.  However, it became clear that fixing behaviour considered too
magical or buggy, could break existing Raku code.  It was therefore decided to
turn this work into this module, with the option of incorporating it into a
later language version of Raku.

=head1 BREAKING CHANGES

=head2 Semantics of multiple endpoints

The original implementation of the C<...> operator allowed a chain of
endpoints to be specified:

   say 1 ... 5 ... 1; # (1 2 3 4 5 4 3 2 1)

This is no longer supported because of the unclear semantics of an endpoint
also serving as a starting point as soon as it is anything more than a numeric
value.  If you would like to have such a sequence, you will have to use
parentheses to indicate meaning:

   say 1 ... (5 ... 1); # (1 2 3 4 5 4 3 2 1)

=head2 Strict meaning of ^

The original implementation of the C<...^> operator treated omission of
endpoints differently for numeric values:

   say 1 ...^ 5.5; # (1 2 3 4 5)

This is generalized to B<always> omit the last B<generated> value, regardless
of whether it actually compared exactly with the endpoint or not.

=head1 AUTHOR

Elizabeth Mattijsen <liz@wenzperl.nl>

Source can be located at: https://github.com/lizmat/Sequence-Generator .
Comments and Pull Requests are welcome.

=head1 COPYRIGHT AND LICENSE

Copyright 2020 Elizabeth Mattijsen

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

=end pod

# vim: ft=perl6 expandtab sw=4
