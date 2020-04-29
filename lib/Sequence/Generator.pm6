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

    proto sub all-but-last(|) {*}
    multi sub all-but-last(PredictiveIterator:D \iterator) {
        AllButLastPredictive.new(iterator)
    }
    multi sub all-but-last(Iterator:D \iterator) {
        AllButLast.new(iterator)
    }

    # Return an iterator that takes a buffer and an iterator, which will
    # first produce the buffer, and then start producing values from the
    # iterator.
    my class BufferIterator does Iterator {
        has $!buffer;
        has $!iterator;

        method new(\buffer, \iterator) {
            my $new := nqp::create(self);
            nqp::bindattr($new,self,'$!buffer',buffer);
            nqp::bindattr($new,self,'$!iterator',iterator);
            $new
        }
        method pull-one() is raw {
            nqp::elems($!buffer)
              ?? nqp::shift($!buffer)
              !! $!iterator.pull-one
        }
        method is-lazy() { $!iterator.is-lazy }
    }

    # Return an iterator that combines two iterators
    my class TwoIterators does Iterator {
        has $!active;
        has $!spare;

        method new(\one, \two) {
            my $new := nqp::create(self);
            nqp::bindattr($new,self,'$!active',one);
            nqp::bindattr($new,self,'$!spare',two);
            $new
        }
        method pull-one() is raw {
            nqp::if(
              nqp::eqaddr((my \pulled := $!active.pull-one),IterationEnd),
              nqp::if(
                nqp::isnull($!spare),
                IterationEnd,
                nqp::stmts(            # switching to spare
                  ($!active := $!spare),
                  ($!spare  := nqp::null),
                  $!active.pull-one
                )
              ),
              pulled
            )
        }
    }

    # Role for iterators that need to handle Slips
    my role Slipper does Iterator {
        has $!slipping;  # the slip we're iterating now

        proto method start-slip(|) {*}
        multi method start-slip(Slip:U \slip) {
            slip
        }
        multi method start-slip(Slip:D \slip) {
            nqp::if(
              nqp::eqaddr(slip,Empty),
              self.pull-one,                 # nothing, so recurse
              nqp::if(
                nqp::eqaddr(
                  (my \result := ($!slipping := slip.iterator).pull-one),
                  IterationEnd
                ),
                nqp::stmts(                  # we've determined there's nothing
                  ($!slipping := nqp::null),
                  IterationEnd
                ),
                result                       # started a Slip
              )
            )
        }

        method slip-one() {
            my \result := $!slipping.pull-one;
            $!slipping := nqp::null if nqp::eqaddr(result,IterationEnd);
            result
        }
    }

#-- classes and helper subs for creating actual iterators ----------------------

    my class UnendingValue does Iterator {
        has Mu $!value;
        method new(Mu \value) {
            nqp::p6bindattrinvres(nqp::create(self),self,'$!value',value)
        }
        method pull-one() is raw { $!value }
        method skip-one(--> True) { }
        method sink-all(--> IterationEnd) { }
        method is-lazy(--> True) { }
    }

    # Unending iterator for constant numeric increment
    class UnendingStep does Iterator {
        has $!value;
        has $!step;
        method new(\first,\step --> Iterator:D) {
            my $new := nqp::create(self);
            nqp::bindattr($new,self,'$!value',first - step);
            nqp::bindattr($new,self,'$!step',step);
            $new
        }
        method pull-one() is raw { $!value := $!value + $!step }
        method is-lazy(--> True) { }
    }

    # Iterator for constant numeric negative increment to given numeric
    class StepDownto does PredictiveIterator {
        has $!value;
        has $!step;
        has $!downto;

        method new(\first, \step, \downto --> Iterator:D) {
            my $new := nqp::create(self);
            nqp::bindattr($new,self,'$!value',first - step);
            nqp::bindattr($new,self,'$!step',step);
            nqp::bindattr($new,self,'$!downto',downto);
            $new
        }
        method pull-one() is raw {
            ($!value := $!value + $!step) < $!downto
              ?? IterationEnd
              !! $!value
        }
        method count-only(--> Int:D) { (($!downto - $!value) / $!step).Int }
        method bool-only(--> Bool:D) { $!value + $!step >= $!downto }
    }

    # Iterator for constant numeric increment upto given numeric
    class StepUpto does PredictiveIterator {
        has $!value;
        has $!step;
        has $!upto;

        method new(\first, \step, \upto --> Iterator:D) {
            my $new := nqp::create(self);
            nqp::bindattr($new,self,'$!value',first - step);
            nqp::bindattr($new,self,'$!step',step);
            nqp::bindattr($new,self,'$!upto',upto);
            $new
        }
        method pull-one() is raw {
            ($!value := $!value + $!step) > $!upto
              ?? IterationEnd
              !! $!value
        }
        method count-only(--> Int:D) { (($!upto - $!value) / $!step).Int }
        method bool-only(--> Bool:D) { $!value + $!step <= $!upto }
    }

    # Iterator for stepping for a single codepoint
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
        method pull-one() is raw {
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
    # each string is then separated and the remaining list_s are equipped
    # with indices.  The result list_s is seeded with the first element
    # of each of the list_s.  Pulling a value will increment the pointer
    # for the last grapheme: if it is out of range, then the next list_s
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
            $no-last ?? all-but-last(self) !! self
        }
        method new(\first, \last, int $no-first, int $no-last) {
            nqp::create(self)!SET-SELF(first, last, $no-first, $no-last)
        }

        method pull-one() is raw {
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
            $no-last ?? all-but-last(self) !! self
        }
        method new(\first, \last, int $no-first, int $no-last) {
            nqp::create(self)!SET-SELF(first, last, $no-first, $no-last)
        }
        method pull-one() is raw { ... }
    }

    # Unending iterator calling .succ
    class UnendingSucc does Iterator {
        has $!value;
        method new(\first) {
            nqp::p6bindattrinvres(nqp::create(self),self,'$!value',first)
        }
        method pull-one() is raw {
            $!value := (my \this := $!value).succ;
            this
        }
        method is-lazy(--> True) { }
    }

    # Unending iterator calling .pred, stopping when exhausted
    class UnendingPred does Iterator {
        has $!value;
        method new(\first) {
            nqp::p6bindattrinvres(nqp::create(self),self,'$!value',first)
        }
        method pull-one() is raw {
            nqp::unless(
              nqp::eqaddr((my \this := $!value),IterationEnd),
              nqp::if(
                nqp::istype(($!value := this.pred),Failure),
                nqp::stmts(
                  $!value.Bool,  # mark Failure as handled
                  ($!value := IterationEnd)
                )
              )
            );
            this
        }
        method is-lazy(--> True) { }
    }

#-- iterators taking one or more lambda's --------------------------------------

    # Iterator calling a lambda without any parameters
    class LambdaNone does Slipper {
        has $!producer; # lambda to produce value

        method new(\seed, \producer, int $no-last) {
            my $new := nqp::create(self);
            nqp::bindattr($new,self,'$!slipping',seed.iterator);
            nqp::bindattr($new,self,'$!producer',producer);
            $no-last ?? all-but-last($new) !! $new
        }
        method pull-one() is raw { 
            my $result;
            nqp::if(
              nqp::isnull($!slipping),
              nqp::stmts(                       # not slipping
                nqp::handle(
                  ($result := $!producer()),
                  'LAST', (return IterationEnd)
                ),
                nqp::if(
                  nqp::istype($result,Slip),
                  self.start-slip($result),
                  $result
                )
              ),
              nqp::if(                          # slipping
                nqp::eqaddr(($result := self.slip-one),IterationEnd),
                self.pull-one, # recurse to handle potential Slip
                $result
              )
            )
        }
        method is-lazy(--> True) is raw { }
    }

    # Iterator calling a lambda without any parameters with endpoint lambda
    class LambdaNoneEnder does Slipper {
        has $!producer; # lambda to produce value
        has $!ender;    # lambda returning true if sequence should end

        method new(\seed, \producer, \ender) {
            my $new := nqp::create(self);
            nqp::bindattr($new,self,'$!slipping',seed.iterator);
            nqp::bindattr($new,self,'$!producer',producer);
            nqp::bindattr($new,self,'$!ender',ender);
            $new
        }
        method pull-one() is raw { 
            my $result;
            nqp::if(
              nqp::isnull($!slipping),
              nqp::stmts(                       # not slipping
                nqp::handle(
                  ($result := $!producer()),
                  'LAST', (return IterationEnd)
                ),
                nqp::if(
                  nqp::istype($result,Slip),
                  ($result := self.start-slip($result))
                )
              ),
              nqp::if(                          # slipping
                nqp::eqaddr(($result := self.slip-one),IterationEnd),
                (return self.pull-one) # recurse to handle potential Slip
              )
            );

            nqp::if(
              $!ender($result),
              IterationEnd,
              $result
            )
        }
    }

    # Iterator calling a lambda without any parameters with ACCEPTS endpoint
    class LambdaNoneAccepts does Slipper {
        has $!producer;    # lambda to produce value, null to mark ended
        has $!endpoint;    # value to call ACCEPTS on with produce value
        has int $!no-last; # flag to indicate skipping last produced value

        method new(\seed, \producer, \endpoint, int $no-last) {
            my $new := nqp::create(self);
            nqp::bindattr($new,self,'$!slipping',seed.iterator);
            nqp::bindattr($new,self,'$!producer',producer);
            nqp::bindattr($new,self,'$!endpoint',endpoint);
            nqp::bindattr_i($new,self,'$!no-last',$no-last);
            $new
        }
        method pull-one() is raw { 
            my $result;
            nqp::if(
              nqp::isnull($!slipping),
              nqp::stmts(                       # not slipping
                nqp::handle(
                  ($result := nqp::ifnull($!producer,(return IterationEnd))()),
                  'LAST', (return IterationEnd)
                ),
                nqp::if(
                  nqp::istype($result,Slip),
                  ($result := self.start-slip($result))
                )
              ),
              nqp::if(                          # slipping
                nqp::eqaddr(($result := self.slip-one),IterationEnd),
                (return self.pull-one) # recurse to handle potential Slip
              )
            );

            nqp::if(
              $!endpoint.ACCEPTS($result),
              nqp::if(
                $!no-last,
                IterationEnd,  # don't bother to produce last value
                nqp::stmts(
                  ($!producer := nqp::null),
                  $result
                )
              ),
              $result
            )
        }
    }

    # Iterator calling a lambda with 1 parameter
    class Lambda1 does Slipper {
        has $!value;    # value to be passed to lambda to produce next value
        has $!producer; # lambda to produce value

        method new(\seed, \producer, int $no-last) {
            my $new := nqp::create(self);
            nqp::bindattr($new,self,'$!slipping',seed.iterator);
            nqp::bindattr($new,self,'$!producer',producer);
            $no-last ?? all-but-last($new) !! $new
        }
        method pull-one() is raw { 
            my $result;
            $!value := nqp::if(
              nqp::isnull($!slipping),
              nqp::stmts(                       # not slipping
                nqp::handle(
                  ($result := $!producer($!value)),
                  'LAST', (return IterationEnd)
                ),
                nqp::if(
                  nqp::istype($result,Slip),
                  self.start-slip($result),
                  $result
                )
              ),
              nqp::if(                          # slipping
                nqp::eqaddr(($result := self.slip-one),IterationEnd),
                (return self.pull-one), # recurse to handle potential Slip
                $result
              )
            )
        }
        method is-lazy(--> True) is raw { }
    }

    # Iterator calling a lambda with 1 parameter with endpoint lambda
    class Lambda1Ender does Slipper {
        has $!value;    # value to be passed to lambda to produce next value
        has $!producer; # lambda to produce value
        has $!ender;    # lambda returning true if sequence should end
        has int $!no-last; # flag to indicate skipping last produced value

        method new(\seed, \producer, \ender, int $no-last) {
            my $new := nqp::create(self);
            nqp::bindattr($new,self,'$!slipping',seed.iterator);
            nqp::bindattr($new,self,'$!producer',producer);
            nqp::bindattr($new,self,'$!ender',ender);
            nqp::bindattr_i($new,self,'$!no-last',$no-last);
            $new
        }
        method pull-one() is raw { 
            my $result;
            nqp::if(
              nqp::isnull($!slipping),
              nqp::stmts(                       # not slipping
                nqp::handle(
                  ($result := nqp::ifnull(
                    $!producer,(return IterationEnd)
                  )($!value)),
                  'LAST', (return IterationEnd)
                ),
                nqp::if(
                  nqp::istype($result,Slip),
                  ($result := self.start-slip($result))
                )
              ),
              nqp::if(                          # slipping
                nqp::eqaddr(($result := self.slip-one),IterationEnd),
                (return self.pull-one) # recurse to handle potential Slip
              )
            );

            nqp::if(
              $!ender($result),
              nqp::if(
                $!no-last,
                IterationEnd,  # don't bother to produce last value
                nqp::stmts(
                  ($!producer := nqp::null),
                  $result
                )
              ),
              ($!value := $result)
            )
        }
    }

    # Iterator calling a lambda with 1 parameter with ACCEPTS endpoint
    class Lambda1Accepts does Slipper {
        has $!value;       # value to be passed to lambda to produce next value
        has $!producer;    # lambda to produce value
        has $!endpoint;    # value to call ACCEPTS on with produce value
        has int $!no-last; # flag to indicate skipping last produced value

        method new(\seed, \producer, \endpoint, int $no-last) {
            my $new := nqp::create(self);
            nqp::bindattr($new,self,'$!slipping',seed.iterator);
            nqp::bindattr($new,self,'$!producer',producer);
            nqp::bindattr($new,self,'$!endpoint',endpoint);
            nqp::bindattr_i($new,self,'$!no-last',$no-last);
            $new
        }
        method pull-one() is raw { 
            my $result;
            nqp::if(
              nqp::isnull($!slipping),
              nqp::stmts(                       # not slipping
                nqp::handle(
                  ($result := nqp::ifnull(
                    $!producer,(return IterationEnd)
                  )($!value)),
                  'LAST', (return IterationEnd)
                ),
                nqp::if(
                  nqp::istype($result,Slip),
                  ($result := self.start-slip($result))
                )
              ),
              nqp::if(                          # slipping
                nqp::eqaddr(($result := self.slip-one),IterationEnd),
                (return self.pull-one) # recurse to handle potential Slip
              )
            );

            nqp::if(
              $!endpoint.ACCEPTS($result),
              nqp::if(
                $!no-last,
                IterationEnd,  # don't bother to produce last value
                nqp::stmts(
                  ($!producer := nqp::null),
                  $result
                )
              ),
              ($!value := $result)
            )
        }
    }

    # Iterator calling a lambda with 2 parameters
    class Lambda2 does Slipper {
        has $!value1;   # first value to be passed to produce next value
        has $!value2;   # second value to be passed to produce next value
        has $!producer; # lambda to produce value

        method new(\seed, \producer, int $no-last) {
            my $new := nqp::create(self);
            nqp::bindattr($new,self,'$!slipping',seed.iterator);
            nqp::bindattr($new,self,'$!producer',producer);
            $no-last ?? all-but-last($new) !! $new
        }
        method pull-one() is raw { 
            my $result;
            nqp::if(
              nqp::isnull($!slipping),
              nqp::stmts(                       # not slipping
                nqp::handle(
                  ($result := $!producer($!value1,$!value2)),
                  'LAST', (return IterationEnd)
                ),
                nqp::if(
                  nqp::istype($result,Slip),
                  ($result := self.start-slip($result))
                )
              ),
              nqp::if(                          # slipping
                nqp::eqaddr(($result := self.slip-one),IterationEnd),
                (return self.pull-one) # recurse to handle potential Slip
              )
            );

            $!value1 := $!value2;
            $!value2 := $result;
        }
        method is-lazy(--> True) is raw { }
    }

    # Iterator calling a lambda with 2 parameters with endpoint lambda
    class Lambda2Ender does Slipper {
        has $!value1;   # first value to be passed to produce next value
        has $!value2;   # second value to be passed to produce next value
        has $!producer; # lambda to produce value
        has $!ender;    # lambda returning true if sequence should end

        method new(\seed, \producer, \ender) {
            my $new := nqp::create(self);
            nqp::bindattr($new,self,'$!slipping',seed.iterator);
            nqp::bindattr($new,self,'$!producer',producer);
            nqp::bindattr($new,self,'$!ender',ender);
            $new
        }
        method pull-one() is raw { 
            my $result;
            nqp::if(
              nqp::isnull($!slipping),
              nqp::stmts(                       # not slipping
                nqp::handle(
                  ($result := $!producer($!value1,$!value2)),
                  'LAST', (return IterationEnd)
                ),
                nqp::if(
                  nqp::istype($result,Slip),
                  ($result := self.start-slip($result))
                )
              ),
              nqp::if(                          # slipping
                nqp::eqaddr(($result := self.slip-one),IterationEnd),
                (return self.pull-one) # recurse to handle potential Slip
              )
            );

            nqp::if(
              $!ender($result),
              IterationEnd,
              nqp::stmts(
                ($!value1 := $!value2),
                ($!value2 := $result)
              )
            )
        }
    }

    # Iterator calling a lambda with 2 parameters with ACCEPTS endpoint
    class Lambda2Accepts does Slipper {
        has $!value1;      # first value to be passed to produce next value
        has $!value2;      # second value to be passed to produce next value
        has $!producer;    # lambda to produce value
        has $!endpoint;    # value to call ACCEPTS on with produce value
        has int $!no-last; # flag to indicate skipping last produced value

        method new(\seed, \producer, \endpoint, int $no-last) {
            my $new := nqp::create(self);
            nqp::bindattr($new,self,'$!slipping',seed.iterator);
            nqp::bindattr($new,self,'$!producer',producer);
            nqp::bindattr($new,self,'$!endpoint',endpoint);
            nqp::bindattr_i($new,self,'$!no-last',$no-last);
            $new
        }
        method pull-one() is raw { 
            my $result;
            nqp::if(
              nqp::isnull($!slipping),
              nqp::stmts(                       # not slipping
                nqp::handle(
                  ($result := nqp::ifnull(
                    $!producer,(return IterationEnd)
                  )($!value1,$!value2)),
                  'LAST', (return IterationEnd)
                ),
                nqp::if(
                  nqp::istype($result,Slip),
                  ($result := self.start-slip($result))
                )
              ),
              nqp::if(                          # slipping
                nqp::eqaddr(($result := self.slip-one),IterationEnd),
                (return self.pull-one) # recurse to handle potential Slip
              )
            );

            nqp::if(
              $!endpoint.ACCEPTS($result),
              nqp::if(
                $!no-last,
                IterationEnd,  # don't bother to produce last value
                nqp::stmts(
                  ($!producer := nqp::null),
                  $result
                )
              ),
              nqp::stmts(
                ($!value1 := $!value2),
                ($!value2 := $result)
              )
            )
        }
    }

    # ensure buffer containes right number of values to be passed
    sub set-buffer-size(\buffer, int $elems) {
        nqp::while(
          nqp::isgt_i(nqp::elems(buffer),$elems),
          nqp::shift(buffer)
        );
        nqp::while(
          nqp::islt_i(nqp::elems(buffer),$elems),
          nqp::unshift(buffer,Any)
        );
        buffer
    }

    # Iterator calling a lambda with last N values
    class LambdaN does Slipper {
        has $!producer;  # lambda to produce value
        has $!values;    # IterationBuffer with values to be passed to producer
        has $!list;      # HLL wrapper around $!values

        method new(\seed, \producer, int $no-last, int $elems) {
            my $new := nqp::create(self);
            nqp::bindattr($new,self,'$!slipping',seed.iterator);
            nqp::bindattr($new,self,'$!producer',producer);
            nqp::bindattr($new,self,'$!list',
              (my \values := nqp::bindattr(
                $new,self,'$!values',set-buffer-size(nqp::clone(seed),$elems)
              )).List
            );
            $no-last ?? all-but-last($new) !! $new
        }
        method pull-one() is raw {
            my $result;
            nqp::if(
              nqp::isnull($!slipping),
              nqp::stmts(                    # not slipping
                nqp::handle(
                  ($result := $!producer(|$!list)),
                  'LAST', (return IterationEnd)
                ),
                nqp::if(
                  nqp::istype($result,Slip),
                  ($result := self.start-slip($result))
                )
              ),
              nqp::if(                       # slipping
                nqp::eqaddr(($result := self.slip-one),IterationEnd),
                (return self.pull-one)  # recurse to handle potential Slip
              )
            );

            nqp::shift($!values);
            nqp::push($!values,$result)
        }
        method is-lazy(--> True) is raw { }
    }

    # Iterator calling a lambda with last N values with endpoint lambda
    class LambdaNEnder does Slipper {
        has $!producer;  # lambda to produce value
        has $!ender;     # lambda returning true if sequence should end
        has $!values;    # IterationBuffer with values to be passed to producer
        has $!list;      # HLL wrapper around $!values

        method new(\seed, \producer, \ender, int $elems) {
            my $new := nqp::create(self);
            nqp::bindattr($new,self,'$!slipping',seed.iterator);
            nqp::bindattr($new,self,'$!producer',producer);
            nqp::bindattr($new,self,'$!ender',ender);
            nqp::bindattr($new,self,'$!list',
              (my \values := nqp::bindattr(
                $new,self,'$!values',set-buffer-size(nqp::clone(seed),$elems)
              )).List
            );
            $new
        }
        method pull-one() is raw {
            my $result;
            nqp::if(
              nqp::isnull($!slipping),
              nqp::stmts(                    # not slipping
                nqp::handle(
                  ($result := $!producer(|$!list)),
                  'LAST', (return IterationEnd)
                ),
                nqp::if(
                  nqp::istype($result,Slip),
                  ($result := self.start-slip($result))
                )
              ),
              nqp::if(                       # slipping
                nqp::eqaddr(($result := self.slip-one),IterationEnd),
                (return self.pull-one)  # recurse to handle potential Slip
              )
            );

            nqp::if(
              $!ender($result),
              IterationEnd,
              nqp::stmts(
                nqp::shift($!values),
                nqp::push($!values,$result)
              )
            )
        }
    }

    # Iterator calling a lambda with last N values with ACCEPTS endpoint
    class LambdaNAccepts does Slipper {
        has $!producer;    # lambda to produce value
        has $!endpoint;    # value to call ACCEPTS on with produce value
        has int $!no-last; # flag to indicate skipping last produced value
        has $!values;      # IterationBuffer with values to pass to producer
        has $!list;        # HLL wrapper around $!values

        method new(\seed, \producer, \endpoint, int $no-last, int $elems) {
            my $new := nqp::create(self);
            nqp::bindattr($new,self,'$!slipping',seed.iterator);
            nqp::bindattr($new,self,'$!producer',producer);
            nqp::bindattr($new,self,'$!endpoint',endpoint);
            nqp::bindattr_i($new,self,'$!no-last',$no-last);
            nqp::bindattr($new,self,'$!list',
              (my \values := nqp::bindattr(
                $new,self,'$!values',set-buffer-size(nqp::clone(seed),$elems)
              )).List
            );
            $new
        }
        method pull-one() is raw {
            my $result;
            nqp::if(
              nqp::isnull($!slipping),
              nqp::stmts(                    # not slipping
                nqp::handle(
                  ($result := nqp::ifnull(
                    $!producer,(return IterationEnd)
                  )(|$!list)),
                  'LAST', (return IterationEnd)
                ),
                nqp::if(
                  nqp::istype($result,Slip),
                  ($result := self.start-slip($result))
                )
              ),
              nqp::if(                       # slipping
                nqp::eqaddr(($result := self.slip-one),IterationEnd),
                (return self.pull-one)  # recurse to handle potential Slip
              )
            );

            nqp::if(
              $!endpoint.ACCEPTS($result),
              nqp::if(
                $!no-last,
                IterationEnd,  # don't bother to produce last value
                nqp::stmts(
                  ($!producer := nqp::null),
                  $result
                )
              ),
              nqp::stmts(
                nqp::shift($!values),
                nqp::push($!values,$result)
              )
            )
        }
    }

    # Iterator calling a lambda with *all* values
    class LambdaAll does Slipper {
        has $!producer;  # lambda to produce value
        has $!values;    # IterationBuffer with values to be passed to producer
        has $!list;      # HLL wrapper around $!values

        method new(\seed, \producer, int $no-last) {
            my $new := nqp::create(self);
            nqp::bindattr($new,self,'$!slipping',seed.iterator);
            nqp::bindattr($new,self,'$!producer',producer);
            nqp::bindattr($new,self,'$!list',nqp::bindattr(
              $new,self,'$!values',nqp::create(IterationBuffer)
            ).List);
            $no-last ?? all-but-last($new) !! $new
        }
        method pull-one() is raw {
            my $result;
            nqp::if(
              nqp::isnull($!slipping),
              nqp::push(                       # not slipping
                $!values,
                nqp::stmts(
                  nqp::handle(
                    ($result := $!producer(|$!list)),
                    'LAST', (return IterationEnd)
                  ),
                  nqp::if(
                    nqp::istype($result,Slip),
                    self.start-slip($result),
                    $result
                  )
                )
              ),
              nqp::if(                         # slipping
                nqp::eqaddr(($result := self.slip-one),IterationEnd),
                self.pull-one,  # recurse to handle potential Slip
                nqp::push($!values,$result)
              )
            )
        }
        method is-lazy(--> True) is raw { }
    }

    # Iterator calling a lambda with *all* values with endpoint lambda
    class LambdaAllEnder does Slipper {
        has $!producer;  # lambda to produce value
        has $!ender;     # lambda returning true if sequence should end
        has $!values;    # IterationBuffer with values to be passed to producer
        has $!list;      # HLL wrapper around $!values

        method new(\seed, \producer, \ender) {
            my $new := nqp::create(self);
            nqp::bindattr($new,self,'$!slipping',seed.iterator);
            nqp::bindattr($new,self,'$!producer',producer);
            nqp::bindattr($new,self,'$!ender',ender);
            nqp::bindattr($new,self,'$!list',nqp::bindattr(
              $new,self,'$!values',nqp::create(IterationBuffer)
            ).List);
            $new
        }
        method pull-one() is raw {
            my $result;
            nqp::if(
              nqp::isnull($!slipping),
              nqp::stmts(                    # not slipping
                nqp::handle(
                  ($result := $!producer(|$!list)),
                  'LAST', (return IterationEnd)
                ),
                nqp::if(
                  nqp::istype($result,Slip),
                  ($result := self.start-slip($result))
                )
              ),
              nqp::if(                       # slipping
                nqp::eqaddr(($result := self.slip-one),IterationEnd),
                (return self.pull-one)  # recurse to handle potential Slip
              )
            );

            nqp::if(
              $!ender($result),
              IterationEnd,
              nqp::push($!values,$result)
            )
        }
    }

    # Iterator calling a lambda with *all* values with ACCEPTS endpoint
    class LambdaAllAccepts does Slipper {
        has $!producer;    # lambda to produce value
        has $!endpoint;    # value to call ACCEPTS on with produce value
        has int $!no-last; # flag to indicate skipping last produced value
        has $!values;      # IterationBuffer with values to pass to producer
        has $!list;        # HLL wrapper around $!values

        method new(\seed, \producer, \endpoint, int $no-last) {
            my $new := nqp::create(self);
            nqp::bindattr($new,self,'$!slipping',seed.iterator);
            nqp::bindattr($new,self,'$!producer',producer);
            nqp::bindattr($new,self,'$!endpoint',endpoint);
            nqp::bindattr_i($new,self,'$!no-last',$no-last);
            nqp::bindattr($new,self,'$!list',nqp::bindattr(
              $new,self,'$!values',nqp::create(IterationBuffer)
            ).List);
            $new
        }
        method pull-one() is raw {
            my $result;
            nqp::if(
              nqp::isnull($!slipping),
              nqp::stmts(                    # not slipping
                nqp::handle(
                  ($result := nqp::ifnull(
                    $!producer,(return IterationEnd)
                  )(|$!list)),
                  'LAST', (return IterationEnd)
                ),
                nqp::if(
                  nqp::istype($result,Slip),
                  ($result := self.start-slip($result))
                )
              ),
              nqp::if(                       # slipping
                nqp::eqaddr(($result := self.slip-one),IterationEnd),
                (return self.pull-one)  # recurse to handle potential Slip
              )
            );

            nqp::if(
              $!endpoint.ACCEPTS($result),
              nqp::if(
                $!no-last,
                IterationEnd,  # don't bother to produce last value
                nqp::stmts(
                  ($!producer := nqp::null),
                  $result
                )
              ),
              nqp::push($!values,$result)
            )
        }
    }

#-- the actual iterator dispatch -----------------------------------------------
    proto method iterator(|) {*}

    # Return iterator for 2 numeric endpoints
    multi method iterator(
      Real:D \first, Real:D \endpoint, Int:D $no-first, Int:D $no-last
    --> Iterator:D) {
        my \iterator := endpoint < first
          ?? endpoint == -Inf
            ?? UnendingStep.new(first - $no-first, -1)
            !! StepDownto.new(first - $no-first, -1, endpoint)
          !! endpoint == Inf
            ?? UnendingStep.new(first + $no-first, +1)
            !! StepUpto.new(first + $no-first, +1, endpoint);

        $no-last ?? all-but-last(iterator) !! iterator
    }

    # Return iterator for numeric ... Whatever
    multi method iterator(
      Real:D \first, Whatever, Int:D $no-first, Int:D $
    --> Iterator:D) {
        UnendingStep.new(first + $no-first, 1)
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

    # Return iterator for anything unending
    multi method iterator(
      Any:D $first, Whatever, Int:D $no-first, Int:D $
    --> Iterator:D) {
        UnendingSucc.new($no-first ?? $first.succ !! $first)
    }

    # Return iterator for anything unending
    multi method iterator(
      Any:D $first, Real:D $endpoint, Int:D $no-first, Int:D $no-last
    --> Iterator:D) {
        $endpoint == Inf
          ?? UnendingSucc.new($no-first ?? $first.succ !! $first)
          !! $endpoint == -Inf
            ?? $no-last
              ?? all-but-last(
                   UnendingPred.new($no-first ?? $first.pred !! $first)
                 )
              !! UnendingPred.new($no-first ?? $first.pred !! $first)
            !! die
    }

    # helper methods for lambdas
    method !lambda-none(\initials, \lambda, \endpoint, int $no-last) {
        nqp::istype(endpoint,Whatever) || endpoint === Inf
          ?? LambdaNone.new(initials, lambda, $no-last)
          !! LambdaNoneAccepts.new(initials, lambda, endpoint, $no-last)
    }
    method !lambda1(\initials, \lambda, \endpoint, int $no-last) {
        nqp::istype(endpoint,Whatever) || endpoint === Inf
          ?? Lambda1.new(initials, lambda, $no-last)
          !! Lambda1Accepts.new(initials, lambda, endpoint, $no-last)
    }
    method !lambda2(\initials, \lambda, \endpoint, int $no-last) {
        nqp::istype(endpoint,Whatever) || endpoint === Inf
          ?? Lambda2.new(initials, lambda, $no-last)
          !! Lambda2Accepts.new(initials, lambda, endpoint, $no-last)
    }
    method !lambda-n(\initials, \lambda, \endpoint, int $no-last, int $elems) {
        nqp::istype(endpoint,Whatever) || endpoint === Inf
          ?? LambdaN.new(initials, lambda, $no-last, $elems)
          !! LambdaNAccepts.new(initials, lambda, endpoint, $no-last, $elems)
    }
    method !lambda-all(\initials, \lambda, \endpoint, int $no-last) {
        nqp::istype(endpoint,Whatever) || endpoint === Inf
          ?? LambdaAll.new(initials, lambda, $no-last)
          !! LambdaAllAccepts.new(initials, lambda, endpoint, $no-last)
    }

    # Return iterator for given initial values with endpoint
    method !iterator-endpoint(
      $source, Mu \endpoint, int $no-first, int $no-last
    --> Iterator:D) {
        my $initials := nqp::create(IterationBuffer);
        my $iterator := nqp::null;

        # find the right iterator
        nqp::until(
          nqp::eqaddr((my \pulled := $source.pull-one),IterationEnd),
          nqp::if(
            nqp::isnull($iterator),
            nqp::if(
              nqp::istype(pulled,Callable),
              ($iterator := nqp::if(
                (my $arg-count := pulled.arity || pulled.count),
                nqp::if(
                  $arg-count == 1,
                  self!lambda1($initials, pulled,
                    nqp::decont(endpoint), $no-last),
                  nqp::if(
                    $arg-count == 2,
                    self!lambda2($initials, pulled,
                      nqp::decont(endpoint), $no-last),
                    nqp::if(
                      $arg-count == Inf,
                      self!lambda-all($initials, pulled,
                        nqp::decont(endpoint), $no-last),
                      self!lambda-n($initials, pulled,
                        nqp::decont(endpoint), $no-last, $arg-count)
                    )
                  )
                ),
                self!lambda-none($initials, pulled,
                  nqp::decont(endpoint), $no-last)
              )),
              nqp::push($initials,pulled)
            ),
            die "Cannot have value after Callable: found {pulled}"
          )
        );

        # no iterator yet, use initial values
        $iterator := self!elucidate($initials, endpoint, $no-last)
          if nqp::isnull($iterator);

        $iterator.skip-one if $no-first;
        $iterator
    }

    # Return iterator for iterators with initial and endpoint values
    method !iterator-iterator(
      Iterator:D \left, Iterator:D \right, int $no-first, int $no-last
    --> Iterator:D) {
        nqp::eqaddr((my \endpoint := right.pull-one),IterationEnd)
          ?? endpoint-mismatch(left, "empty list")
          !! nqp::istype(endpoint,Whatever) || endpoint === Inf
            ?? die()  # can never produce later
            !! TwoIterators.new(
                 self!iterator-endpoint(left, endpoint, $no-first, $no-last),
                 right
               )
    }

    # Return iterator for given initial values with endpoint
    multi method iterator(
      @source, Mu \endpoint, Int:D $no-first, Int:D $no-last
    --> Iterator:D) is default {
        nqp::istype(endpoint,Iterable)
          ?? self!iterator-iterator(
               @source.iterator, endpoint.iterator, $no-first, $no-last
             )
          !! self!iterator-endpoint(
               @source.iterator, endpoint, $no-first, $no-last
             )
    }

    method !elucidate2(
      IterationBuffer:D \seed, \endpoint, int $no-last
    --> Iterator:D) {
        my \one := nqp::atpos(seed,0);
        my \two := nqp::atpos(seed,1);

        if nqp::eqaddr(one.WHAT,two.WHAT) {
            if nqp::istype(one,Real) {
                (my \step := two - one)
                  ?? nqp::istype(endpoint,Whatever) || endpoint === Inf
                    ?? UnendingStep.new(one, step)
                    !! nqp::istype(endpoint,Real)
                      ?? step-to(one, step, endpoint, $no-last)
                      !! endpoint-mismatch(one, endpoint)
                  !! UnendingValue.new(one)
            }
            elsif one.succ === two {
                nqp::istype(endpoint,Whatever) || endpoint === Inf
                  ?? UnendingSucc.new(one)
                  !! nqp::istype(endpoint,one.WHAT)
                    ?? Lambda1Accepts.new(seed, *.succ, endpoint, $no-last)
                    !! endpoint-mismatch(one, endpoint)
            }
            elsif two.succ === one {
                nqp::istype(endpoint,Whatever) || endpoint === Inf
                  ?? UnendingPred.new(one)
                  !! nqp::istype(endpoint,one.WHAT)
                    ?? Lambda1Accepts.new(seed, {
                         nqp::istype((my \value := .pred),Failure)
                           ?? (last)
                           !! value
                       }, endpoint, $no-last)
                    !! endpoint-mismatch(one, endpoint)
            }
            elsif one === two {
                UnendingValue.new(one)
            }
            elsif nqp::istype(endpoint,Whatever) || endpoint === Inf {
                BufferIterator.new(seed, UnendingSucc.new(two.succ))
            }
            else {
                not-deducable(one,two)
            }
        }
        else {
            not-deducable(one,two)
        }
    }

    method !elucidateN(
      IterationBuffer:D \seed, \endpoint, int $no-last
    --> Iterator:D) {
        my int $elems = nqp::elems(seed);
        my \one   := nqp::atpos(seed,$elems - 3);
        my \two   := nqp::atpos(seed,$elems - 2);
        my \three := nqp::atpos(seed,$elems - 1);
        my $step;

        # all can be considered numerical
        if nqp::istype(one.WHAT,Real)
          && nqp::istype(two.WHAT,Real)
          && nqp::istype(three.WHAT,Real) {
            $step := two - one;

            # arithmetic sequence
            if three - two == $step {
                nqp::istype(endpoint,Whatever) || endpoint === Inf
                  ?? TwoIterators.new(
                       seed.iterator, UnendingStep.new(three + $step, $step))
                  !! nqp::istype(endpoint,Real)
                    ?? Lambda1Accepts.new(seed, * + $step,
                         $step > 0 ?? * >= endpoint !! * <= endpoint, $no-last)
                    !! nqp::istype(endpoint,Callable)
                      ?? Lambda1Accepts.new(seed, * + $step, endpoint, $no-last)
                      !! endpoint-mismatch(seed, endpoint)
            }

            # something to check still
            elsif one || two || three {
                my $mult := (two / one).narrow;
                three / two == $mult
                  ?? nqp::istype(endpoint,Whatever) || endpoint === Inf
                    ?? Lambda1.new(seed, * * $mult, $no-last)
                    !! nqp::istype(endpoint,Real)
                         || nqp::istype(endpoint,Callable)
                      ?? Lambda1Accepts.new(seed,* * $mult,endpoint,$no-last)
                      !! endpoint-mismatch(seed, endpoint)
                  !! nqp::istype(endpoint,Whatever) || endpoint === Inf
                    ?? TwoIterators.new(
                         seed.iterator, UnendingStep.new(three + 1, 1))
                    !! nqp::istype(endpoint,Real)
                         || nqp::istype(endpoint,Callable)
                      ?? Lambda1Accepts.new(seed, * + 1, endpoint, $no-last)
                      !! endpoint-mismatch(seed, endpoint)
            }
            else {
                not-deducable(one,two,three)
            }
        }

        # all the same type
        elsif nqp::eqaddr(one.WHAT,two.WHAT)
          && nqp::eqaddr(two.WHAT,three.WHAT) {

            # string always .succ or other classes that don't add up
            if nqp::istype(one,Str)
              || try { $step := two - one } === Nil {
                BufferIterator.new(seed, UnendingSucc.new(three.succ))
            }

            # classes that can add up
            else {
                three - two === $step
                  ?? BufferIterator.new(
                       seed, UnendingStep.new(three + $step, $step))
                  !! not-deducable(one,two,three);
            }
        }
        else {
            not-deducable(one,two,three);
        }
    }

    # take seed / endpoint / and turn it into an iterator
    method !elucidate(IterationBuffer:D \seed, \endpoint, int $no-last) {
        nqp::iseq_i(nqp::elems(seed),2)
          ?? self!elucidate2(seed, endpoint, $no-last)
          !! nqp::isgt_i(nqp::elems(seed),2)
            ?? self!elucidateN(seed, endpoint, $no-last)
            !! nqp::elems(seed)
              ?? self.iterator(nqp::shift(seed), endpoint, 0, $no-last)
              !! die
    }

#-- helper subs ----------------------------------------------------------------

    sub step-to(\value, \step, \endpoint, int $no-last --> Iterator:D) {
        step > 0
          ?? StepUpto.new(  value, step,
               $no-last ?? endpoint - step !! endpoint)
          !! StepDownto.new(value, step,
               $no-last ?? endpoint - step !! endpoint)
    }

    # ender version for unending sequences
    sub no-end($ --> 0) { }

    # make quitting easy
    sub not-deducable(*@values) is hidden-from-backtrace {
        X::Sequence::Deduction.new(from => @values>>.raku.join(",")).throw
    }
    sub endpoint-mismatch(\from,\endpoint) is hidden-from-backtrace {
        X::Sequence::Endpoint.new(from => from, endpoint => endpoint).throw
    }
}

#-- operator front-end ---------------------------------------------------------

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

my constant &infix:<…> is export := &infix:<<...>>;
my constant &infix:<…^> is export := &infix:<<...^>>;
my constant &infix:<^…> is export := &infix:<<^...>>;
my constant &infix:<^…^> is export := &infix:<<^...^>>;

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

This module started out as an attempt to make the C<...> operator (and friends)
much faster by re-implementing it using C<Iterator>s, rather than C<gather> /
C<take>.  However, it became clear that fixing behaviour considered too
magical or buggy, could break existing Raku code.  It was therefore decided to
turn this work into this module, with the option of incorporating it into a
later language version of Raku.

This module is between 4x and 20x faster than the Raku 6.d implementation.

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

=head2 Using .pred ends sequence if Failure

The original implementation of the C<...> operator would die if C<.pred> was
being used to generate the next value, and that would return a Failure.  This
has been changed to ending the sequence.

=head2 No longer silently ignores values on LHS after Callable

The original implementation of the C<...> would ignore any values B<after>
a Callable on the LHS, e.g.:

   1,2,3, * + 1, 7,8,9 ... 100;

This now dies.

=head2 No longer silently ignores values with RHS list starting with *

The original implementation of the C<...> would ignore any values B<after>
a Whatever as the first element of a list on the RHS, e.g.:

   1,2,3 ... *,42,666

This now dies.

=head1 AUTHOR

Elizabeth Mattijsen <liz@wenzperl.nl>

Source can be located at: https://github.com/lizmat/Sequence-Generator .
Comments and Pull Requests are welcome.

=head1 COPYRIGHT AND LICENSE

Copyright 2020 Elizabeth Mattijsen

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

=end pod

# vim: ft=perl6 expandtab sw=4
