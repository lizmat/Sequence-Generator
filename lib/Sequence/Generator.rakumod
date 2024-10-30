# This module is intended to be part of the Rakudo core in the
# foreseeable future.

use nqp;
use trace;

class Sequence::Generator {

#- copy of Rakudo::Iterator ----------------------------------------------------

    # Create iterator that produces all values *except* the last of a given
    # iterator.  Returns an empty iterator if the given iterator did not
    # produce any value.
    my role AllButLastRole {
        has $!iterator;
        has $!value;

        method new($iterator is raw --> Iterator:D) {
            if nqp::eqaddr((my $value := $iterator.pull-one),IterationEnd) {
                ().iterator
            }
            else {
                my $self := nqp::create(self);
                nqp::bindattr($self,$?CLASS,'$!iterator',$iterator);
                nqp::bindattr($self,$?CLASS,'$!value',   $value);
                $self
            }
        }
        method pull-one() is raw {
            my $this := $!value;
            nqp::eqaddr(($!value := $!iterator.pull-one),IterationEnd)
              ?? IterationEnd
              !! $this
        }
    }
    my class AllButLast does Iterator does AllButLastRole { }
    my class AllButLastPredictive does PredictiveIterator does AllButLastRole {
        method count-only() { ($!iterator.count-only || 1) - 1 }
        method bool-only()  {  $!iterator.count-only > 1       }
    }

    # Return an iterator that combines two iterators
    my class TwoIterators does Iterator {
        has $!active;
        has $!spare;

        method new($one is raw, $two is raw --> Iterator:D) {
            my $self := nqp::create(self);
            nqp::bindattr($self,TwoIterators,'$!active',$one);
            nqp::bindattr($self,TwoIterators,'$!spare', $two);
            $self
        }
        method pull-one() is raw {
            my $pulled := $!active.pull-one;
            if nqp::eqaddr($pulled,IterationEnd) {
                if nqp::isnull($!spare) {
                    IterationEnd
                }
                else {            # switching to spare
                    $!active := $!spare;
                    $!spare  := nqp::null;
                    $!active.pull-one
                }
            }
            else {
                $pulled
            }
        }
    }

    # Role for iterators that need to handle Slips
    my role Slipper does Iterator {
        has $!slipping;  # the slip we are iterating now

        proto method start-slip(|) {*}
        multi method start-slip(Slip:U $slip) {
            $slip
        }
        multi method start-slip(Slip:D $slip is raw) {
            nqp::if(
              nqp::eqaddr($slip,Empty),
              self.pull-one,                 # nothing, so recurse
              nqp::if(
                nqp::eqaddr(
                  (my $result := ($!slipping := $slip.iterator).pull-one),
                  IterationEnd
                ),
                nqp::stmts(                  # determined there is nothing
                  ($!slipping := nqp::null),
                  IterationEnd
                ),
                $result                       # started a Slip
              )
            )
        }

        method slip-one() {
            my $result := $!slipping.pull-one;
            $!slipping  := nqp::null if nqp::eqaddr($result,IterationEnd);
            $result
        }
    }

#-- classes for creating actual iterators --------------------------------------

    my class UnendingValue does Iterator {
        has Mu $!value;
        method new(Mu $value is raw --> Iterator:D) {
            nqp::p6bindattrinvres(
              nqp::create(self),UnendingValue,'$!value',$value
            )
        }
        method pull-one() is raw { $!value }
        method skip-one(--> True) { }
        method sink-all(--> IterationEnd) { }
        method is-lazy(--> True) { }
    }

    # Unending iterator for constant numeric increment/decrement
    class UnendingStep does Iterator {
        has $!value;
        has $!step;
        method new($first is raw, $step is raw --> Iterator:D) {
            my $self := nqp::create(self);
            nqp::bindattr($self,UnendingStep,'$!value',$first - $step);
            nqp::bindattr($self,UnendingStep,'$!step', $step);
            $self
        }
        method pull-one() is raw { $!value := $!value + $!step }
        method is-lazy(--> True) { }
    }

    # Iterator for constant numeric negative increment to given numeric
    class StepDownto does PredictiveIterator {
        has $!value;
        has $!step;
        has $!downto;

        method new($first is raw, $step is raw, $downto is raw --> Iterator:D) {
            my $self := nqp::create(self);
            nqp::bindattr($self,StepDownto,'$!value', $first - $step);
            nqp::bindattr($self,StepDownto,'$!step',  $step);
            nqp::bindattr($self,StepDownto,'$!downto',$downto);
            $self
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

        method new($first is raw, $step is raw, $upto is raw --> Iterator:D) {
            my $self := nqp::create(self);
            nqp::bindattr($self,StepUpto,'$!value',$first - $step);
            nqp::bindattr($self,StepUpto,'$!step', $step);
            nqp::bindattr($self,StepUpto,'$!upto', $upto);
            $self
        }
        method pull-one() is raw {
            ($!value := $!value + $!step) > $!upto
              ?? IterationEnd
              !! $!value
        }
        method count-only(--> Int:D) { (($!upto - $!value) / $!step).Int }
        method bool-only(--> Bool:D) { $!value + $!step <= $!upto }
        method is-monotonically-increasing(--> True) { }
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
        method new(
          $first is raw, $last is raw, int $no-first, int $no-last
        --> Iterator:D) {
            nqp::create(self)!SET-SELF(
              nqp::ord($first), nqp::ord($last), $no-first, $no-last
            )
        }
        method pull-one() is raw {
            --$!todo
              ?? nqp::chr($!codepoint = nqp::add_i($!codepoint,$!step))
              !! IterationEnd
        }
        method push-all($target is raw --> IterationEnd) {
            my int $todo = $!todo;
            my int $cp   = $!codepoint;
            my int $step = $!step;

            nqp::if(
              $todo,
              nqp::while(
                --$todo,
                $target.push(nqp::chr($cp = nqp::add_i($cp,$step)))
              )
            );
            $!todo = $todo;  # make sure count/bool-only are correct
        }
        method count-only() { $!todo }
        method bool-only() { nqp::hllbool($!todo) }
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
        method new($first is raw, $last is raw, int $no-first, int $no-last) {
            nqp::create(self)!SET-SELF($first, $last, $no-first, $no-last)
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
            $!value := (my $this := $!value).succ;
            $this
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
              nqp::eqaddr((my $this := $!value),IterationEnd),
              nqp::if(
                nqp::istype(($!value := $this.pred),Failure),
                nqp::stmts(
                  $!value.Bool,  # mark Failure as handled
                  ($!value := IterationEnd)
                )
              )
            );
            $this
        }
        method is-lazy(--> True) { }
    }

#-- iterators taking one or more lambdas ---------------------------------------

    # Iterator calling a lambda without any parameters
    class LambdaNone does Slipper {
        has $!producer; # lambda to produce value

        method new($seed, $producer, int $no-last) {
            my $self := nqp::create(self);
            nqp::bindattr($self,LambdaNone,'$!slipping',$seed.iterator);
            nqp::bindattr($self,LambdaNone,'$!producer',$producer);
            $no-last ?? all-but-last($self) !! $self
        }
        method pull-one() is raw {
            my $result;

            if nqp::isnull($!slipping) {
                nqp::handle(                # not slipping
                  ($result := $!producer()),
                  'LAST', (return IterationEnd)
                );
                nqp::istype($result,Slip)
                  ?? self.start-slip($result)
                  !! $result
            }
            else {                          # slipping
                nqp::eqaddr(($result := self.slip-one),IterationEnd)
                  ?? self.pull-one          # recurse to handle potential Slip
                  !! $result
            }
        }
        method is-lazy(--> True) is raw { }
    }

    # Iterator calling a lambda without any parameters with ACCEPTS endpoint
    class LambdaNoneAccepts does Slipper {
        has $!producer;    # lambda to produce value, null to mark ended
        has $!endpoint;    # value to call ACCEPTS on with produce value
        has int $!no-last; # flag to indicate skipping last produced value

        method new($seed, $producer, $endpoint, int $no-last) {
            my $self := nqp::create(self);
            nqp::bindattr($self,LambdaNoneAccepts,'$!slipping',$seed.iterator);
            nqp::bindattr($self,LambdaNoneAccepts,'$!producer',$producer);
            nqp::bindattr($self,LambdaNoneAccepts,'$!endpoint',$endpoint);
            nqp::bindattr_i($self,LambdaNoneAccepts,'$!no-last', $no-last);
            $self
        }
        method pull-one() is raw {
            my $result;

            # not slipping
            if nqp::isnull($!slipping) {
                nqp::handle(
                  ($result := nqp::ifnull($!producer,(return IterationEnd))()),
                  'LAST', (return IterationEnd)
                );
                $result := self.start-slip($result)
                  if nqp::istype($result,Slip);

                if $!endpoint.ACCEPTS($result) {
                    $!no-last  # do not bother to produce last value?
                      ?? ($result    := IterationEnd)
                      !! ($!producer := nqp::null)
                }
            }

            # slipping
            elsif nqp::eqaddr(($result := self.slip-one),IterationEnd) {
                self.pull-one  # recurse to handle potential Slip
            }

            $result
        }
    }

    # Iterator calling a lambda with 1 parameter
    class Lambda1 does Slipper {
        has $!value;    # value to be passed to lambda to produce next value
        has $!producer; # lambda to produce value

        method new($seed, $producer, int $no-last) {
            my $self := nqp::create(self);
            nqp::bindattr($self,Lambda1,'$!slipping',$seed.iterator);
            nqp::bindattr($self,Lambda1,'$!producer',$producer);
            $no-last ?? all-but-last($self) !! $self
        }

        method pull-one() is raw {
            my $result;

            $!value := do if nqp::isnull($!slipping) {
                # not slipping
                nqp::handle(
                  ($result := $!producer($!value)),
                  'LAST', (return IterationEnd)
                );

                $result := nqp::istype($result,Slip)
                  ?? self.start-slip($result)
                  !! $result
            }
            else {
                # slipping
                nqp::eqaddr(($result := self.slip-one),IterationEnd)
                  ?? (return self.pull-one)  # recurse to handle potential Slip
                  !! $result
            }
        }
        method is-lazy(--> True) is raw { }
    }

    # Iterator calling a lambda with 1 parameter with ACCEPTS endpoint
    class Lambda1Accepts does Slipper {
        has $!value;       # value to be passed to lambda to produce next value
        has $!producer;    # lambda to produce value
        has $!endpoint;    # value to call ACCEPTS on with produce value
        has int $!no-last; # flag to indicate skipping last produced value

        method new($seed, $producer, $endpoint, int $no-last) {
            my $self := nqp::create(self);
            nqp::bindattr($self,Lambda1Accepts,'$!slipping',$seed.iterator);
            nqp::bindattr($self,Lambda1Accepts,'$!producer',$producer);
            nqp::bindattr($self,Lambda1Accepts,'$!endpoint',$endpoint);
            nqp::bindattr_i($self,Lambda1Accepts,'$!no-last',$no-last);
            $self
        }
        method pull-one() is raw {
            my $result;

            if nqp::isnull($!slipping) {
                nqp::handle(
                  ($result := nqp::ifnull(
                    $!producer,(return IterationEnd)
                  )($!value)),
                  'LAST', (return IterationEnd)
                );
                $result := self.start-slip($result)
                  if nqp::istype($result,Slip);
            }
            elsif nqp::eqaddr(($result := self.slip-one),IterationEnd) {
                return self.pull-one;  # recurse to handle potential Slip
            }

            if $!endpoint.ACCEPTS($result) {
                if $!no-last {
                    IterationEnd       # do not bother to produce last value
                }
                else {                 # after this we're done
                    $!producer := nqp::null;
                    $result
                }
            }
            else {
                $!value := $result
            }
        }
    }

    # Iterator calling a lambda with 2 parameters
    class Lambda2 does Slipper {
        has $!value1;   # first value to be passed to produce next value
        has $!value2;   # second value to be passed to produce next value
        has $!producer; # lambda to produce value

        method new($seed, $producer, int $no-last) {
            my $self := nqp::create(self);
            nqp::bindattr($self,Lambda2,'$!slipping',$seed.iterator);
            nqp::bindattr($self,Lambda2,'$!producer',$producer);
            $no-last ?? all-but-last($self) !! $self
        }

        method pull-one() is raw {
            my $result;

            # not slipping
            if nqp::isnull($!slipping) {
                nqp::handle(
                  ($result := $!producer($!value1,$!value2)),
                  'LAST', (return IterationEnd)
                );
                $result := self.start-slip($result)
                  if nqp::istype($result,Slip);
            }

            # slipping
            elsif nqp::eqaddr(($result := self.slip-one),IterationEnd) {
                return self.pull-one;   # recurse to handle potential Slip
            }

            $!value1 := $!value2;
            $!value2 := $result;
        }
        method is-lazy(--> True) is raw { }
    }

    # Iterator calling a lambda with 2 parameters with ACCEPTS endpoint
    class Lambda2Accepts does Slipper {
        has $!value1;      # first value to be passed to produce next value
        has $!value2;      # second value to be passed to produce next value
        has $!producer;    # lambda to produce value
        has $!endpoint;    # value to call ACCEPTS on with produce value
        has int $!no-last; # flag to indicate skipping last produced value

        method new($seed, $producer, $endpoint, int $no-last) {
            my $self := nqp::create(self);
            nqp::bindattr($self,Lambda2Accepts,'$!slipping',$seed.iterator);
            nqp::bindattr($self,Lambda2Accepts,'$!producer',$producer);
            nqp::bindattr($self,Lambda2Accepts,'$!endpoint',$endpoint);
            nqp::bindattr_i($self,Lambda2Accepts,'$!no-last',$no-last);
            $self
        }

        method pull-one() is raw {
            my $result;

            # not slipping
            if nqp::isnull($!slipping) {
                nqp::handle(
                  ($result := nqp::ifnull(
                    $!producer,(return IterationEnd)
                  )($!value1,$!value2)),
                  'LAST', (return IterationEnd)
                );
                $result := self.start-slip($result)
                  if nqp::istype($result,Slip);
            }

            # slipping
            elsif nqp::eqaddr(($result := self.slip-one),IterationEnd) {
                return self.pull-one;  # recurse to handle potential Slip
            }

            if $!endpoint.ACCEPTS($result) {
                if $!no-last {
                    IterationEnd       # do not bother to produce last value
                }
                else {                 # after this we're done
                    $!producer := nqp::null;
                    $result
                }
            }
            else {
                $!value1 := $!value2;
                $!value2 := $result
            }
        }
    }

    # Iterator calling a lambda with last N values
    class LambdaN does Slipper {
        has $!producer;  # lambda to produce value
        has $!values;    # IterationBuffer with values to be passed to producer
        has $!list;      # HLL wrapper around $!values

        method new($seed, $producer, int $no-last, int $elems) {
            my $self := nqp::create(self);
            nqp::bindattr($self,LambdaN,'$!slipping',$seed.iterator);
            nqp::bindattr($self,LambdaN,'$!producer',$producer);
            nqp::bindattr($self,LambdaN,'$!list',
              nqp::bindattr($self,LambdaN,'$!values',
                set-buffer-size(nqp::clone($seed),$elems)
              ).List
            );
            $no-last ?? all-but-last($self) !! $self
        }

        method pull-one() is raw {
            my $result;

            # not slipping
            if nqp::isnull($!slipping) {
                nqp::handle(
                  ($result := $!producer(|$!list)),
                  'LAST', (return IterationEnd)
                );
                $result := self.start-slip($result)
                  if nqp::istype($result,Slip);
            }

            # slipping
            elsif nqp::eqaddr(($result := self.slip-one),IterationEnd) {
                return self.pull-one;  # recurse to handle potential Slip
            }

            nqp::shift($!values);
            nqp::push($!values,$result)
        }
        method is-lazy(--> True) is raw { }
    }

    # Iterator calling a lambda with last N values with ACCEPTS endpoint
    class LambdaNAccepts does Slipper {
        has $!producer;    # lambda to produce value
        has $!endpoint;    # value to call ACCEPTS on with produce value
        has int $!no-last; # flag to indicate skipping last produced value
        has $!values;      # IterationBuffer with values to pass to producer
        has $!list;        # HLL wrapper around $!values

        method new($seed, $producer, $endpoint, int $no-last, int $elems) {
            my $self := nqp::create(self);
            nqp::bindattr($self,LambdaNAccepts,'$!slipping',$seed.iterator);
            nqp::bindattr($self,LambdaNAccepts,'$!producer',$producer);
            nqp::bindattr($self,LambdaNAccepts,'$!endpoint',$endpoint);
            nqp::bindattr_i($self,LambdaNAccepts,'$!no-last',$no-last);
            nqp::bindattr($self,LambdaNAccepts,'$!list',
              nqp::bindattr($self,LambdaNAccepts,'$!values',
                set-buffer-size(nqp::clone($seed),$elems)
              ).List
            );
            $self
        }

        method pull-one() is raw {
            my $result;

            # not slipping
            if nqp::isnull($!slipping) {
                nqp::handle(
                  ($result := nqp::ifnull(
                    $!producer,(return IterationEnd)
                  )(|$!list)),
                  'LAST', (return IterationEnd)
                );
                $result := self.start-slip($result)
                  if nqp::istype($result,Slip);
            }

            # slipping
            elsif nqp::eqaddr(($result := self.slip-one),IterationEnd) {
                return self.pull-one;  # recurse to handle potential Slip
            }

            if $!endpoint.ACCEPTS($result) {
                if $!no-last {
                    IterationEnd       # do not bother to produce last value
                }
                else {                 # after this we're done
                    $!producer := nqp::null;
                    $result
                }
            }
            else {
                nqp::shift($!values);
                nqp::push($!values,$result)
            }
        }
    }

    # Iterator calling a lambda with *all* values
    class LambdaAll does Slipper {
        has $!producer;  # lambda to produce value
        has $!values;    # IterationBuffer with values to be passed to producer
        has $!list;      # HLL wrapper around $!values

        method new($seed, $producer, int $no-last) {
            my $self := nqp::create(self);
            nqp::bindattr($self,LambdaAll,'$!slipping',$seed.iterator);
            nqp::bindattr($self,LambdaAll,'$!producer',$producer);
            nqp::bindattr($self,LambdaAll,'$!list',
              nqp::bindattr(
                $self,LambdaAll,'$!values',nqp::create(IterationBuffer)
              ).List
            );
            $no-last ?? all-but-last($self) !! $self
        }

        method pull-one() is raw {
            my $result;

            # not slipping
            if nqp::isnull($!slipping) {
                nqp::push(
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
                );
            }

            # slipping
            else {
                nqp::eqaddr(($result := self.slip-one),IterationEnd)
                  ?? self.pull-one  # recurse to handle potential Slip
                  !! nqp::push($!values,$result)
            }
        }
        method is-lazy(--> True) is raw { }
    }

    # Iterator calling a lambda with *all* values with ACCEPTS endpoint
    class LambdaAllAccepts does Slipper {
        has $!producer;    # lambda to produce value
        has $!endpoint;    # value to call ACCEPTS on with produce value
        has int $!no-last; # flag to indicate skipping last produced value
        has $!values;      # IterationBuffer with values to pass to producer
        has $!list;        # HLL wrapper around $!values

        method new($seed, $producer, $endpoint, int $no-last) {
            my $self := nqp::create(self);
            nqp::bindattr($self,LambdaAllAccepts,'$!slipping',$seed.iterator);
            nqp::bindattr($self,LambdaAllAccepts,'$!producer',$producer);
            nqp::bindattr($self,LambdaAllAccepts,'$!endpoint',$endpoint);
            nqp::bindattr_i($self,LambdaAllAccepts,'$!no-last',$no-last);
            nqp::bindattr($self,LambdaAllAccepts,'$!list',
              nqp::bindattr(
                $self,LambdaAllAccepts,'$!values',nqp::create(IterationBuffer)
              ).List
            );
            $self
        }

        method pull-one() is raw {
            my $result;

            # not slipping
            if nqp::isnull($!slipping) {
                nqp::handle(
                  ($result := nqp::ifnull(
                    $!producer,(return IterationEnd)
                  )(|$!list)),
                  'LAST', (return IterationEnd)
                );
                $result := self.start-slip($result)
                  if nqp::istype($result,Slip);
            }

            # slipping
            elsif nqp::eqaddr(($result := self.slip-one),IterationEnd) {
                return self.pull-one;  # recurse to handle potential Slip
            }

            if $!endpoint.ACCEPTS($result) {
                if $!no-last {
                    IterationEnd       # do not bother to produce last value
                }
                else {                 # after this we're done
                    $!producer := nqp::null;
                    $result
                }
            }
            else {
                nqp::push($!values,$result)
            }
        }
    }

#-- the actual iterator dispatch -----------------------------------------------
    proto method iterator(|) {*}

    # Return iterator for 2 numeric endpoints
    multi method iterator(
      Real:D $first, Real:D $endpoint, Int:D $no-first, Int:D $no-last
    --> Iterator:D) {
        my $iterator := $endpoint < $first
          ?? $endpoint == -Inf
            ?? (return UnendingStep.new($first - $no-first, -1))
            !! StepDownto.new($first - $no-first, -1, $endpoint)
          !! $endpoint == Inf
            ?? (return UnendingStep.new($first + $no-first, +1))
            !! StepUpto.new($first + $no-first, +1, $endpoint);

        $no-last ?? all-but-last($iterator) !! $iterator
    }

    # Return iterator for two string endpoints
    multi method iterator(
      Str:D $first, Str:D $last, Int:D $no-first, Int:D $no-last
    --> Iterator:D) {
        $first.chars == $last.chars
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

    # Return iterator for anything with a numeric endpoint
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
    method !lambda-none($initials, $lambda, $endpoint, int $no-last) {
        $endpoint == Inf
          ?? LambdaNone.new($initials, $lambda, $no-last)
          !! LambdaNoneAccepts.new($initials, $lambda, $endpoint, $no-last)
    }
    method !lambda1($initials, $lambda, $endpoint, int $no-last) {
        $endpoint == Inf
          ?? Lambda1.new($initials, $lambda, $no-last)
          !! Lambda1Accepts.new($initials, $lambda, $endpoint, $no-last)
    }
    method !lambda2($initials, $lambda, $endpoint, int $no-last) {
        $endpoint == Inf
          ?? Lambda2.new($initials, $lambda, $no-last)
          !! Lambda2Accepts.new($initials, $lambda, $endpoint, $no-last)
    }
    method !lambda-n($initials, $lambda, $endpoint, int $no-last, int $elems) {
        $endpoint == Inf
          ?? LambdaN.new($initials, $lambda, $no-last, $elems)
          !! LambdaNAccepts.new($initials, $lambda, $endpoint, $no-last, $elems)
    }
    method !lambda-all($initials, $lambda, $endpoint, int $no-last) {
        $endpoint == Inf
          ?? LambdaAll.new($initials, $lambda, $no-last)
          !! LambdaAllAccepts.new($initials, $lambda, $endpoint, $no-last)
    }

    # Return iterator for given initial values with endpoint
    method !iterator-endpoint(
      $source, Mu $endpoint, int $no-first, int $no-last
    --> Iterator:D) {
        my $initials := nqp::create(IterationBuffer);
        my $iterator := nqp::null;

        # find the right iterator
        nqp::until(
          nqp::eqaddr((my $pulled := $source.pull-one),IterationEnd),
          nqp::if(
            nqp::isnull($iterator),
            nqp::if(
              nqp::istype($pulled,Code),
              ($iterator := nqp::if(
                (my $arg-count := $pulled.arity || $pulled.count),
                nqp::if(
                  $arg-count == 1,
                  self!lambda1($initials, $pulled,
                    nqp::decont($endpoint), $no-last),
                  nqp::if(
                    $arg-count == 2,
                    self!lambda2($initials, $pulled,
                      nqp::decont($endpoint), $no-last),
                    nqp::if(
                      $arg-count == Inf,
                      self!lambda-all($initials, $pulled,
                        nqp::decont($endpoint), $no-last),
                      self!lambda-n($initials, $pulled,
                        nqp::decont($endpoint), $no-last, $arg-count)
                    )
                  )
                ),
                self!lambda-none($initials, $pulled,
                  nqp::decont($endpoint), $no-last)
              )),
              nqp::push($initials,$pulled)
            ),
            die "Cannot have value after Callable: found $pulled"
          )
        );

        # no iterator yet, use initial values
        $iterator := self!elucidate($initials, $endpoint, $no-last)
          if nqp::isnull($iterator);

        $iterator.skip-one if $no-first;
        $iterator
    }

    # Return iterator for iterators with initial and endpoint values
    method !iterator-iterator(
      Iterator:D $left, Iterator:D $right, int $no-first, int $no-last
    --> Iterator:D) {
        nqp::eqaddr((my $endpoint := $right.pull-one),IterationEnd)
          ?? endpoint-mismatch($left, "empty list")
          !! $endpoint == Inf
            ?? die()  # can never produce later
            !! TwoIterators.new(
                 self!iterator-endpoint($left, $endpoint, $no-first, $no-last),
                 $right
               )
    }

    # Return iterator for given initial values with endpoint
    multi method iterator(
      @source, Mu $endpoint, Int:D $no-first, Int:D $no-last
    --> Iterator:D) is default {
        nqp::istype($endpoint,Iterable)
          ?? self!iterator-iterator(
               @source.iterator, $endpoint.iterator, $no-first, $no-last
             )
          !! self!iterator-endpoint(
               @source.iterator, $endpoint, $no-first, $no-last
             )
    }

    method !elucidate2(
      IterationBuffer:D $seed is raw, $endpoint is raw, int $no-last
    --> Iterator:D) {
        my $one := nqp::atpos($seed,0);
        my $two := nqp::atpos($seed,1);

        nqp::eqaddr($one.WHAT,$two.WHAT)
          ?? nqp::istype($one,Real)

            # numeric sequence
            ?? (my $step := $two - $one)
              ?? $endpoint == Inf
                ?? UnendingStep.new($one, $step)
                !! nqp::istype($endpoint,Real)
                  ?? step-to($one, $step, $endpoint, $no-last)
                  !! endpoint-mismatch($one, $endpoint)
              !! not-deducible($one,$two)  # no direction


            # short-cut not deducible
            !! $one === $two
              ?? not-deducible($one,$two)

              # potential .succ sequence
              !! $one.succ === $two

                # .succ sequence
                ?? $endpoint == Inf
                  ?? UnendingSucc.new($one)
                  !! nqp::istype($endpoint,$one.WHAT)
                    ?? Lambda1Accepts.new($seed, *.succ, $endpoint, $no-last)
                    !! endpoint-mismatch($one, $endpoint)

                # potential .pred sequence
                !! $two.succ === $one

                  # .pred sequence
                  ?? $endpoint === -Inf
                    ?? UnendingPred.new($one)
                    !! nqp::istype($endpoint, $one.WHAT)
                      ?? Lambda1Accepts.new($seed, {
                           nqp::istype((my $value := .pred),Failure)
                             ?? (last)
                             !! $value
                         }, $endpoint, $no-last)
                      !! endpoint-mismatch($one, $endpoint)

          # alas, no go
                  !! not-deducible($one, $two)
          !! not-deducible($one, $two)
    }

    method !elucidateN(
      IterationBuffer:D $seed, $endpoint, int $no-last
    --> Iterator:D) {
        my int $elems = nqp::elems($seed);
        my $one   := nqp::atpos($seed,$elems - 3);
        my $two   := nqp::atpos($seed,$elems - 2);
        my $three := nqp::atpos($seed,$elems - 1);

        nqp::istype($one.WHAT,Real)
          && nqp::istype($two.WHAT,Real)
          && nqp::istype($three.WHAT,Real)

          # all numerics
          ?? (my $step := $two - $one) == $three - $two

            # arithmetic sequence
            ?? $endpoint == Inf
              ?? TwoIterators.new($seed.iterator,
                   UnendingStep.new($three + $step, $step))
              !! nqp::istype($endpoint,Real)
                ?? Lambda1Accepts.new($seed, * + $step,
                     step-endpoint($step, $endpoint), $no-last)
                !! nqp::istype($endpoint,Code)
                  ?? Lambda1Accepts.new($seed, * + $step, $endpoint, $no-last)
                  !! endpoint-mismatch($seed, $endpoint)

            # numbers, but not an arithmetic sequence
            !! $one && $two && $three
                 && (my $mult := ($two / $one).narrow) > 0
                 && $three / $two == $mult

              # geometric sequence
              ?? $endpoint == Inf
                ?? Lambda1.new($seed, * * $mult, $no-last)
                !! nqp::istype($endpoint,Real)
                  ?? Lambda1Accepts.new($seed, * * $mult,
                       mult-endpoint($mult, $endpoint), $no-last)
                  !! nqp::istype($endpoint,Code)
                    ?? Lambda1Accepts.new($seed, * * $mult, $endpoint, $no-last)
                    !! endpoint-mismatch($seed, $endpoint)
              !! not-deducible($one, $two, $three, "here")

          # all same type, potential .succ
          !! nqp::eqaddr($one.WHAT,$two.WHAT)
               && nqp::eqaddr($two.WHAT,$three.WHAT)
            ?? $one.succ === $two && $two.succ === $three

              # simple .succ
              ?? $endpoint == Inf
                ?? TwoIterators.new(
                     $seed.iterator,
                     UnendingSucc.new($three.succ)
                   )
                !! nqp::istype($three.WHAT,$endpoint.WHAT)
                     || nqp::istype($endpoint,Code)
                  ?? Lambda1Accepts.new($seed, *.succ, $endpoint, $no-last)
                  !! endpoint-mismatch($three, $endpoint)

              # not a simple .succ
              !! $two.succ === $one && $three.succ === $two

                # simple .pred
                ?? $endpoint === -Inf
                  ?? Lambda1.new($seed, *.pred, $no-last)
                  !! nqp::istype($three.WHAT,$endpoint.WHAT)
                       || nqp::istype($endpoint,Code)
                    ?? Lambda1Accepts.new($seed, *.succ, $endpoint, $no-last)
                    !! endpoint-mismatch($three, $endpoint)

            # alas, no go
                !! not-deducible($one, $two, $three)
            !! not-deducible($one, $two, $three)
    }

    # take seed / endpoint / and turn it into an iterator
    method !elucidate(IterationBuffer:D $seed, $endpoint, int $no-last) {
        my int $elems = nqp::elems($seed);
        $elems == 2
          ?? self!elucidate2($seed, $endpoint, $no-last)
          !! $elems > 2
            ?? self!elucidateN($seed, $endpoint, $no-last)
            !! $elems
              ?? self.iterator(nqp::shift($seed), $endpoint, 0, $no-last)
              !! die
    }

#-- helper subs ----------------------------------------------------------------

    # helper sub to get correct "all but last" iterator
    proto sub all-but-last(|) {*}
    multi sub all-but-last(
      PredictiveIterator:D $iterator
    --> PredictiveIterator:D) {
        AllButLastPredictive.new($iterator)
    }
    multi sub all-but-last(Iterator:D $iterator --> Iterator:D) {
        AllButLast.new($iterator)
    }

    # helper sub to make a nqp::list_s for 2 codepoints
    sub cps2list_s(int $from, int $to) {
        my int $step = $from < $to ?? 1 !! -1;
        my int $this = $from - $step;

        my $chars := nqp::list_s;
        nqp::until(
          nqp::iseq_i(($this = nqp::add_i($this,$step)),$to),
          nqp::push_s($chars,nqp::chr($this))
        );
        nqp::push_s($chars,nqp::chr($this));

        $chars
    }

    # ensure buffer containes right number of values to be passed
    sub set-buffer-size(
      IterationBuffer:D $buffer, int $elems
    --> IterationBuffer:D) {
        nqp::while(
          nqp::isgt_i(nqp::elems($buffer),$elems),
          nqp::shift($buffer)
        );
        nqp::while(
          nqp::islt_i(nqp::elems($buffer),$elems),
          nqp::unshift($buffer,Any)
        );
        $buffer
    }

    # helper sub to return correct stepper ender
    sub step-endpoint($step, $endpoint) {
        $step > 0 ?? * >= $endpoint !! * <= $endpoint
    }

    # helper sub to return correct multiplication ender
    sub mult-endpoint($mult, $endpoint) {
        $mult > 1 ?? * >= $endpoint !! * <= $endpoint
    }

    # helper sub to return correct stepper iterator
    sub step-to($value, $step, $end, int $no-last --> Iterator:D) {
        $step > 0
          ?? StepUpto.new(  $value, $step, $no-last ?? $end - $step !! $end)
          !! StepDownto.new($value, $step, $no-last ?? $end - $step !! $end)
    }

    # make quitting easy
    sub not-deducible(*@values) is hidden-from-backtrace {
        X::Sequence::Deduction.new(from => @values>>.raku.join(",")).throw
    }
    sub endpoint-mismatch(\from,\endpoint) is hidden-from-backtrace {
        X::Sequence::Endpoint.new(from => from, endpoint => endpoint).throw
    }
}

#-- operator front-end ---------------------------------------------------------

my proto sub infix:<...>(|) is export is equiv(&infix:<...>) {*}
my multi sub infix:<...>(Mu $a, Whatever) { $a ... Inf }
my multi sub infix:<...>(Mu $a, Mu $b) {
    Seq.new: Sequence::Generator.iterator($a, $b, 0, 0)
}

my proto sub infix:<...^>(|) is export is equiv(&infix:<...>) {*}
my multi sub infix:<...^>(Mu $a, Whatever) { $a ...^ Inf }
my multi sub infix:<...^>(Mu $a, Mu $b) {
    Seq.new: Sequence::Generator.iterator($a, $b, 0, 1)
}

my proto sub infix:<^...>(|) is export is equiv(&infix:<...>) {*}
my multi sub infix:<^...>(Mu $a, Whatever) { $a ^... Inf }
my multi sub infix:<^...>(Mu $a, Mu $b) {
    Seq.new: Sequence::Generator.iterator($a, $b, 1, 0)
}

my proto sub infix:<^...^>(|) is export is equiv(&infix:<...>) {*}
my multi sub infix:<^...^>(Mu $a, Whatever) { $a ^...^ Inf }
my multi sub infix:<^...^>(Mu $a, Mu $b) {
    Seq.new: Sequence::Generator.iterator($a, $b, 1, 1)
}

my constant &infix:<…>   is export := &infix:<<...>>;
my constant &infix:<…^>  is export := &infix:<<...^>>;
my constant &infix:<^…>  is export := &infix:<<^...>>;
my constant &infix:<^…^> is export := &infix:<<^...^>>;

#dd -> {slip 'zero','one'} ... *;

# # vim: expandtab shiftwidth=4