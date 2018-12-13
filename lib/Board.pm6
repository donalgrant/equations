use v6;

use Algorithm::Combinatorics:from<Perl5> qw<tuples combinations permutations>;

use lib '/Users/imel/gitdev/donalgrant/equations/lib';
use Globals;
use RPN;

class Board {

  has BagHash $.U is required;   # Unused cubes
  has BagHash $.R;   # Required cubes
  has BagHash $.P;   # Permitted cubes
  has BagHash $.F;   # Forbidden cubes
  
  has Str $.G='';        # goal (string of cubes)
  has Numeric %.S{Str};  # solutions (keys are rpn strings, values are numeric for RPN)

  multi method new( BagHash $U ) { note "new from BagHash"; self.bless(:$U) }
#  multi method new( Seq $cubes ) { note "new from Seq"; my $U=BagHash.new($cubes);      Board.new(:$U) }
  multi method new( Str $cubes ) { note "new from Str"; my $U=BagHash.new($cubes.comb); Board.new(:$U) }
#  multi method new( List $cubes) { note "new from List"; my $U=BagHash.new($cubes);     Board.new(:$U) }
  submethod TWEAK() { self.clear_solutions; $!R=BagHash.new; $!P=BagHash.new; $!F=BagHash.new }
  
  method clear_solutions { %!S=(); self }

  method save_solution (RPN $rpn) {
    my $v=+$rpn;
    return False unless $v.defined;
    return False unless self.goal==$v;  # we've set a goal, and this ain't it
    %!S{~$rpn}=$v;
    return True;
  }
  
  method required  { $!R.kxxv }
  method permitted { $!P.kxxv }
  method forbidden { $!F.kxxv }
  method unused    { $!U.kxxv }

  method goal      { +$!G // Nil }

  method available  { ($!R (+) $!P (+) $!U).kxxv }
  
  method display {
    my $div='_' x 40;
    my $out=qq:to/END/;
      $div                            
      Unused:     { self.unused.sort }
      Required:   { self.required.sort }
      Permitted:  { self.permitted.sort }
      Forbidden:  { self.forbidden.sort }
      Goal:       { self.goal }
      $div
    END
    return $out;
  }
  
  method debug {
    note "Keys in Unused";
    for $!U.keys -> $k { note "Key >>{$k}<< has {$!U{$k}} entries" }
  }

=begin pod
  method !move(Str $cube, BagHash $from_bag is rw, BagHash $to_bag is rw) {
    quit "$cube not available to move" unless $from_bag{$cube}>0;
    $from_bag{$cube}--;
    $to_bag{$cube}++;
    self;
  }
  method move_to_required(Str $cube)  { self!move($cube,$!U,$!R) }
  method move_to_permitted(Str $cube) { self!move($cube,$!U,$!P) }
  method move_to_forbidden(Str $cube) { self!move($cube,$!U,$!F) }
=end pod

  method move_to_required(Str $cube)  { quit "no $cube available" unless $!U{$cube}>0; $!U{$cube}--; $!R{$cube}++ }
  method move_to_permitted(Str $cube) { quit "no $cube available" unless $!U{$cube}>0; $!U{$cube}--; $!P{$cube}++  }
  method move_to_forbidden(Str $cube) { quit "no $cube available" unless $!U{$cube}>0; $!U{$cube}--; $!F{$cube}++  }

  method move_to_goal(Str $cubes) { 
    for $cubes.comb -> $g { die "$g not available for goal" unless $!U{$g}>0; $!U{$g}-- }
    $!G=$cubes;
    self;
  }

  method install_goal($goal) { $!G=$goal; self }

  method solution_list {
# note "solution_list going in S={%!S.pairs}";
    my $defined_rpn_keys = %!S.keys.grep( { %!S{$_}.defined } );
    return () unless $defined_rpn_keys.elems > 0;
    return $defined_rpn_keys.map( { RPN.new($_) } );
  }

  method req_tuples(@c,&r) {
    my Bag $bag=Bag.new( $!R.list.grep(&r) );
    return @c.map( *.join('') ).grep( Bag.new(*.comb) (>=) $bag ).map( *.comb );
  }
  method req_num_tuples(@c) { return self.req_tuples(@c,&digit) }
  method req_ops_tuples(@c) { return self.req_tuples(@c,&op)    }

  method goal_options( $max_digits=3 ) {
    my $digit_bag=Bag.new( [|self.available] ).pairs.grep(/<digit>/).Bag;
    my @goal_options=$digit_bag.pairs.grep( *.value==1 ).map( *.key );  # singletons
    for 2..$max_digits -> $k {
      last if $k>$digit_bag.total;
      my @p=tuples( $digit_bag.kxxv, $k );
      @goal_options.push( |@p.unique(:as( *.join('') )).map( *.join('') ).grep( none /^0\d+/ ) );
    }
    return @goal_options;
  }

  method calculate_solutions($ncubes) {  # ncubes is maximum number of cubes to use
    die "Goal must be set before calculating solutions" unless $!G.chars > 0;
    my $bag=self.available.Bag;
note "Calculate solutions for $ncubes cubes from set of {$bag.kxxv} cubes";
    my $num=Bag.new( $bag.kxxv.grep(/<digit>/) ); 
# note "Number   cubes available: {$num.kxxv}";
    my $ops=Bag.new( $bag.kxxv.grep(/ <op>  /) );
# note "Operator cubes available: {$ops.kxxv}";
    my $nops=(floor($ncubes/2) < $ops.total) ?? floor($ncubes/2) !! $ops.total;  # the largest number of ops cubes possible
    my $nnum=( ($nops+1)     > $num.total) ?? $num.total     !! ($nops+1);   # the largest number of num cubes possible
                                                                             #  (based on ops too)
    $nops = $nnum-1;                                                         # no more ops cubes than num cubes - 1
# note "calculate_solutions:  nops=$nops; nnum=$nnum";
    return self unless $nops>=1 && $nnum>=2;
# note "passed first return check";
    my @pn=get_tuples $nnum, $num, Bag.new( $!R.kxxv.grep(/<digit>/) );
    my @po=get_tuples $nops, $ops, Bag.new( $!R.kxxv.grep(/ <op>  /) );
    my @ops_slots=ops_slots($nops);
note "pn=[{@pn.map({ $_.join(',') }).join('],[')}]";
note "po=[{@po.map({ $_.join(',') }).join('],[')}]";
note "ops_slots={@ops_slots.join(', ')}";
    my $max_solutions=200000;
    my $n_solutions= @pn * @po * @ops_slots;    # numeric context -- product of array sizes
note "{+@pn} x {+@po} x {+@ops_slots} RPNs=$n_solutions solutions";
    return self unless $n_solutions;
    if ($n_solutions>$max_solutions) {
note "...and that's too many by a factor of {$n_solutions/$max_solutions}; reducing";
      my $reduce_factor=min( 4.0, ($n_solutions/$max_solutions)**(1.0/3.0) ); 
note "reduce_factor=$reduce_factor"; 
      my $nsl=max( 3, (+@ops_slots/$reduce_factor).floor ); 
note "nsl=$nsl";
      my $max_tuples=sqrt($max_solutions/@ops_slots).floor;
note "max_tuples=$max_tuples";
      my $npn=$max_tuples;
      my $npo=$max_tuples;
      @pn       =choose_n $npn, @pn;
      @po       =choose_n $npo, @po;
      @ops_slots=choose_n $nsl, @ops_slots;
note "new pn=[{@pn.map({ $_.join(',') }).join('],[')}]";
note "new po=[{@po.map({ $_.join(',') }).join('],[')}]";
note "new ops_slots={@ops_slots.join(', ')}";
      $n_solutions= @pn * @po* @ops_slots;
note "...Now:  {+@pn} x {+@po} x {+@ops_slots} RPNs=$n_solutions";
    }
    my $i=0;
# note "stepping through number sets {@pn.join(', ')}";
    for @pn -> $pn {
# note "looking at number tuple $pn ({$pn.elems} elements) ({$i*@po*@ops_slots}'th one)"; # if ++$i%100==0;
      for @po -> $po {
# note "looking at operator $po ({$po.elems} elements) with $pn";
         for @ops_slots -> $slot {  # now construct this RPN
# note "slot is $slot";
	   my ($ipn,$ipo)=(0,0);
	   my $x=$pn[$ipn++];
# note "from ipn 0, x=$x; now ipn=$ipn";
	  for $slot.comb -> $s {
# note "slot item $s with ipn=$ipn, x=$x";
	    $x~=$pn[$ipn++];
	    $x~=$po[$ipo++] for (1..$s);
# note "...and now x=$x";
	  }
	  my $rpn=RPN.new($x);
# note "saving solution $rpn = {+$rpn}";  
	  self.save_solution($rpn);
	}
      }
    }
# note "{self.solution_list}";
    self;
  }
}  # end of class declaration

sub ops_slots($n) is export {
  return ['1'] if $n==1;
  return ['11','02'] if $n==2;
  my @options=['0', '1'];
  for 2..$n-1 -> $slot {
    my @new_options;
    for @options -> $this_option {
# note "working on slot $slot of option $this_option";
      my $naccum=$slot - $this_option.comb.join('+').EVAL;
      my @accum_options;
      for 0..$naccum -> $n_in_slot {
	@accum_options.push("$this_option$n_in_slot");
      }
      @new_options.push(|@accum_options);
    }
    @options=@new_options;
  }
  return @options.map( { $_~($n-$_.comb.join('+').EVAL) } );
}

sub unique_tuples(@a) { return @a.unique(:as( *.join('') )) }

# generate the list of tuples of length $n from Bag $src while enforcing use of all in Bag $req

sub get_tuples($n,Bag $src,Bag $req) {
# note "for $n cubes, src={$src}, req={$req}";
# note "src >= req?  {$src (>=) $req}";
  return () unless $src (>=) $req;
# note "src BagHash={$src.BagHash}; req={$req}";
# note "diff={$src.BagHash (-) $req}";
  my $remain=($src.BagHash (-) $req).BagHash;
  my @req_list=$req.kxxv;
note "remaining after removing req:  {[$remain.kxxv].join(', ')}";
note "will add {@req_list.join(', ')} to each combination";
note "$n vs total={$req.total}";
  my @comb;
  if ($n > @req_list) {  # not all cubes are required
note "calculating combinations of {[ $remain.kxxv ].join(', ')} taking {$n - @req_list} at a time";
    @comb=combinations( $[ [$remain.kxxv] ], $n - @req_list );
note "there are {+@comb} combinations";
note "Combinations prior to adding req'd:  [{@comb.map({ $_.join(', ') }).join('], [')}]";
    for 0..@comb.end -> $c {
      @comb[$c]=Array.new.append(|@req_list,|@comb[$c]); 
    }
note "Combinations after adding req'd:  [{@comb.map({ $_.join(', ') }).join('], [')}]";
    @comb=unique_tuples @comb;
note "Unique Combinations after adding req'd:  [{@comb.map({ $_.join(', ') }).join('], [')}]";
  } else { @comb.push( $[ $req.kxxv ] ) }  # only one element in combination array
# note "Combinations:  [{@comb.map( { $_.join(', ') } ).join('], [') }]";
# note "Combinations first tuple first element:  {@comb[0][0]}";
  # now for each element of @comb, generate all the permutations and add to the total list
  my @perms;
# note "first tuple is {@comb[0].join(', ')}";
  for @comb -> $ti { my @pt=permutations( $[ |$ti ] ); @perms.push(|@pt) }
#  for @comb { @perms.push( permutations( $[ $_ ] ) ) }
# note "number of permutation elements = {+@perms}";
  @perms=unique_tuples( $[ @perms ] );
# note "after unique, number elements  = {+@perms}";
  return @perms; 
}
	       
sub choose_n($n,@c) {
  return @c if $n>=@c.elems;
  for 0..$n-1 { @c[$_,($_+1..@c.end).pick].=reverse }
  return @c[0..$n-1];
}

# h is array of solutions
# b is Bag of available cubes
sub filter_solutions_usable(@h, Bag $b) {
  my @r;      # result which passed filters
  for @h -> $s {
    my $bag_s=Bag.new( $s.comb );
    @r.push($s) if $b (>=) $bag_s;
  }
  return @r;
}

# h is array of solutions
# b is Bag of required cubes
sub filter_solutions_required(@h, Bag $b) {
  my @r;      # result which passed filters
  for @h -> $s {
    my $bag_s=Bag.new( $s.comb );
    @r.push($s) if $bag_s (>=) $b;
  }
  return @r;
}

=begin pod

=head1 NAME

Board.pm - Operate on Board

=head1 DESCRIPTION

  Board Components:
     * Unused cubes
     * Required cubes
     * Permitted cubes
     * Forbidden cubes
     * Goal

  The functions a Board needs to be able to do are:
     * move a cube from unused to one of the other sections
     * board display
     * decide whether goal is achievable

  Also, put the Calculation Engine here, which can be used by the 
  Computer.

=head2 Object Data

  * U - BagHash of Unused cubes
  * R - BagHash of Required cubes
  * P - BagHash of Permitted cubes
  * F - BagHash of Forbidden cubes

  * G - goal (number)

  * S - hash of Solutions where key=rpn display string, with value=rpn value

=head2 Constructors

  * new(List) - Creates a new board (resetting the %SOLUTIONS cache) using cubes in a List argument

=head2 Accessors

  * solutions()       - return a reference to the 'S' Solutions hash (should be a copy, or a true accessor fn)

=head2 Mutators

Each of these Mutators return a reference to self to allow chaining

  * move(cube,from,to) - primarily access via synonym functions (below); move a cube
                         from the "from" bag (an element of self) to the "to" bag
  * move_to_required(cube) - move a cube from the Unused bag to the Required bag
  * move_to_permitted(cube) - move a cube from the Unused bag to the Permitted bag
  * move_to_forbidden(cube) - move a cube from the Unused bag to the Forbidden bag

  * clear_solutions() - reset the cache to an empty hash

=end pod
