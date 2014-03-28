package Board;

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

=cut


use strict;
use warnings;
use feature qw( switch say );

use Algorithm::Combinatorics qw( tuples combinations permutations );

use lib '/Users/imel/gitdev/equations/lib';
use Globals;
use Bag;
use RPN;

=head2 Object Data

  * U - Bag of Unused cubes
  * R - Bag of Required cubes
  * P - Bag of Permitted cubes
  * F - Bag of Forbidden cubes

  * G - goal (number)

  * S - hash of Solutions where key=rpn display string, with value=rpn value

=head2 Constructors

  * new(Bag) - Creates a new board (resetting the %SOLUTIONS cache) using cubes in Bag argument

=cut

sub new {
  my $class=shift;
  my $cubes=shift;  # bag of cubes
  my $self={ U=>$cubes, R=>Bag->new(), P=>Bag->new(), F=>Bag->new(), G=>undef, S=>{} };
  bless $self, $class;
  return $self->clear_solutions();
}

=head2 Accessors

  * solutions()       - return a reference to the 'S' Solutions hash (should be a copy, or a true accessor fn)

=cut

# because only *simple* hashes can be shared, must
# go through some contortions to share a hash of 
# arrays.  Should maybe do an entire second package for this.

use Data::Dumper;

sub clear_solutions { my $self=shift; $self->{S}={}; return $self }

sub save_solution {
  my $self=shift;
  my $rpn=shift;
  my $v=$rpn->value();
  return unless defined $v;
#  ::assert { length($self->goal()) } "calculations only done once goal has been set";
  return if $self->goal()!=$v;  # we've set a goal, and this ain't it
  $self->{S}{"$rpn"}=$v; 
}

sub solution_list {
  my $self=shift; 
  my @defined_rpn_keys=grep { defined $self->{S}{$_} } keys %{$self->{S}};
  return [] unless scalar(@defined_rpn_keys);
  return [ map { RPN->new($_) } @defined_rpn_keys ];
}

sub required  { return $_[0]->{R}->copy() }
sub permitted { return $_[0]->{P}->copy() }
sub forbidden { return $_[0]->{F}->copy() }
sub unused    { return $_[0]->{U}->copy() }

sub goal { return $_[0]->{G} // '' }

# duplicate copies here, but don't worry about speed
sub available { return $_[0]->unused()->add($_[0]->required())->add($_[0]->permitted()) }

sub display {
  my $self=shift;
  my $out=
    "Board:      ".join('',sort { $a cmp $b } $self->unused()->list())    . "\n" .
    "Required:   ".join('',sort { $a cmp $b } $self->required()->list())  . "\n" .
    "Permitted:  ".join('',sort { $a cmp $b } $self->permitted()->list()) . "\n" .
    "Forbidden:  ".join('',sort { $a cmp $b } $self->forbidden()->list()) . "\n" .
    "Goal:  ".$self->goal();
  return $out;
}



=head2 Mutators

Each of these Mutators return a reference to self to allow chaining

  * move(cube,from,to) - primarily access via synonym functions (below); move a cube
                         from the "from" bag (an element of self) to the "to" bag
  * move_to_required(cube) - move a cube from the Unused bag to the Required bag
  * move_to_permitted(cube) - move a cube from the Unused bag to the Permitted bag
  * move_to_forbidden(cube) - move a cube from the Unused bag to the Forbidden bag

  * clear_solutions() - reset the cache to an empty hash

=cut

sub move {
  my $self=shift;
  my $cube=shift;
  my $from=shift;
  my $to=shift;
  ::msg "move $cube from $from to $to" if $::opt{debug};
  ::msg Dumper($self) if $::opt{debug};
  ::assert { exists $self->{$from}           } "$from bag exists";
  ::assert { exists $self->{$to}             } "$to bag exists";
  ::assert { $self->{$from}->contains($cube) } "$cube is still available in $from";
  $self->{$from}->remove($cube);
  $self->{$to}->add($cube);
  return $self;
}

sub move_to_required  { return $_[0]->move($_[1],'U','R') }
sub move_to_permitted { return $_[0]->move($_[1],'U','P') }
sub move_to_forbidden { return $_[0]->move($_[1],'U','F') }

sub move_to_goal { 
  my $self=shift;
  my $goal=shift;
  for (split('',$goal)) { $self->{U}->remove($_) }
  return $self->{G}=$goal;
}

sub install_goal {
  my $self=shift;
  my $goal=shift;
  $self->{G}=$goal;
  return $self;
}

sub num_grep(@)  { return grep /\d/, @_ }
sub ops_grep(@)  { return grep m{^[-+*/@^]$}, @_ }

sub goal_options {
  my $self=shift; 
  my $max_digits=shift() // 3;
  my $digits=Bag->new( num_grep $self->available()->list() ); 
  my @goal_options=$digits->singletons();
  for my $k (2..$max_digits) {
    last if $k>$digits->n();
    my @p=tuples( [ $digits->list() ], $k ); 
    push @goal_options, grep { !/^0\d+/ } ::unique map { join('',@$_) } @p;
  }
  return @goal_options;
}

# not a member function
sub ops_slots {
  my $n=shift; 
  return ('1') if $n==1;
  return ('11','02') if $n==2;
  my @options=('0', '1');
  for my $slot (2..$n-1) {  
    my @new_options;
    for my $this_option (@options) {   
      my $naccum=$slot - eval join('+',split('',$this_option));
      my @accum_options;
      for my $n_in_slot (0..$naccum) {
	push @accum_options, "$this_option$n_in_slot";
      }
      push@new_options,@accum_options;
    }
    @options=@new_options;
  }
  @options = map { $_ . ($n-eval join('+',split('',$_))) } @options;   # final slot must have all remaining
  return @options;
}

# also not a member function

sub unique_tuples(@) { return map { [split('',$_)] } ::unique map { join('',@$_) } @_ }

# generate the list of tuples of length $n from Bag $src while enforcing use of all in Bag $req
sub get_tuples($$$) {
  my $n=shift;
  my $src=shift;
  my $req=shift;
  ::msg "for $n cubes, src=@{[ $src->display() ]}, req=@{[ $req->display() ]}" if $::opt{debug};
  return () unless $src->contains($req);
  my $remain=$src->copy()->remove($req);
  ::msg "remaining after removing req:  @{[ $remain->display() ]}" if $::opt{debug};
  my @req_list=$req->list();
#  ::msg "will add @{[ join(',',@req_list) ]} to each comb" if $::opt{debug};
  my @comb;
  if ($n>$req->n()) {  # not all cubes are required
    @comb=combinations( [ $remain->list() ], $n-$req->n() );
#    ::msg "Combinations prior to adding req'd:  ".Dumper(\@comb) if $::opt{debug};
    @comb=unique_tuples map { push @$_,@req_list; $_ } @comb;
#    ::msg "Combinations after adding req'd: ".Dumper(\@comb) if $::opt{debug};
  } else { @comb=( [ @req_list ] ) }  # only one element in combination array
#  ::msg "Combinations:  ".Dumper(\@comb) if $::opt{debug};
  # now for each element of @comb, generate all the permutations and add to the total list
  my @perms;
#  ::msg "for example, for ".Dumper($comb[0]).", permutations=".Dumper([permutations($comb[0])]) if $::opt{debug};
  for (@comb) { push @perms, permutations( $_ ) }
  ::msg "number of permutation elements = ".scalar(@perms) if $::opt{debug};
  @perms=unique_tuples @perms;
  ::msg "after unique, number elements  = ".scalar(@perms) if $::opt{debug};
  return @perms; 
}

sub req_num_tuples { 
  my $self=shift;
  my $num=Bag->new( num_grep $self->required()->list() );
  return map { [split('',$_)] } grep { Bag->new(split('',$_))->contains($num) } map { join('',@$_) } @_; 
}
sub req_ops_tuples { 
  my $self=shift;
  my $ops=Bag->new( ops_grep $self->required()->list() );
  return map { [split('',$_)] } grep { Bag->new(split('',$_))->contains($ops) } map { join('',@$_) } @_; 
}
	       
sub choose_n($@) {
  my $n=shift;
  return @_ if $n>=scalar(@_);
  return @{ [ ::shuffle @_ ] }[(0..$n-1)];
}

sub calculate_solutions {
  my $self=shift;
  my $ncubes=shift;   # maximum number of cubes to use
  ::assert { length($self->goal()) } "Goal is set before calculating";
  my $bag=$self->available();
  ::msg "Calculate solutions for $ncubes cubes from set of @{[ $bag->n() ]} cubes" if $::opt{debug};
  my $num=Bag->new( num_grep $bag->list() ); 
  my $ops=Bag->new( ops_grep $bag->list() ); 
  ::msg "Number   cubes available: ".$num->display() if $::opt{debug};
  ::msg "Operator cubes available: ".$ops->display() if $::opt{debug};
  my $nops=(int($ncubes/2) < $ops->n()) ? int($ncubes/2) : $ops->n();  # the largest number of ops cubes possible
  my $nnum=( ($nops+1) > $num->n()) ? $num->n() : ($nops+1); # the largest number of num cubes possible, based on ops too
  $nops = $nnum-1;                                           # no more ops cubes than num cubes - 1
  return $self unless $nops>=1 && $nnum>=2;
  my @pn=get_tuples $nnum, $num, Bag->new( num_grep $self->required()->list() );
  my @po=get_tuples $nops, $ops, Bag->new( ops_grep $self->required()->list() );
  my @ops_slots=ops_slots($nops);
  my $max_solutions=200000;
  my $n_solutions=scalar(@pn)*scalar(@po)*scalar(@ops_slots);
  ::msg "@{[ scalar(@pn) ]} x @{[ scalar(@po) ]} x @{[ scalar(@ops_slots) ]} RPNs=$n_solutions" if $::opt{debug};
  return $self unless $n_solutions;
  if ($n_solutions>$max_solutions) {
    ::msg "...and that's too many by a factor of @{[ $n_solutions/$max_solutions ]}; reducing" if $::opt{debug};
    my $reduce_factor=::min( 4.0, ($n_solutions/$max_solutions)**(1.0/3.0) );  
    my $nsl=::max( 3, int(scalar(@ops_slots)/$reduce_factor) ); 
    my $max_tuples=int(sqrt($max_solutions/scalar(@ops_slots)));
    my $npn=$max_tuples;
    my $npo=$max_tuples;
    @pn       =choose_n $npn, @pn;
    @po       =choose_n $npo, @po;
    @ops_slots=choose_n $nsl, @ops_slots;
    $n_solutions=scalar(@pn)*scalar(@po)*scalar(@ops_slots);
    ::msg "...Now:  @{[ scalar(@pn) ]} x @{[ scalar(@po) ]} x @{[ scalar(@ops_slots) ]} RPNs=$n_solutions" if $::opt{debug};
  }
my $i=0;
  for my $pn (@pn) {
#    ::msg "looking at number tuple ".($i*scalar(@po)*scalar(@ops_slots)) if ++$i%100==0;
    for my $po (@po) {
      for my $slot (@ops_slots) {  # now construct this RPN
	my ($ipn,$ipo)=(0,0);
	my $x=$pn->[$ipn++];
	for my $s (split('',$slot)) {
	  $x.=$pn->[$ipn++];
	  $x.=$po->[$ipo++] for (1..$s);
	}
	my $rpn=RPN->new($x);  
	$self->save_solution($rpn);
      }
    }
  }
  ::msg Dumper($self->solution_list()) if $::opt{debug};
  return $self;
}

sub filter_solutions_usable {
  my $h=shift;   # ref to array of solutions
  my $b=shift;   # bag of available cubes
  my $r=[];      # result which passed filters
  for my $s (@$h) {
    my $bag_s=Bag->new( split('',$s) );
    push @$r, $s if $b->contains($bag_s);
  }
  return $r;
}

sub filter_solutions_required {
  my $h=shift;   # ref to array of solutions
  my $b=shift;   # bag of required cubes
  my $r=[];      # result which passed filters
  for my $s (@$h) {
    my $bag_s=Bag->new( $s->list() );
    push @$r, $s if $bag_s->contains($b);
  }
  return $r;
}

1;

=pod

sub calculate_solutions_old {
  my $self=shift;
  my $max_cubes=shift;    # maximum number of cubes to use
  my $bag=$self->available();
  my @list;
  for (num_grep $bag->set()) { push @list,$self->_process_rpn(RPN->new($_),$bag,$max_cubes) }
  for (@list) { $self->save_solution($_) if $_>=0 && $_<=1000 && $_==int($_) }
  return $self;
}

sub _process_rpn {
  my $self=shift;
  my $rpn=shift;    # rpn string to process
  my $b0=shift;     # full bag of cubes
  my $max=shift;    # max number of cubes (maxdepth)
  $self->{C}{"$max:$rpn"}=1;    # we've now processed this RPN.  Note that for a given bag, residual bag depends on rpn
  my @rpn=($rpn);
  my @rpn_list=$rpn->list();
  my @list=$b0->copy()->remove(@rpn_list)->list();  # remove cubes used so far
  my $nums=Bag->new(num_grep @list);
  my $ops =Bag->new(ops_grep @list);
  return @rpn if $nums->empty() || $ops->empty();
  my $v=$rpn->value();
  return @rpn if scalar(@rpn_list) >= $max;
  for my $n ($nums->set()) {
    for my $op ($ops->set()) {
      unless ( (($n==0) && ($op eq '/')) ||                           # no divide by zero
	       (($v==0) && ($op eq '@')) ||                           # no 0th root
	       (($n<0)  && ($op eq '@') && 
		(($v!=int($v)) || ($v%2==0))) ) {                     # roots of negative numbers
	my $new_rpn=RPN->new($rpn.$n.$op);
	push @rpn, $self->_process_rpn($new_rpn,$b0,$max) unless exists $self->{C}{"$max:$new_rpn"};  # don't re-process rpns
      }
      next if ($op eq '+') || ($op eq '*');                           # commutative operators
      next if $n==$v;                                                 # symmetric expression
      unless ( (($v==0) && ($op eq '/')) || 
	       (($n==0) && ($op eq '@')) ||
	       (($v<0)  && ($op eq '@') &&
		($n%2==0)) ) {                       # same as above, but for swapped args
	my $new_rpn=RPN->new($n.$rpn.$op);
	push @rpn, $self->_process_rpn($new_rpn,$b0,$max);
      }
    }
  }
  return @rpn;
}

=cut
