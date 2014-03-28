#!/usr/bin/env perl

use feature qw( say switch );

use strict;
use warnings;

use Algorithm::Combinatorics qw( tuples );

use lib '/Users/imel/Desktop/Dropbox/dev/equations/lib';
use Bag;
use RPN;
use Board;

sub unique(@) { return keys %{{ map { $_=>undef } @_ }} }

sub shuffle(@) {
  for my $i (0..$#_-1) {
    my $r=int(rand()*($#_-$i));
    ($_[$i],$_[$r+$i])=($_[$r+$i],$_[$i]);
  }
  return @_;
}
      
sub calc {
  my $n1=shift;
  my $op=shift;
  my $n2=shift;
  given ($op) {
    when('+') { return $n1+$n2 }
    when('-') { return $n1-$n2 }
    when('*') { return $n1*$n2 }
    when('/') { return $n1/$n2 }
    when('^') { return $n1**$n2 }
    when('@') { return $n2**(1.0/$n1) }
    default   { die "Unrecognized operator:  $op" }
  }
}

sub nums_grep(@) { return grep /\d/, @_ }
sub ops_grep(@)  { return grep m{^[-+*/@^]$}, @_ }

package Cube;

sub new {
  my $class=shift;
  my $self={};
  $self->{faces}=[ @_ ];
  bless $self, $class;
  $self->roll();
  return $self;
}

sub roll {
  my $self=shift;
  my $n=scalar(@{$self->{faces}});
  $self->{showing}=$self->{faces}[int(rand()*$n)];
}

sub showing { my $self=shift; return $self->{showing} }

package Red_Cube;  
push @Red_Cube::ISA, qw( Cube );
sub new { my $class=shift; return $class->SUPER::new('0','1','2','3','+','-') }

package Blue_Cube;  
push @Blue_Cube::ISA, qw( Cube );
sub new { my $class=shift; return $class->SUPER::new('0','1','2','3','*','/') }

package Green_Cube;  
push @Green_Cube::ISA, qw( Cube );
sub new { my $class=shift; return $class->SUPER::new('4','5','6','^','/','-') }

package Black_Cube;  
push @Black_Cube::ISA, qw( Cube );
sub new { my $class=shift; return $class->SUPER::new('7','8','9','/','-','@') }

package Cube_Bag;
sub new     { my $class=shift; return bless { dice=>[ @_ ] }, $class }
sub roll    { my $self=shift;  for (@{$self->{dice}}) { $_->roll() } }
sub showing { my $self=shift;  return map { $_->showing() } @{$self->{dice}} }
sub unique  { my $self=shift;  return ::unique $self->showing() }

package main;

use Data::Dumper;

my %cache=();
my %proc=();    # hash of already-processed rpn keys

my ($ccount,$ncount)=(0,0);

sub eval_rpn {
  my $rpn=shift;
  ++$ccount;
  return $cache{$rpn} if exists $cache{$rpn};
  ++$ncount;
  my @list=split('',$rpn);
  return 0 unless (@list);
  my @stack;
  while (@list && push @stack, shift @list) {
    next unless $stack[-1]=~m{^[-+*/@^]$};
    my $op=pop @stack;
    my $n2=pop @stack;
    my $n1=pop @stack;
    push @stack, ::calc($n1,$op,$n2);
  }
  return $cache{$rpn}=shift @stack;
}

sub process_rpn {
  my $rpn=shift;    # rpn string to process
  my $b0=shift;     # full bag of cubes
  my $max=shift;    # max number of cubes (maxdepth)
  my $goal=shift() // undef;    # optionally, quit if we find an $rpn which evaluates to the Goal
  $proc{$rpn}=1;    # we've now processed this string.  Note that for a given initial bag, residual bag depends on rpn
  my @rpn=($rpn);
  my @list=$b0->copy()->remove(split('',$rpn))->list();  # remove cubes used so far
  my $nums=new Bag ::nums_grep @list;
  my $ops =new Bag ::ops_grep  @list;
  return @rpn if $nums->empty() || $ops->empty();
  my $v=eval_rpn($rpn);
  return @rpn if defined $goal && $v==$goal;   # stop if goal is defined and achieved
  return @rpn if length($rpn) >= $max;
  for my $n ($nums->set()) {
    for my $op ($ops->set()) {
      unless ( (($n==0) && ($op eq '/')) ||                           # no divide by zero
	       (($v==0) && ($op eq '@')) ||                           # no 0th root
	       (($n<0)  && ($op eq '@') && 
		(($v!=int($v)) || ($v%2==0))) ) {                       # roots of negative numbers
	my $new_rpn=$rpn.$n.$op;
	return ($new_rpn) if defined $goal && eval_rpn($new_rpn)==$goal;
	push @rpn, process_rpn($new_rpn,$b0,$max,$goal) unless $proc{$new_rpn};  # don't re-process rpns
      }
      next if ($op eq '+') || ($op eq '*');                           # commutative operators
      next if $n==$v;                                                 # symmetric expression
      unless ( (($v==0) && ($op eq '/')) || 
	       (($n==0) && ($op eq '@')) ||
	       (($v<0)  && ($op eq '@') &&
		($n%2==0)) ) {                       # same as above, but for swapped args
	my $new_rpn=$n.$rpn.$op;
	return ($new_rpn) if defined $goal && eval_rpn($new_rpn)==$goal;
	push @rpn, process_rpn($new_rpn,$b0,$max,$goal) unless $proc{$new_rpn};  # don't re-process rpns
      }
    }
  }
  return @rpn;
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
    my $bag_s=Bag->new( split('',$s) );
    push @$r, $s if $bag_s->contains($b);
  }
  return $r;
}

my @c=map { new Red_Cube   } (1..12);
my @d=map { new Blue_Cube  } (1..8);
my @e=map { new Green_Cube } (1..6);
my @f=map { new Black_Cube } (1..6);

my $B = new Cube_Bag (@c,@d,@e,@f);
$B->roll();

say "showing:  ",sort { $a cmp $b } $B->showing();

# 1.  Select a goal
#     a.  Generate the list of selectable goals

my %H;
my $BOARD=Bag->new( $B->showing() );
my $digits=Bag->new( ::nums_grep $BOARD->list() );
my @goal_options=$digits->singletons();
my $solution;
my $solution_found=0;
for my $k (2..2) {   # 1, 2, or 3 digit goals
  last if $k>$digits->n();
  my @p=tuples([$digits->list()],$k);
  push @goal_options, grep { !/^0\d+/ } ::unique map { join('',@$_) } @p;
}
# say "goal options:  ",join(',',sort { $a<=>$b } @goal_options);

# look for constructibility for each goal option
for my $g (::shuffle @goal_options) {
  say "evaluating $g";
  my $N=$BOARD->copy()->remove(split('',$g));
  # try to construct this goal
  my $max_cubes=5;
  my @list=();
  %proc=();  # reset the cache
  for (::nums_grep $N->set()) { push @list,process_rpn($_,$N,$max_cubes) }
  for (@list) { push @{$H{::eval_rpn($_)}},$_ }
  $solution=$g;
  $solution_found=exists $H{$g};
  last if $solution_found;
}

say Dumper(\%H) unless $solution_found;
say "choose goal of $solution; plan options are:\n @{[ join(qq(\n),@{$H{$solution}}) ]}";


# Play the game

# keep solutions as game is played.  Monitor bag of cubes required for
# solution, and only recalculate if bag is no longer available 

my $done=0;

my $REQUIRED =new Bag ();
my $PERMITTED=new Bag ();
my $FORBIDDEN=new Bag ();

while (not $done) {

  # user section

  my $bonus_taken=0;

user_move:
  my $move='';
  say "Board:      ",join('',sort { $a cmp $b } $BOARD->list());
  say "Required:   ",join('',sort { $a cmp $b } $REQUIRED->list());
  say "Permitted:  ",join('',sort { $a cmp $b } $PERMITTED->list());
  say "Forbidden:  ",join('',sort { $a cmp $b } $FORBIDDEN->list());
  say "Goal:  $solution";
  until ($BOARD->n($move)) {
    print "Cube:  "; $move=<STDIN>;  chomp $move;
  }
  my $section='';
  until ($section=~/^(R|P|F|E|B)/i) {
    print "To (R(equired) P(ermitted) F(orbidden) B(onus) E(quation):  "; $section=<STDIN>; chomp $section;
  }
  $BOARD->remove($move);
  given ($section) {
    when (/^R/i) { $REQUIRED->add($move) }
    when (/^P/i) { $PERMITTED->add($move) }
    when (/^F/i) { $FORBIDDEN->add($move) }
    when (/^B/i) { 
      do { say "Only one bonus per turn"; goto user_move } if $bonus_taken;
      $FORBIDDEN->add($move); 
      $bonus_taken=1; 
      goto user_move; 
    }
    when (/^E/i) { 
      $REQUIRED->add($move); 
      print "Enter RPN Equation:  ";
      my $rpn=<STDIN>;  chomp $rpn;
      my $rpn_cubes=Bag->new(split('',$rpn));
      my $Avail_cubes=$REQUIRED->copy()->add($PERMITTED);
      my $result=eval_rpn($rpn);  # need to validate RPN here
      die "Your RPN=$result, which is not the goal!" unless $result==$solution;
      die "Your RPN does not use all the REQUIRED section cubes!" unless $rpn_cubes->contains($REQUIRED);
      die "Not enough cubes to make your RPN!" unless $Avail_cubes->contains($rpn_cubes);
      die "You win!  Congratulations!";
    }
  }
  
  # computer move

  # need to work out bonus move for computer, triggered when forbidden move is available
  # and number of unused cubes on board module number of players is not 1.

  my $max_cubes=5;
  # current solutions still valid?
  my $Usable=$BOARD->copy()->add($REQUIRED)->add($PERMITTED);
  my $Available=$REQUIRED->copy()->add($PERMITTED);  # + one from the board
 loop:  
  $H{$solution}=filter_solutions_required($H{$solution},$REQUIRED);
  for ($BOARD->set()) {
    my $go_out=filter_solutions_usable($H{$solution},$Available->copy()->add($_));
    if (scalar @$go_out) {
      say "I win!  I can go out with solution(s):\n",join("\n",@$go_out);
      exit;
    }
  }
  $H{$solution}=filter_solutions_usable($H{$solution},$Usable);
  say Dumper($H{$solution});
  if (scalar @{$H{$solution}}) {  # solution still exists; find required or irrelevant cubes
    my $keep=new Bag ();
    for (@{$H{$solution}}) { $keep->union(split('',$_)) }
    # try to put cube in required for shortest solution
    my ($shortest_rpn) = sort { length($a) <=> length($b) } @{$H{$solution}};
    my $shortest_rpn_cubes = Bag->new(split('',$shortest_rpn));
    my @req_options=$shortest_rpn_cubes->copy()->remove($REQUIRED)->list();
    my $n_from_solve=$shortest_rpn_cubes->copy()->remove($REQUIRED)->remove($PERMITTED)->n();
    if (scalar(@req_options) && $n_from_solve>2) {
      my $cube=shift @req_options;
      $BOARD->remove($cube); 
      if (rand()>0.6) { $REQUIRED->add($cube) } else { $PERMITTED->add($cube) }  # change it up
    } else {
      my $excess=$BOARD->copy()->remove($keep);
      if ($excess->n()) {
	my $cube=$excess->random_item();
	$BOARD->remove($cube); $FORBIDDEN->add($cube);
      } else {  # no forbidden cubes -- put in permitted
	my $cube=$BOARD->random_item();
	$BOARD->remove($cube); $PERMITTED->add($cube);
      }
    }
  } else {  # have to now go and find new solutions
    say "Recalculating solutions...";
    %H=();
    my $Usable=$BOARD->copy()->add($REQUIRED)->add($PERMITTED);
    # try to construct the goal
    $max_cubes+=2;
    die "I challenge you;  I can't see the solution" if $max_cubes>9;
    my @list=();
    %proc=();  # reset the cache
    for (::nums_grep $Usable->set()) { push @list,process_rpn($_,$Usable,$max_cubes) }
    for (@list) { push @{$H{::eval_rpn($_)}},$_ }
    say Dumper($H{$solution});
    goto loop;   # wow, a goto loop!!
  }
}

