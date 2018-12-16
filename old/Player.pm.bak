package Player;

use strict;
use warnings;
use feature qw( say switch );
use Data::Dumper;

use Globals;
use RPN;
use Bag;
use Board;

=head1 NAME

Player.pm - Player in an Equations Game

=head1 DESCRIPTION

Functions a player must be able to perform:

   * choose a goal from the board (Unused cubes)
   * Keep track of formulas for achieving a goal
   * decide when to recalculate formulas for achieving a goal
   * decide what cube to move on the board:
     - "forbidden" -- when cubes are not necessary for a solution and too many cubes are on the board
                      so that a solution would be otherwise made possible.
     - "timing" -- bonus move based on cubes left to go out and number of players
     - "go-out" -- when a solution can be achieved
     - "required" -- to cut down on list of allowed solutions
     - "permitted" -- random fraction of moves which would have been "required"
   * identify opponent "mistakes"---when goal has been made impossible
   * resign -- when loss is inevitable

=cut

sub new {
  my $class=shift;
  my $self={};
  return bless $self, $class;
}

sub choose_goal {
  my $self=shift;
  my $board=shift;
  my $max_digits=shift // 2;
  my $B=Board->new();  # empty bag, to be replaced
  # look for constructibility for each goal option
  for my $g (::shuffle $board->goal_options($max_digits)) {  
    $B=Board->new($board->unused());
    $B->move_to_goal($g);
    ::msg "calculating goal $g" if $::opt{debug};
    $B->clear_solutions();
    $B->calculate_solutions($_) for (3,5);
    last if scalar(@{ $B->solution_list() });
  }
  return undef unless scalar( @{ $B->solution_list() } );
  ::msg "Solutions:  ".join("\n",map { "$_" } @{ $B->solution_list() } ) if $::opt{debug};
  return $B->goal();
}

sub manual {
  my $self=shift;
  my $B=shift;
  my $bonus_taken=shift() // 0;
  my ($move,$section);

  say $B->display();
  do { print "Cube:  "; $move=<STDIN>;  chomp $move } until $B->unused()->n($move);
  do { print "To (R(equired) P(ermitted) F(orbidden) B(onus) E(quation):  "; $section=<STDIN>; chomp $section }
    until $section=~/^(R|P|F|E|B)/i;

  given ($section) {
    when (/^R/i) {  $B->move_to_required($move) }
    when (/^P/i) { $B->move_to_permitted($move) }
    when (/^F/i) { $B->move_to_forbidden($move) }
    when (/^B/i) { 
      do { ::msg "Only one bonus per turn"; return $self->manual($B,$bonus_taken) } if $bonus_taken;
      $B->move_to_forbidden($move);
      return $self->manual($B,1);
    }
    when (/^E/i) { 
      my $must_use=$B->required()->add($move);
      my $now_avail=$B->permitted()->add($must_use);
      print "Enter RPN Equation:  ";
      my $rpn_in=<STDIN>;  chomp $rpn_in;  my $rpn=RPN->new($rpn_in);
      my $rpn_cubes=Bag->new($rpn->list());
      my $result=$rpn->value();  # need to validate RPN here
      do { ::msg "Your RPN=$result, which is not the goal!";      return $self->manual($B,$bonus_taken) }
	unless $result==$B->goal();
      do { ::msg "Your RPN does not use all the required cubes!"; return $self->manual($B,$bonus_taken) } 
	unless $rpn_cubes->contains($must_use);
      do { ::msg "Not enough cubes to make your RPN!";            return $self->manual($B,$bonus_taken) } 
	unless $now_avail->contains($rpn_cubes);
      ::msg "You win!  Congratulations!";  return 0;
    }
  }
  return $self; 
}

sub computed {
  my $self=shift;
  my $B=shift;
  my $nr=$B->required()->n();
  my $max_cubes=shift() // ::max($nr+(1-($nr%2)),3);

  # need to work out bonus move for computer, triggered when forbidden move is available
  # and number of unused cubes on board module number of players is not 1.

  # need to check permitted+required+1 from unused to make sure we aren't a single 
  # cube away from a solution -- do a recalculation to make sure opponent isn't about
  # to go out.  Maybe only need to do this in the scenario where haven't got a "required" move.

  # current solutions still valid?
  my $Go_Out_Cubes=$B->required()->add($B->permitted());  # + one from the board 
 loop:  
  my $solutions=$B->solution_list();
  $solutions=Board::filter_solutions_required($solutions,$B->required());
  for ($B->unused()->set()) {
    my $go_out=Board::filter_solutions_usable($solutions,$Go_Out_Cubes->copy()->add($_));
    if (scalar @$go_out) {
      say "I win!  I can go out with solution(s):\n",join("\n",@$go_out);
      return 0;
    }
  }
  $solutions=Board::filter_solutions_usable($solutions,$B->available());
  if (scalar @$solutions) {  # solution still exists; find required or irrelevant cubes
    if (rand()<0.1) {  # do something crazy about 10% of the time
      $B->move_to_forbidden($B->unused()->random_item());
      return $self;
    }
    my $keep=new Bag ();  # keep is all the cubes used in solutions -- don't forbid these
    for (@$solutions) { $keep->union($_->list()) }  # each $_ is now an RPN object
    # try to put cube in required for shortest solution
#    my ($shortest_rpn) = sort { length($a) <=> length($b) } @$solutions;  # solution we're working towards
    my ($shortest_rpn) = ::shuffle @$solutions;  # solution we're working towards
    my $shortest_rpn_cubes = Bag->new($shortest_rpn->list());
    # need to qualify $req_options by what's actually unused!  (could already be in permitted, so not unused)
    my $req_options=$shortest_rpn_cubes->copy()->remove($B->required())->intersect($B->unused()); 
    my $n_from_solve=$shortest_rpn_cubes->copy()->remove($B->required())->remove($B->permitted())->n();  # n left to solve
    if ($req_options->n() && ($n_from_solve>2)) {  # can -->req'd if >2 to solve (no go out) & non-empty req'd options
      my $cube=$req_options->random_item();
      ::assert { $B->unused()->contains($cube) } "cube $cube is actually still unused";
      if (rand()>0.3) { $B->move_to_required($cube) } else { $B->move_to_permitted($cube) }  # change it up
    } else {
      my $excess=$B->unused()->copy()->remove($keep);
      if ($excess->n()) {
	$B->move_to_forbidden($excess->random_item());
      } else {  # no forbidden cubes -- put in permitted
	$B->move_to_permitted($B->unused()->random_item());
      }
    }
  } else {  # have to now go and find new solutions
    say "Recalculating solutions...";
    # before we clear out the solutions and start over, can we build on our current solution list?
    my $old_solutions=$B->solution_list();  
    ::msg "old solutions ".Dumper($old_solutions) if $::opt{debug};
    for my $old_rpn (@$old_solutions) {
      ::msg "looking at this RPN:  ".$old_rpn->display() if $::opt{debug};
      # generate the bag of cubes for this rpn
      my $rpn_bag=Bag->new($old_rpn->list());
      # figure out which required is not in the solution
      my $missing=$B->required()->remove($rpn_bag);
      ::msg "missing item is ".$missing->display() if $::opt{debug};
      # take the available cubes minus those in solution; separate into operators and numbers
      my $avail=$B->available()->remove($rpn_bag);
      ::msg "available for new board = ".$avail->display() if $::opt{debug};
      my @avail_num = Board::num_grep $avail->list();  ::msg "nums = ".join(',',@avail_num) if $::opt{debug};
      my @avail_ops = Board::ops_grep $avail->list();  ::msg "ops  = ".join(',',@avail_ops) if $::opt{debug};
      # step through operators
      for my $op (::unique @avail_ops) {
	::msg "try op $op" if $::opt{debug};
	#    generate a Board with one required cube and the rest of the available cubes as unused (not including this op)
	my $NB=Board->new($avail->copy()->remove($op)->add($missing));
	$NB->move_to_required($_) for $missing->list();
	#    set goal to either 0 or 1 depending on operator:
	if ($op=~/[+-]/)       { $NB->install_goal('0') }
	#      +-   => goal=0
	elsif ($op=~m{[*/^@]}) { $NB->install_goal('1') }
	#      */^@ => goal=1
	#    calculate goals for (1,3,5,7) cubes
	::msg "Temp Board now set up:  ".$NB->display() if $::opt{debug};
	my $i_solutions;
	for (1,3,5) {
	  $NB->calculate_solutions($_);
	  $i_solutions=$NB->solution_list();
	  last if scalar(@$i_solutions);
	}
	::msg "Temp Board solutions ".Dumper($NB->solution_list()) if $::opt{debug};
	# append new solutions to old ones
	if (scalar(@$i_solutions)) {
	  $B->clear_solutions();
	  for my $new_solution (@$i_solutions) {
	    my $rpn=($op eq '@') ? RPN->new("$new_solution$old_rpn$op") : RPN->new("$old_rpn$new_solution$op");
	    ::msg "saving new solution:  $rpn" if $::opt{debug};
	    $B->save_solution($rpn);
	  }
	  ::msg "And now redo the turn with new solution list" if $::opt{debug};
	  return $self->computed($B,$max_cubes);
	}
	# and we should consider moving solutions to Player instead of Board
      }
    }
    ::msg "finished" if $::opt{debug};
    $B->clear_solutions();
    # try to construct the goal
    $max_cubes+=2;
    die "I challenge you;  I can't see the solution" if $max_cubes>$B->available()->n();  # don't die, but get RPN, eval, then maybe concede
    $B->calculate_solutions($max_cubes);
    say Dumper($B->solution_list()) if $::opt{debug};
    return $self->computed($B,$max_cubes);
  }

  return $self;
}

1;
