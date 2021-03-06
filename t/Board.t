#!/usr/bin/env perl

use strict;
use warnings;
use feature qw( say switch );
use Test::More;
use Test::Exception;
use Data::Dumper;

BEGIN {
  use lib '/Users/imel/gitdev/equations/lib';
  use_ok 'Board';
  use_ok 'Bag';
}

sub same_content($$$) {
  my $ha=shift;
  my $hb=shift;
  my $msg=shift;
  is_deeply( [sort { $a cmp $b } @$ha], [sort { $a cmp $b } @$hb], $msg );
}

my @board_methods=qw( new move move_to_required move_to_forbidden move_to_permitted 
		      clear_solutions solution_list goal_options calculate_solutions );

can_ok( 'Board', @board_methods );

my $cubes=Bag->new(qw( 1 2 2 3 - + ));

my $B= Board->new($cubes);
isa_ok($B,'Board');

same_content( [ $B->unused()->list()    ], [ qw( 1 2 2 3 - + ) ],    "initial unused"    );
same_content( [ $B->required()->list()  ], [ qw( ) ],                "initial required"  );
same_content( [ $B->permitted()->list() ], [ qw( ) ],                "initial permitted" );
same_content( [ $B->forbidden()->list() ], [ qw( ) ],                "initial forbidden" );
same_content( [ $B->available()->list() ], [ $B->unused()->list() ], "initial available" );

is_deeply( [ sort {$a<=>$b} $B->goal_options()                                  ], 
	   [ sort {$a<=>$b} ( 1,3,12,13,21,22,23,31,32,
			      122,123,132,212,213,221,223,231,232,312,321,322 ) ], "initial goals" );

$B->move_to_required('-');
$B->move_to_forbidden('3');
$B->move_to_permitted('+');

same_content( [ $B->unused()->list()    ], [ qw( 1 2 2 ) ], "initial unused"    );
same_content( [ $B->required()->list()  ], [ qw( - ) ],     "initial required"  );
same_content( [ $B->permitted()->list() ], [ qw( + ) ],     "initial permitted" );
same_content( [ $B->forbidden()->list() ], [ qw( 3 ) ],     "initial forbidden" );
same_content( [ $B->available()->list() ], 
	      [ $B->unused()->add($B->required())->add($B->permitted())->list() ],  "initial available" );


is_deeply( [ sort {$a<=>$b} $B->goal_options()         ], 
	   [ sort {$a<=>$b} ( 1,12,21,22,122,212,221 ) ], "initial goals" );

$B=Board->new(Bag->new(qw( 0 0 0 0 2 2 3 3 3 4 5 7 7 8 8 * + + + - - / / @ @ ^ 8 3 + 8 - - / / ^ * 2 5 )));
$B->move_to_permitted($_) for qw( 8 3 + );
$B->move_to_required($_)  for qw( 8 - - ^ * 2 5 );

$B->clear_solutions();
is_deeply( [ $B->solution_list() ], [ [] ], "Empty solution list" );
dies_ok( sub { $B->calculate_solutions(5) }, "Attempt to Calculate Solutions prior to setting goal should fail." );

$B->move_to_goal("20");
is( $B->goal(), 20, "Goal is set for this board" );

lives_ok( sub { $B->calculate_solutions(5) }, "Can calculate once goal has been set." );
is_deeply( [ $B->solution_list() ], [ [] ], "Empty solution list (still)" );

lives_ok( sub { $B->calculate_solutions(9) }, "Calculate with more cubes" );
my $rpn_list=$B->solution_list();
is( $_->value(), $B->goal(), "$_ from solution list evals to goal" ) for @$rpn_list;

done_testing;
