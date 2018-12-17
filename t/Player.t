#!/usr/bin/env perl

use strict;
use warnings;
use Test::More;
use Test::Exception;
use Data::Dumper;

BEGIN {
  use lib '/Users/imel/gitdev/donalgrant/equations/lib';
  use_ok 'Globals';
  use_ok 'Player';
  use_ok 'Board';
  use_ok 'Bag';
}

sub same_content($$$) {
  my $ha=shift;
  my $hb=shift;
  my $msg=shift;
  is_deeply( [sort { $a cmp $b } @$ha], [sort { $a cmp $b } @$hb], $msg );
}

my @player_methods=qw( new choose_goal manual );

can_ok( 'Player', @player_methods );

my $P=Player->new();
my $B=Board->new(Bag->new(qw( 1 2 2 3 - )));

isa_ok($P,'Player');
isa_ok($B,'Board');

my $g=$P->choose_goal($B);

is( $g, '1', "Only one goal possible");

$::opt{debug}=0;
ok( defined $P->choose_goal(Board->new(Bag->new(qw( 1 2 3 4 5 6 7 8 9 - + * / ^ @ )))), "Found a goal on a larger board" );
is( $P->choose_goal(Board->new(Bag->new(qw( - * - + / )))),      undef, "No goal possible: no numbers");
is( $P->choose_goal(Board->new(Bag->new(qw( 1 - + / )))),        undef, "No goal possible: single digit");
is( $P->choose_goal(Board->new(Bag->new(qw( 1 2 3 * / )))),      undef, "No goal possible:  cannot construct" );
is( $P->choose_goal(Board->new(Bag->new(qw( 1 2 1 2 * / ))), 1), undef, "No goal possible:  1 digit, no singletons" );

my $cube_str='*++------///001111112333356@@@25';

# play five boards

for (5) {
  $B=Board->new(Bag->new(split('',$cube_str)));
  my $g=$P->choose_goal($B);
  ok( defined $g, "Choose a goal" );
  ok( $B->move_to_goal($g), "move to goal" );
 inner:
  while (1) { 
    ::msg $B->display(); 
    last inner unless $P->computed($B,7); 
#    last inner unless $P->manual($B);
  }
}

done_testing();
