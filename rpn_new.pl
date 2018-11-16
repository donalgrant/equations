#!/usr/bin/env perl

use feature qw( say switch );

use strict;
use warnings;

use Algorithm::Combinatorics qw( tuples );

use lib '/Users/imel/gitdev/donalgrant/equations/lib';
use Bag;
use RPN;
use Board;
use Player;
use Cube;      

package main;

$::opt{debug}=scalar(@ARGV);

use Data::Dumper;

my @c=map { new Red_Cube   } (1..12);
my @d=map { new Blue_Cube  } (1..8);
my @e=map { new Green_Cube } (1..6);
my @f=map { new Black_Cube } (1..6);

my $CB = new Cube_Bag (@c,@d,@e,@f);
$CB->roll();

my $B = new Board(Bag->new($CB->showing()));

my $P=Player->new();
my $g=$P->choose_goal($B);

::assert { defined $g } "Found a goal from ".$B->display();

$B->move_to_goal($g);

::msg "Solutions:  ".join("\n",map { "$_" } @{ $B->solution_list() } ) if $::opt{debug};

# Play the game

# keep solutions as game is played.  Monitor bag of cubes required for
# solution, and only recalculate if bag is no longer available 

while (1) {

  last unless $P->manual($B);    # human player's move  
  last unless $P->computed($B);  # computer move

}

