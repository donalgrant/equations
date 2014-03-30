#!/usr/bin/env perl

use strict;
use warnings;
use feature qw( say switch );
use Test::More;
use Data::Dumper;

BEGIN {
  use lib '/Users/imel/gitdev/equations/lib';
  use_ok 'Cube';
}

sub same_content($$$) {
  my $ha=shift;
  my $hb=shift;
  my $msg=shift;
  is_deeply( [sort { $a cmp $b } @$ha], [sort { $a cmp $b } @$hb], $msg );
}

my @cube_methods=qw( new roll showing );

can_ok( $_, @cube_methods ) for qw( Cube Red_Cube Blue_Cube Green_Cube Black_Cube );
can_ok( 'Cube_Bag', qw( new roll showing unique ) );

my @f=qw( 1 2 3 4 5 6 );
my $regex='['.join('',@f).']';
my $c=Cube->new(@f);
isa_ok( $c, 'Cube' );
same_content [ $c->faces() ], [ @f ], "Faces properly assigned";
like( $c->showing(), qr/$regex/, "roll shows one of allowed faces" );
isa_ok( $c->roll(), 'Cube' );
my %h=();
for (1..10000) { $h{$c->roll()->showing()}=1 }  # 10,000 rolls should be enough to get at least one face each!
same_content [keys %h], [@f], "All faces rolled after 10,000 tries";

@f=qw( 0 1 2 3 + - );
$regex='['.join('',@f).']';
my $red=Red_Cube->new(@f);
isa_ok( $red, 'Red_Cube' );
like( $red->showing(), qr/$regex/, "roll shows one of allowed Red_Cube faces" );
isa_ok( $red->roll(), 'Red_Cube' );
%h=();
for (1..10000) { $h{$red->roll()->showing()}=1 }  # 10,000 rolls of a Red_Cube
same_content [keys %h], [@f], "All faces rolled after 10,000 tries";

@f=qw( 0 1 2 3 * / );
$regex='['.join('',@f).']';
my $blue=Blue_Cube->new(@f);
isa_ok( $blue, 'Blue_Cube' );
like( $blue->showing(), qr/$regex/, "roll shows one of allowed Blue_Cube faces" );
isa_ok( $blue->roll(), 'Blue_Cube' );
%h=();
for (1..10000) { $h{$blue->roll()->showing()}=1 }  # 10,000 rolls of a Blue_Cube
same_content [keys %h], [@f], "All faces rolled after 10,000 tries";

@f=qw( 4 5 6 ^ * - );
$regex='['.join('',@f).']';
my $green=Green_Cube->new(@f);
isa_ok( $green, 'Green_Cube' );
like( $green->showing(), qr/$regex/, "roll shows one of allowed Green_Cube faces" );
isa_ok( $green->roll(), 'Green_Cube' );
%h=();
for (1..10000) { $h{$green->roll()->showing()}=1 }  # 10,000 rolls of a Green_Cube
same_content [keys %h], [@f], "All faces rolled after 10,000 tries";

@f=qw( 7 8 9 @ / - );
$regex='['.join('',@f).']';
my $black=Black_Cube->new(@f);
isa_ok( $black, 'Black_Cube' );
like( $black->showing(), qr/$regex/, "roll shows one of allowed Black_Cube faces" );
isa_ok( $black->roll(), 'Black_Cube' );
%h=();
for (1..10000) { $h{$black->roll()->showing()}=1 }  # 10,000 rolls of a Black_Cube
same_content [keys %h], [@f], "All faces rolled after 10,000 tries";

my $cb=Cube_Bag->new($red,$blue,$green,$black);
$regex='['.join('',sort {$a cmp $b} $red->showing(),$blue->showing(),$green->showing(),$black->showing()).']';
isa_ok( $cb, 'Cube_Bag' );
my @cbf=$cb->showing();
is( scalar(@cbf), 4, "four cubes in Cube_Bag" );
for (@cbf) { like( $_, qr{$regex}, "$_ is one of the faces" ) }
isa_ok( $cb->roll(), 'Cube_Bag' );
%h=();
for (1..10000) { $h{$_}=1 for ($cb->roll()->showing()) }   # 10,000 rolls of four cubes in Cube_Bag
same_content [keys %h], [::unique $red->faces(),$blue->faces(),$green->faces(),$black->faces()], 
  "All faces in Cube_Bag rolled after 10,000 tries";

done_testing;
