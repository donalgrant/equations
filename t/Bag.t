#!/usr/bin/env perl

use strict;
use warnings;
use feature qw( say switch );
use Test::More;
use Data::Dumper;

BEGIN {
  use lib '/Users/imel/gitdev/equations/lib';
  use_ok 'Bag';
}

sub same_content($$$) {
  my $ha=shift;
  my $hb=shift;
  my $msg=shift;
  is_deeply( [sort { $a cmp $b } @$ha], [sort { $a cmp $b } @$hb], $msg );
}

my @bag_methods=qw( new empty list set copy 
		    remove remove_all add union
		    n count_is count_gt count_lt 
		    contains equals matches singletons multiples
		 );
can_ok( 'Bag', @bag_methods );

my $Q = new Bag;
same_content [ $Q->list() ], [ ], "constructor for empty bag";
is( $Q->empty(), 1,               "bag is empty" );

my @b=(qw( a b c d a b c c c ));
my $B = new Bag(@b);

isa_ok( $B, 'Bag' );

same_content [ $B->list() ], [ qw( a a b b c c c c d ) ], "bag";
same_content [ $B->set()  ], [ qw( a b c d ) ],           "set";

my $C=$B->copy();
isa_ok( $C, 'Bag' );

same_content [ $B->list() ], [ qw( a a b b c c c c d ) ], "copy";

$C->remove(qw(a b b));
same_content [ $B->list() ], [ qw( a a b b c c c c d ) ], "original unchanged";
same_content [ $C->list() ], [ qw( a c c c c d ) ],       "items removed";

$C->remove_all(qw(c d));
same_content [ $C->list() ], [ qw( a ) ],                 "items removed";

$C->remove(qw( b ));
same_content [ $C->list() ], [ qw( a ) ],                 "non-element removal has no effect";

$C->remove_all(qw( c ));
same_content [ $C->list() ], [ qw( a ) ],                 "non-element remove_all has no effect";

$C->add(qw( a a ));
same_content [ $C->list() ], [ qw( a a a ) ],             "add two elements with pre-existing key";

$C->add(qw( b b b ));
same_content [ $C->list() ], [ qw( a a a b b b ) ],       "add three elements which didn't pre-exist";

my $h={ a=>2, b=>2, c=>4, d=>1 };
my $D = new Bag($h);
isa_ok( $D, 'Bag' );
same_content [ $D->list() ], [ qw( a a b b c c c c d ) ], "constructed from hashref";

my $q=[ qw( a a b b c c c c d ) ];
my $E = new Bag($q);
isa_ok( $E, 'Bag' );
same_content [ $E->list() ], [ qw( a a b b c c c c d ) ], "constructed from arrayref";

is( $E->n(), 9,    "correct n in bag" );
is( $E->n('c'), 4, "correct n for an element" );
is( $E->n('e'), 0, "correct n for missing element" );

$E->remove_all(qw(a b c d));
ok( $E->empty(),   "Empty bag" );

$E->add(qw(a b))->add(qw(a b c d))->remove_all(qw(a))->remove(qw(c))->remove(qw(e));
same_content [ $E->list() ], [ qw( b b d ) ],         "chaining adds and removes";
same_content [ $E->copy()->remove(qw(b))->list() ], [ qw( b d ) ], "chained copy";
same_content [ $E->list() ], [ qw( b b d ) ],          "chained copy leaves original unaltered";

$E->add(qw(e f f f g h));
same_content [ $E->singletons() ], [ qw( d e g h ) ], "singletons";
same_content [ $E->multiples() ],  [ qw( b f ) ],     "multiples"  ;
same_content [ $E->count_is(2) ],  [ qw( b ) ],       "pairs via count_is";

same_content [ $E->count_gt(2) ],  [ qw( f ) ],       "count_gt";
same_content [ $E->count_lt(3) ],  [ qw( b d e g h ) ], "count_lt";

same_content [ $E->count_is() ],   [ qw( ) ],  "count_is with null arg";
same_content [ $E->count_gt() ],   [ qw( b d e f g h ) ], "count_gt with null arg";
same_content [ $E->count_lt() ],   [ qw( ) ],  "count_lt with null arg";

my $F = Bag->new( qw( a b c ) );
$F->add($F);

same_content [ $F->list() ], [ qw( a b c a b c ) ], "add";
same_content [ $F->copy()->remove(Bag->new(qw(a b)))->list() ],     [ qw( c a b c ) ], "remove";
same_content [ $F->copy()->remove_all(Bag->new(qw(a b)))->list() ], [ qw( c c ) ],     "remove_all";

my $G = new Bag( [ qw( b b b ) ] );
same_content [ $F->copy()->union($G->list())->list() ], [ qw( a a b b b c c ) ], "union(Array)";
same_content [ $F->copy()->union($G)->list() ],     [ qw( a a b b b c c ) ], "union(Bag)";

same_content [ $F->copy()->intersect($G->list())->list() ], [ qw( b b ) ], "intersect(Array)";
same_content [ $F->copy()->intersect($G)->list() ],         [ qw( b b ) ], "intersect(Bag)";

is( $G->contains($G->list()), 1, "containing self" );
is( $G->contains($G),     1, "contain self" );
is( $G->contains(qw( b )),    1, "containing subset" );
is( $G->contains(Bag->new(qw( b ))), 1, "contain subset" );
is( $G->contains(qw( a b )),  0, "contains disjoint set" );
is( $G->contains(Bag->new(qw( a b ))), 0, "contains disjoint set" );
is( $G->contains($G->copy()->add($G)->list()), 0, "twice self not contained" );
is( $G->contains($G->copy()->add($G)), 0,     "twice self contains not" );

$G->add(qw( a ));
is( $G->equals($G->list()), 1, "equals identity" );
is( $G->equals($G),     1, "equals identity" );
is( $G->equals($G->copy()->remove(qw( b ))->list()), 0, "equals on strict subset" );
is( $G->equals($G->copy()->remove(qw( b ))),     0, "equals on strict subset" );
is( $G->equals($G->copy()->add($G)->list()),     0, "equals on superset" );
is( $G->equals($G->copy()->add($G)),             0, "equals on superset" );

is( $G->matches($G->list()), 1, "matches identity list" );
is( $G->matches($G->set()),  1, "matches identity set" );
is( $G->matches($G),     1, "matches identity list" );

is( $G->matches($G->copy()->remove(qw(a))->list()),     0, "matches to missing key" );
is( $G->matches($G->copy()->remove(qw(b))->list()),     1, "matches to strict subset with all keys" );
is( $G->matches($G->copy()->remove_all(qw(b))->list()), 0, "matches to removed key" );

is( $G->matches($G->copy()->remove(qw(a))),         0, "matches to missing key" );
is( $G->matches($G->copy()->remove(qw(b))),         1, "matches to strict subset with all keys" );
is( $G->matches($G->copy()->remove_all(qw(b))),     0, "matches to removed key" );

my $H=Bag->new($G);
is( $H->equals($G), 1, "constructor from Bag" );

my $x=$H->random_item();
ok( $H->contains($x), "Got random_item ($x)" );

same_content Bag::listref_from_any(qw(a b)),            [qw(a b)],     "listref_from_any(array)";
same_content Bag::listref_from_any([qw(a b)]),          [qw(a b)],     "listref_from_any(arrayref)";
same_content Bag::listref_from_any({ a=>1, b=>2 }),     [qw(a b b)],   "listref_from_any(hashref)";
same_content Bag::listref_from_any(Bag->new(qw(a b))),  [qw(a b)],     "listref_from_any(Bag)";

same_content Bag::listref_from_any(Bag->new(qw(a b)),[qw(b c)],qw(f g h),{ c=>2, d=>1, e=>0 }),
  [qw( a b b c c c d f g h )],  "listref_from_any(mixed args)";
				   
done_testing;
