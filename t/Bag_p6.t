#!/usr/bin/env perl6

use v6;

use Test;

# plan 26;

sub bag-content-is ( BagHash $b, List $l, $msg ) {
  is-deeply $b.kxxv.sort.list, $l.sort, $msg;
}

my $Q = BagHash.new;

is $Q.total, 0,                                "new BagHash is empty";

my $b = BagHash.new: qw<a b c d a b c c c>;

is $b.^name, 'BagHash',                        "Created BagHash with content";
bag-content-is $b, qw<a a b b c c c c d>,      "bag contents confirmed";
is-deeply $b.keys.sort, qw<a b c d>,           "bag.keys gives set equivalent";

my $c=$b.kxxv.BagHash;
is-deeply $b, $c,                              "bag copy";
bag-content-is $c, qw<a a b b c c c c d>,      "copied bag contents confirmed";

$c<a>--; $c<b>-=2;
bag-content-is $c, qw<a c c c c d>,            "elements removed from copied bag";
bag-content-is $b, qw<a a b b c c c c d>,      "original bag contents unchanged";
					       
$c<c>=0;  $c<d>=0;			       
bag-content-is $c, qw<a>.list,                 "complete removal of items from copied bag";
					       
$c<b>--;				       
bag-content-is $c, qw<a>.list,                 "non-element removal has no effect";
$c<c>=0;				       
bag-content-is $c, qw<a>.list,                 "non-element complete removal has no effect";
					       
$c<a>++; $c<a>++;			       
bag-content-is $c, qw<a a a>,                  "add elements with pre-existing key";
					       
$c<b>=3;				       
bag-content-is $c, qw<a a a b b b>,            "add three elemens which didn't pre-exist";

my %h = ( a=>2, b=>2, c=>4, d=>1 );
my $D = %h.BagHash;
bag-content-is $D, qw<a a b b c c c c d>,      "construct a BagHash from a hash";

my @q = <a a b b c c c c d>;
my $E = @q.BagHash;
bag-content-is $E, qw<a a b b c c c c d>,      "construct a BagHash from an array";

is $E.total, 9,                                "correct number of items in bag";
is $E<c>,    4,                                "correct count for an element";
is $E<e>,    0,                                "correct count for missing element";

for (qw<a b c d>) { $E{$_}=0 }
is $E.total, 0,                                "Bag empty after removing all elements";

$E<a>++; $E<b>++; $E{$_}++ for qw<a b c d>; $E<a>=0; $E<c>=0; $E<e>=0;
bag-content-is $E, qw<b b d>,                 "Sequence of adds and removes";

$E{$_}++ for qw<e f f f g h>;
is-deeply $E.pairs.grep( *.value==1 ).map(*.key).sort, qw<d e g h>,      "singletons";
is-deeply $E.pairs.grep( *.value >1 ).map(*.key).sort,  qw<b f>,         "multiples";
is-deeply $E.pairs.grep( *.value==2 ).map(*.key).sort,  qw<b>.list,      "doubles";
is-deeply $E.pairs.grep( *.value >2 ).map(*.key).sort,  qw<f>.list,      "count gt 2";
is-deeply $E.pairs.grep( *.value <3 ).map(*.key).sort,  qw<b d e g h>,   "count lt 3";

my $G=BagHash.new: qw<a b c>;
$G = ($G (+) $G).BagHash;

bag-content-is $G, qw<a b c a b c>,                                      "duplicate contents";
bag-content-is ($G (|) qw<b b b>.BagHash).BagHash, qw<a b c a b c b>,    "union of two bags";
bag-content-is ($G (&) qw<b b b>.BagHash).BagHash, qw<b b>,              "intersection of two bags";
is ( $G (<=) $G ), True,                                                 "bag is subset of itself";
is ( $G (>=) qw<a b>.BagHash ), True,                                    "items in bag are subset of the bag";
is ( $G (>=) qw<b b d>.BagHash ), False,                                 "Disjoint set is not a subset";
is ( $G (<=) qw<b b d>.BagHash ), False,                                 "Disjoint set is not a subset either way";

my $H = $G.kxxv.BagHash;  # make a copy?
is $G== $H, True,                                                        "Copy of BagHash should be the same";
is $G===$H, False,                                                       "...But not the exact same object";



done-testing;	 
