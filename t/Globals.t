#!/usr/bin/env perl

use strict;
use warnings;

use Test::More;
use Test::Exception;

BEGIN {
  use lib '/Users/imel/gitdev/equations/lib';
  use_ok 'Globals';
}

lives_ok { ::caller_list() } "caller_list";

lives_ok { ::msg "test message" } "test message";
lives_ok { ::msg "test", "src"  } "test message with source";

dies_ok  { ::quit "quit test"   } "quit test";
dies_ok  { ::quit "quit", "src" } "quit test with source";

lives_ok { ::assert { 1 } "pass test" } "assert with pass";
dies_ok  { ::assert { 0 } "fail test" } "assert with fail";

is_deeply( [ ::unique(qw( a b b b c c )) ], [ qw( a b c ) ], "::unique" );

my @list=qw( a b c d e 1 2 3 4 5 );
my @original_list=@list;
my @shuffled=::shuffle @list;  # make a copy

is( scalar(@list), scalar(@shuffled), "shuffle preserves array length" );
is_deeply( [ sort { $a cmp $b } @shuffled ], [ sort { $a cmp $b } @list ], "shuffle retains all elements" );
my $same=1;
for my $i (0..$#list) { $same &&= ($list[$i] eq $shuffled[$i]) }
isnt( $same, 1, "shuffle altered the order of the list.  (Might not always happen, but fail should be very rare.)" );
$same=1;
for my $i (0..$#list) { $same &&= ($original_list[$i] eq $list[$i]) }
is( $same, 1, "shuffle didn't alter the order of original list." );


is( min(5,9,3,234,-2,0,3), -2,   "min on array" );
is( max(5,9,3,234,-2,0,3), 234,  "max on array" );


done_testing;
