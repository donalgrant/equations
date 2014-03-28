#!/usr/bin/env perl

use strict;
use warnings;
use feature qw( say switch );
use Test::More;
use Test::Exception;
use Data::Dumper;

BEGIN {
  use lib '/Users/imel/gitdev/equations/lib';
  use_ok 'RPN';
}

sub same_content($$$) {
  my $ha=shift;
  my $hb=shift;
  my $msg=shift;
  is_deeply( [sort { $a cmp $b } @$ha], [sort { $a cmp $b } @$hb], $msg );
}

my @rpn_methods=qw( new value display );

can_ok( 'RPN', @rpn_methods );
	
my $rpn=new RPN "76+"; 
isa_ok($rpn,'RPN');

is( $rpn->value(), 13, "simple addition" );

is( RPN->new("76+")->value(), 13, "cached" );
is( RPN->new('76*')->value(), 42, "simple multiplication" );
is( RPN->new('82/')->value(), 4,  "simple division" );
is( RPN->new('76-')->value(), 1,  "simple subtraction" );
is( RPN->new('38@')->value(), 2,  "roots" );
is( RPN->new('25^')->value(), 32, "exponent" );

is( RPN->new('98+7-6*5/2^')->value(), 144, "chain calculations" );
is( RPN->new('55555+-/*')->value(),    -5, "stacked operations" );
		
same_content [ RPN->new('55555+-/*')->list() ], [qw( + - / * 5 5 5 5 5 )], "RPN list";
	   
is( $rpn+0, 13, "operator overloading for value" );
cmp_ok( $rpn, '<', 20, "operator overloading for comparisons" );
is( "$rpn", "76+", "operator overloading for string" );

done_testing( 16 );
