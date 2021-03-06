#!/usr/bin/env perl

use strict;
use warnings;
use feature qw( say switch );
use Test::More;
use Test::Exception;
use Data::Dumper;

BEGIN {
  use lib '/Users/imel/gitdev/donalgrant/equations/lib';
  use_ok 'RPN';
}

sub same_content($$$) {
  my $ha=shift;
  my $hb=shift;
  my $msg=shift;
  is_deeply( [sort { $a cmp $b } @$ha], [sort { $a cmp $b } @$hb], $msg );
}

my @rpn_methods=qw( new new_from_aos value display valid_rpn valid_aos aos_to_rpn rpn_to_aos 
		    full_parens parens_on_ops process_inner_parens parens_for_op );

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

is( RPN::rpn_to_aos('76+'), '7+6', "simple rpn_to_aos conversion" );
is( RPN::rpn_to_aos('98+7-6*5/2^'), '((((9+8)-7)*6)/5)^2', "longer rpn_to_aos conversion" );

is( $rpn->aos(), '7+6', "aos version" );

is_deeply( [ RPN::parens_for_op('@^',qw( 7 ^ 2 )) ], [qw( (7^2) )], "parens_for_op max precedence" );
is_deeply( [ RPN::parens_for_op('*/',qw( 7 / 2 )) ], [qw( (7/2) )], "parens_for_op med precedence" );
is_deeply( [ RPN::parens_for_op('+-',qw( 7 - 2 )) ], [qw( (7-2) )], "parens_for_op low precedence" );

is_deeply( [ RPN::parens_for_op('@^',qw( 7 ^ 2 + 3 * 4 )) ], [qw( (7^2) + 3 * 4 )], "parens_for_op max precedence mixed" );
is_deeply( [ RPN::parens_for_op('*/',qw( 7 / 2 + 3 * 4 )) ], [qw( (7/2) + (3*4) )], "parens_for_op med precedence mixed" );
is_deeply( [ RPN::parens_for_op('+-',qw( 7 - 2 + 3 * 4 )) ], [qw( ((7-2)+3) * 4 )], "parens_for_op low precedence mixed" );


is_deeply( [ RPN::find_inner_parens(split('','7 + (6*(2+3)) + (3/4) - 2')) ], [ 7, 11 ], "outer parens location" );
is_deeply( [ RPN::find_inner_parens(split('','7 + (6* 2+3)) + (3/4) - 2')) ], [ 4, 11 ], "unbalanced parens not detected" );
is_deeply( [ RPN::find_inner_parens(split('','7 +  6* 2+3   +  3/4  - 2')) ], [ -1, -1 ], "no parens" );
is_deeply( [ RPN::find_inner_parens(split('','7 +  6* 2+3)  +  3/4  - 2')) ], [ undef, undef ], "unbalanced parens inside" );

is( RPN::full_parens(' ( 7 ) '),           '7',                 "full parens example -3 (fixes redundant parens)"); 
is( RPN::full_parens(' 7 +  '),            '7+',                "full parens example -2 (ignore unbalanced expressions)"); 
is( RPN::full_parens('+'),                 '+',                 "full parens example -1 (ignore invalid expressions)");
is( RPN::full_parens('7'),                 '7',                 "full parens example 0");
is( RPN::full_parens('7 + 6'),             '(7+6)',             "full parens example 1");
is( RPN::full_parens('7 + 6 * 2'),         '(7+(6*2))',         "full parens example 2");
is( RPN::full_parens('7 + 6 * 2 ^ 1 @ 4'), '(7+(6*((2^1)@4)))', "full parens example 3");
is( RPN::full_parens('(7+6)'),             '(7+6)',             "full parens example 4");
is( RPN::full_parens('7+(6*2)'),           '(7+(6*2))',         "full parens example 5");
is( RPN::full_parens('(7+6)*2'),           '((7+6)*2)',         "full parens example 6");
is( RPN::full_parens('(7+6*2)'),           '(7+(6*2))',         "full parens example 7");
is( RPN::full_parens('(7+3*2)+2*3'),       '((7+(3*2))+(2*3))', "full parens example 8");
is( RPN::full_parens('(7+3)*(2+2)*3'),     '(((7+3)*(2+2))*3)', "full parens example 9");

is( RPN::aos_to_rpn('7'),   '7',   "aos conversion to rpn for a single number" );
is( RPN::aos_to_rpn('7+'),  undef, "aos conversion to rpn on invalid aos string is undefined" );
is( RPN::aos_to_rpn('7+6'), '76+', "aos converted to rpn" );

is( RPN::aos_to_rpn('(((((9+8)-7)*6)/5)^2)'), '98+7-6*5/2^', 'convert aos to rpn on fully parethesized expression' );

is( RPN::aos_to_rpn($rpn->aos()), "$rpn", "identity operation for AOS->RPN->AOS" );

is( RPN::valid_aos($rpn->aos()), 1, "aos() is valided as aos" );

my $aos='  (5 +4) / (3+ 2  ) ^ (1@2)  ';
my $rpn_version='54+32+12@^/';

my $rpn_str="$rpn";
my $original_aos=$aos;
my $original_rpn="$rpn";

is( RPN::aos_to_rpn($aos), $rpn_version, "Correct handling of spaces" );
  
is( RPN::valid_aos($aos), 1, "$aos is aos");
is( RPN::valid_rpn($rpn), 1, "RPN $rpn is rpn");
is( RPN::valid_rpn($rpn_str), 1, "$rpn_str is rpn");
is( RPN::valid_aos($rpn), 0, "RPN $rpn is not aos");
is( RPN::valid_aos($rpn_str), 0, "$rpn_str is not aos");
is( RPN::valid_rpn($aos), 0, "$aos is not rpn");

is( $rpn_str, $original_rpn, "No change to str by valid_* functions");
is( $aos,     $original_aos, "No change to aos by valid_* functions");

$rpn=RPN->new_from_aos($aos);
is( "$rpn", $rpn_version, "construction of RPN from aos" );

$aos=' 3 + 2 - 5 / 6 ';
is( RPN::full_parens($aos), '((3+2)-(5/6))', "full_parens" );

is( RPN::valid_aos('(1+(4+3)/-4)'),    0, "outlier case for aos validation" );
is( RPN::valid_aos('()()()1+2'),       0, "outlier case for aos validation:  need for paren-content check" );

# extra parens cases
$aos='((1+3))+((2-2))';
is( RPN::valid_aos($aos),   1,               "aos validation:  allow extra parens" );
is( RPN::full_parens($aos), '((1+3)+(2-2))', "extra parens fixed by full parens" );
is( RPN::aos_to_rpn($aos),  '13+22-+',       "extra parens in aos ok for conversion to rpn" );

is( RPN::valid_aos('7'),               1, "outlier case for aos:  a single number" );

done_testing;
