#!/usr/bin/perl

use feature qw( say switch );

use strict;
use warnings;

use lib '/Users/imel/Desktop/Dropbox/dev/equations/lib';
use Bag;

sub unique(@) { return keys %{{ map { $_=>undef } @_ }} }

sub calc {
  my $n1=shift;
  my $op=shift;
  my $n2=shift;
  given ($op) {
    when('+') { return $n1+$n2 }
    when('-') { return $n1-$n2 }
    when('*') { return $n1*$n2 }
    when('/') { return $n1/$n2 }
    when('^') { return $n1**$n2 }
    when('@') { return $n2**(1.0/$n1) }
    default   { die "Unrecognized operator:  $op" }
  }
}

package Cube;

sub new {
  my $class=shift;
  my $self={};
  $self->{faces}=[ @_ ];
  bless $self, $class;
  $self->roll();
  return $self;
}

sub roll {
  my $self=shift;
  my $n=scalar(@{$self->{faces}});
  $self->{showing}=$self->{faces}[int(rand()*$n)];
}

sub showing { my $self=shift; return $self->{showing} }

package Red_Cube;  
push @Red_Cube::ISA, qw( Cube );
sub new { my $class=shift; return $class->SUPER::new('0','1','2','3','+','-') }

package Blue_Cube;  
push @Blue_Cube::ISA, qw( Cube );
sub new { my $class=shift; return $class->SUPER::new('0','1','2','3','*','/') }

package Green_Cube;  
push @Green_Cube::ISA, qw( Cube );
sub new { my $class=shift; return $class->SUPER::new('4','5','6','^','/','-') }

package Black_Cube;  
push @Black_Cube::ISA, qw( Cube );
sub new { my $class=shift; return $class->SUPER::new('7','8','9','/','-','@') }

package Cube_Bag;
sub new     { my $class=shift; return bless { dice=>[ @_ ] }, $class }
sub roll    { my $self=shift;  for (@{$self->{dice}}) { $_->roll() } }
sub showing { my $self=shift;  return map { $_->showing() } @{$self->{dice}} }
sub unique  { my $self=shift;  return ::unique $self->showing() }

package main;

use Data::Dumper;

my %cache=();

my $count=0;

sub process_cubes {
  my $b0=shift;
  my @list=sort { $a cmp $b } $b0->list();
  my $cache_key=join(',',@list);
  return @{ $cache{$cache_key} } if exists $cache{$cache_key};
  my $nums=new Bag @{ [ grep /\d/, @list ] };
  my $ops =new Bag @{ [ grep m{^[-+*/@^]$}, @list ] };
  return @{ $cache{$cache_key}=[ $nums->set() ] } if $ops->empty() || $nums->n()<2; # stop unless op & pair of nums
  my @nums=$nums->set();
  my (@result)=@nums;                                    # can always use numbers without further calculation
  # need to replace outer two loops with a loop over pairs, with duplicates only if there is a multiple in the bag
  for my $i1 (0..$#nums) {
    for my $i2 ($i1..$#nums) {
      next if $i1==$i2 && $nums->n($nums[$i1])<2;  # n1 op n1 not available
      for my $op ($ops->set()) {
	my $b=$b0->copy()->delete($nums[$i1],$nums[$i2],$op);          # generate residual bag of cubes with these three out
	unless ( ($nums[$i2]==0 && $op eq '/') || ($nums[$i1]==0 && $op eq '@') ) {    # no divide by zero or 0th root
	  my @l1=process_cubes($b->copy()->add(::calc($nums[$i1],$op,$nums[$i2])));    # first order:  n1 op n2
	  push @result,::unique(@l1); 
	}
	next if ($op eq '+') || ($op eq '*');            # commutative operators
	next if $i1==$i2;                                # symmetric expression
	unless ( ($nums[$i1]==0 && $op eq '/') || ($nums[$i2]==0 && $op eq '@') ) {    # same as above, but for
	  my @l2=process_cubes($b->copy()->add(::calc($nums[$i2],$op,$nums[$i1])));    # second order:  n2 op n1
	  push @result,::unique(@l2);
	}
      }
    }
  }
#  say "process_cube count ",++$count,": ",join(',',@list),' gives ',join(',',@result);
  return @{ $cache{$cache_key}=[ ::unique(@result) ] };
}

my @c=map { new Red_Cube   } (1..3);
my @d=map { new Blue_Cube  } (1..3);
my @e=map { new Green_Cube } (1..3);
my @f=map { new Black_Cube } (1..3);

my $B = new Cube_Bag (@c,@d,@e,@f);
$B->roll();

say "showing:  ",$B->showing();
say "unique:   ",$B->unique();

my @list=process_cubes(Bag->new([$B->showing()]));
say "Final list:  ",join(' ',map { sprintf('%0.3f',$_) } sort { $a <=> $b } @list);

say "cache:";
for (keys %cache) { say "$_ -> ",join(' ',@{ $cache{$_} }) }

=pod

improve cacheing:

instead of stepping through three cubes at a time, move through two cubes at a time:  an operator and a number:

result list with associated number
choose op and number combinations

generate two new results for each (unless symmetric or divide-by-zero)

Symmetric:  a + b, a * b, a op a
Divide by zero:  a / 0, 0 Root a

Cache result list -- but order is significant  (in effect, use reverse polish notation for cubes in list)

Result is a string of cubes and operators in rpn, with an associated value
Then it's just a matter of getting allowed permutations of an rpn string:
  1) Must always have at least one more number than operator when reading string from left to right
  2) For the complete string, must have exactly one more number than operators

numbers:  abcde
ops:      mnop

abm, bam, abn, ban, abo, bao, abp, bap, ac/ca..., ad/da..., ae/ea..., bc/cd..., bd/db..., be/eb..., ..., de/ed...

now add a number and an op:
abmcn, cabmn, 
