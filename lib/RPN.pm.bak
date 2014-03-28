package RPN;

use strict;
use warnings;
use feature qw( say switch );

use Globals;

=head1 NAME

RPN.pm - Handle calculations for Equations Game

=head1 DESCRIPTION

Functions for the RPN Calculator:

   * Calculate value from RPN string
   * Cache RPN values calculated

=cut

my %RPN_CACHE;   # maintained for all objects

# in profiling, this was faster than using given/when or $opssubs{$op}->($n1,$n2)

sub calc {
  my ($n1,$op,$n2)=@_;
  return $n1+$n2                                                   if $op eq '+';
  return $n1-$n2                                                   if $op eq '-';
  return $n1*$n2                                                   if $op eq '*';
  return ( ($n2==0)                     ? undef : $n1/$n2)         if $op eq '/';
  return $n1**$n2                                                  if $op eq '^';
  return ( ($n1==0 || ($n2<0 && $n1>0)) ? undef : $n2**(1.0/$n1) ) if $op eq '@';
  ::quit "Unrecognized operator:  $op";
}

sub new {
  my $class=shift;
  my $self={ _=>shift() };   # rpn string
  return bless $self, $class;
}

use overload '""' => \&display, '0+' => \&value, fallback => 1;

my %ops=map { ($_,$_) } (qw( + - * / ));  $ops{'^'}='**'; $ops{'@'}='**(1.0/';
my %num=map { ($_,1) } (0..9);

sub value {
  my $self=shift;
  my $rpn=$self->{_};
  return $RPN_CACHE{$rpn} if exists $RPN_CACHE{$rpn};
  my @list=split('',$rpn);
  return 0 unless (@list);
  my @stack;
  my $bos;
  while (@list && push @stack, $bos=shift @list) {
    next if exists $num{$bos};  # fastest way to do matching -- much better than =~/\d/
    my $op=pop @stack;
    my $n2=pop @stack;
    my $n1=pop @stack;
    my $v=calc($n1,$op,$n2);
    return $RPN_CACHE{$rpn}=undef unless defined $v;
    push @stack, $v;
  }
  return $RPN_CACHE{$rpn}=shift @stack;
}

# eventually construct parenthesized expression here
sub display {
  my $self=shift;
  return $self->{_};
}

sub list { my $self=shift; return split('',$self->{_}) }

1;
