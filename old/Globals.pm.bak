package main;

use strict;
use warnings;
use feature qw( say switch );

=head1 NAME

Globals.pm - Global functions useful to all modules and the main program

=head1 DESCRIPTION

Global functions should be referenced with the full package qualifier, in order 
to make it obvious that these are defined outside the current file / module.

Not implemented yet, but this should include any Global data, particularly
an %::opt hash of execution options.

=cut

=head2 Execution Functions

  * ::caller_list()                       - for diagnostics, returns list of calling subroutines
  * ::msg  "message text", "source"       - display a message ("source" argument is optional)
  * ::quit "message", "source"            - quit with a message ("source argument is optional)
  * ::assert { test_code } "test message" - like Test::ok(), but quits on failure and operates quietly.

=cut

sub caller_list {
  my @list;
  my $level=0;
  while (my $src_at_level=( ( caller(++$level) )[3] )) { 
    push @list,$src_at_level;
  }
  return @list;
}

# second argument to ::msg is the message source info.  If missing, then
# ::msg will assume just the calling subroutine

sub msg {                                                              # send messages to STERR and log
  my $txt=shift() // '[ Undefined text ]';
  my $src=shift() // ${ [::caller_list()] }[1] // $0;
  my $out="$txt [from $src]";
  print STDOUT $out,"\n";  # if $::opt{verbose}; 
}  

# second argument to quit is source info, passed along to ::msg
# if missing, it will pass the calling subroutine for ::quit (or the calling sequence with -debug set)

sub quit(;$$) { 
  my $txt=shift() // '[ Undefined text ]';
  my $src=shift() // ${ [::caller_list()] }[1] // $0;
#  $::opt{verbose}=1; 
  ::msg $txt, $src; 
  die; 
} 

# is there a way to capture the text of the code?
sub assert(&$) { 
  my $test=shift; 
  my $msg=shift() // '[ Unspecified assertion ]';
#  my $src= $::opt{verbose} ? join(',',@{ [::caller_list()] }) // $0 : ${ [::caller_list()] }[1] // $0;
  my $src=join(',',@{ [::caller_list()] }) // $0;
  if (&$test) { } # ::msg  '(PASS) '.$msg, $src if $::opt{debug} }
  else        { ::quit 'FAIL***'.$msg, $src }
}

=head2 Array Operations

  * ::unique(@)        - return a copy of the list argument where the multiple copies have been removed
  * ::shuffle_in_place - shuffles an array in-place -- no return value
  * ::shuffle(@)       - makes a copy of the list, randomly reorder elements of the list copy, 
                         and returns the re-ordered list copy.  Original remains unchanged.
  * ::min(@)           - minimum value in list
  * ::max(@)           - maximum value in list

=cut

sub unique(@)  { my %h=(); grep { !$h{$_}++ } @_ }

sub shuffle_in_place(@) {
  for my $i (0..$#_-1) {
    my $r=int(rand()*($#_-$i));
    ($_[$i],$_[$r+$i])=($_[$r+$i],$_[$i]);
  }
  return undef;   # make it clear that this is in place -- no return value
}

sub shuffle(@) { my @c=@_; ::shuffle_in_place @c; return @c }

sub max { my $m=shift; for (@_) { $m=($_>$m)?$_:$m } return $m }
sub min { my $m=shift; for (@_) { $m=($_<$m)?$_:$m } return $m }
1;
