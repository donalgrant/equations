use v6;

=head1 NAME

=begin pod

Globals.pm - Global functions useful to all modules and the main program

=end pod

=head1 DESCRIPTION

=begin pod

Global functions should be referenced with the full package qualifier, in order 
to make it obvious that these are defined outside the current file / module.

Not implemented yet, but this should include any Global data, particularly
an %::opt hash of execution options.

=end pod

=head2 Execution Functions

=begin pod

  * caller_list()                       - for diagnostics, returns list of calling subroutines
  * msg  "message text", "source"       - display a message ("source" argument is optional)
  * quit "message", "source"            - quit with a message ("source argument is optional)
  * assert { test_code } "test message" - like Test::ok(), but quits on failure and operates quietly.

=end pod

=head2 Array Operations

=begin pod

  * unique(@)        - Built-in to Perl 6
  * shuffle_in_place - shuffles an array in-place -- no return value
  * shuffle(@)       - makes a copy of the list, randomly reorder elements of the list copy, 
                       and returns the re-ordered list copy.  Original remains unchanged.
  * min(@)           - Built-in to Perl 6
  * max(@)           - Built-in to Perl 6

=end pod

module Globals {

sub caller() is export { Backtrace.new.list }

sub msg($txt='[ Undefined text ]', $src?, :$trace) is export { 
  my $out=$txt;
  $out ~= "[from $src]" if $src;
  $out ~= caller() if $trace;
  put($out); 
}  

sub quit($txt='[ Undefined text ]') is export { msg($txt, :trace); die() }

sub assert(Block $test, $msg='[ Unspecified assertion ]') is export {
  quit('FAIL***'~$msg) unless so $test();
}

sub shuffle_in_place ( @array ) is export {
  for 0..@array.end-1 { @array[ $_, ($_+1..@array.end).pick ] .= reverse }
}
sub shuffle ( @array ) is export { my @c=@array; shuffle_in_place @c; return @c }
  
}
