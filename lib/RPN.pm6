use v6;

my $lib;
use Test;

BEGIN {
  $lib=q{/Users/imel/gitdev/donalgrant/equations/lib};
}

# use lib q{/Users/imel/gitdev/donalgrant/equations/lib};
use Globals;

=begin pod

=head1 NAME

RPN.pm - Handle calculations for Equations Game

=head1 DESCRIPTION

Functions for the RPN Module:

   * Calculate value from RPN string
   * Cache RPN values calculated
   * valid_rpn   - return true if string is a valid rpn string
   * valid_aos   - return true if string is a valid aos string
   * aos_to_rpn  - convert an aos-formatted string to an rpn string
   * rpn_to_aos  - convert an rpn-formatted string to an aos string
   * full_parens - add full parentheses to aos expression so that 
                   no reliance on operator precedence is needed for interpretation.

=end pod

class RPN {

  has $!rpn;

  my %RPN_CACHE;   # maintained for all objects

# in Perl5 profiling, this was faster than using given/when or $opssubs{$op}->($n1,$n2)

  sub calc {
    my ($n1,$op,$n2)=@_;
    return $n1+$n2                                                        if $op eq '+';
    return $n1-$n2                                                        if $op eq '-';
    return $n1*$n2                                                        if $op eq '*';
    return ( ($n2==0)                     ?? Nil !! $n1/$n2)              if $op eq '/';
    return $n1**$n2                                                       if $op eq '^';
    return ( ($n1==0 or ($n2 < 0 and $n1 > 0)) ?? Nil !! $n2**(1.0/$n1) ) if $op eq '@';
    quit "Unrecognized operator:  $op";
  }

=begin pod

=item valid_rpn

A string (with all spaces removed) is identified as a valid RPN string if it passes the following tests:

  * Final character in string is an operator
  * First two characters in string are numbers
  * Total number of operators must be exactly one less than the number of numbers
  * As the string is read from left to right, there must never be as many operators as numbers so far
  * Only operators and numbers are permitted (after all spaces have been removed)

This takes some time to implement, so this will not be used for all constructor calls, but only
when there is a question about the source of the input.  (I.e., from a human.)

=item valid_aos

A string (with all spaces removed) is identified as a valid AOS string if it passes the following tests:

  * All parentheses are balanced
  * Only operators, numbers, and parentheses are permitted
  * If the parentheses are ignored, the sequence of the expression must be (number operator)* number.  I.e.,
    it should start and finish with a number, and each pair of numbers must be separated by a single operator.
  * The above rules are true of every parenthesized sub-expression

=item aos_to_rpn

Examples:  ( (9+2) -8)^(3-1) ^ (6/2)   i.e., 3^(2^3) -->  92+8-31-62/^^
           ( (9+2) -8)^(3-1) / (6/3)   i.e., (3^2)/2 -->  92+8-31-^63// 
           ( (9+2) -8)@8/2             i.e., (3@8)/2 -->  92+8-8@2/
           ( (9+2) -8)@8@2             i.e., 3@(8@2) -->  92+8-82@@

I.e., exponentiation and radical take precedence to the right

  0.  Order of numbers is preserved
  1.  Order of operators is not necessarily preserved.  Can be seen as a placing
      ops on a stack, then popping one off the stack every time a close parens is encountered.
      This only works for a "fully-parenthesized" expression.


=item rpn_to_aos (via _rpn_array_to_pos) 

For example:  342/42++ --> (3+((4/2)+(4+2)))

The algorithm for converting an RPN string to an AOS string is:

  0.  Break the string into an array of strings, one element per character
  1.  Scan along the array until first operator is found -- capture that as the "center" of a new string:  '/'
  2.  Prior two characters must be numbers, since this is the first operator.  Add those before and
      after the operator, and surround with parens and (optional) white-space:  ' (4/2) '
  3.  Replace the three array elements with this single string.
  4.  Repeat from step 1 until only a single element remains.  This will be the AOS string.

The repeated operations 1-3 are accomplished by using the recursive _rpn_array_to_pos() function

=end pod

sub valid_rpn ($rpn) {
  $rpn.=trim;           # remove all white-space
  my ($ops,$num)=(0,0);
  for $rpn.comb {
    if    (m{\d})       { $num++ }
    elsif (m{<[\+\-\*\/\@\^]>}) { $ops++ }
    else { return False }
    return False if $ops>=$num;
  }
  return False unless $num>=2;
  return False unless $ops==$num-1;
  return True;
}

sub valid_aos ($aos) {
  $_=$aos.trim;
  return False if m{<[\^0..9\(\)\+\-\/\*\^\@]>};
  my $level=0;
  my ($last_open);
  my @c=$_.comb;
  s:g/<[\(\)]>//;                                            # ignoring parentheses...
  return False unless m{^(<[0..9]><[\-\+\*\/\@\^]>)*<[0..9]>$};  # ...should start and end with a number, and have op between
                                                           # ...alternating through the whole expression
  for ^@c -> $i {                                          # step through for paren balance and some content check
    if (@c[$i]~~/<[\(]>/) {
      ++$level;
      $last_open=$i;
    }
    elsif (@c[$i]~~/<[\)]>/) {
      --$level;
      return False if $level < 0;  # cannot close more expressions than are opened
      my $op_pos;
      for ($last_open+1)..($i-1) -> $j { $op_pos=$j if (@c[$j]~~m{ <[\-\+\*\/\^\@]> }) }
      return False unless defined $op_pos;  # must be an operator in every parenthesized expression
      return False unless grep m{\d}, @c[$last_open+1..$op_pos-1];  # need two arguments for every operator
      return False unless grep m{\d}, @c[$op_pos+1..$i-1];          # one on each side of the operator
      $_=join('',@c[$last_open+1..$i-1]);
      my $n=$_.comb(/<[0..9]>/).elems;
      my $o=$_.comb(/<[ \+ \- \* \/ \@ \^ ]> /).elems;
      return False unless $n-$o == 1;       # count nums and ops; always one more op than numbers
    }
  }
  return False if $level;      # must be balanced parens
  return True;
}

sub aos_to_rpn ($aos) {
  $_=$aos.trim;
  return Nil unless valid_aos($_);
  my @c=full_parens($_).comb;  # guarantees syntax for the rest of the conversion
  return @c[0] if +@c==1;      # special case of a single digit
  my (@ops,@num,@eq);
  for @c -> $c {
    if ($c~~m{<[0..9]>})       { push @num,$c }
    elsif ($c~~m{<[\-\+\*\/\@\^]>}) { push @ops,$c }
    elsif ($c~~m{<[\(]>})      { }  # nop
    elsif ($c~~m{<[\)]>})      { push @eq, @num, pop @ops; @num=() }
    else { quit "unrecognized character $c in aos" }
  }
  return @eq.join('');
}

# not quite sure what the best spacing and parentheses are

sub _rpn_array_to_aos {
  return @_[0] if +@_==1;
  my $i=2;
  while ( !(@_[$i]~~m{^<[\-\*\/\@\^\+]>$}) ) { $i++ }
  my $s1=(@_[$i-2]~~m{<[\)]>\s*$}) ?? '' !! '';  # for now, always no space
  my $s2=(@_[$i-1]~~m{^\s*<[\(]>}) ?? '' !! '';  # ...on both sides
  my $e="@_[$i-2]$s1@_[$i]$s2@_[$i-1]";
  my @x=@_.elems>3 ?? "($e)" !! $e;
  unshift @x, @_[0..$i-3] if $i > 2;
  push @x, @_[$i+1..@_.end]  if $i < @_.end;
  return _rpn_array_to_aos(@x);
}

# argument to rpn_to_aos must be guaranteed to be a valid RPN.  
# can use valid_rpn to ensure this if necessary

sub rpn_to_aos { return _rpn_array_to_aos(@_[0].comb) }

=begin pod

=item full_parens

Need "full_parens" to make conversion from aos to rpn work conveniently.  This functon
ensures that an aos expression is fully-parenthesized, i.e., anytime there is an operator,
there will be a matching set of opening and closing parentheses showing where the start
and end of the operator's operands are.  For example:
  
     7 + 6              ==>  (7+6)
     7 + 6 * 2          ==>  (7+(6*2))
     7 + 6 * 2 ^ 1 @ 4  ==>  (7+(6*((2^1)@4)))

This function enforces operator precedence, from lowest to highest:

     Group 1:  + -
     Group 2:  * /
     Group 3:  ^ @

Within each group, the precedence is left-to-right.  Existing parentheses are unchanged:

     (7+6)       ==> (7+6)
     7+(6*2)     ==> (7+(6*2))
     (7+6)*2     ==> ((7+6)*2)
     (7+6*2)     ==> (7+(6*2))
     (7+3*2)+2*3 ==> (((7+3)*2)+(2*3))

The groups can be tokenized:

    token operator token

where each token can be a number or another group.  full_parens() enforces surrounding every group
with a pair of '()'s, by parsing the groups into these tokens, recursively, and adding parentheses
to groups where they do not exist, while paying attention to operator precedence.

Algorithm:

   0. Remove white-space, and split every character into an aray.
   1. Find any pre-existing (un-processed) parentheses, and scan to the first completed parethetical expression.  
      I.e., if the expression is 7+(6*(3+2)-4*(1-4)), then the first completed expression will be (3+2).
      a.  In the process, if the expression is found to have unbalanced paretheses, then return undef for the expression
      b.  If there are no parentheses, proceed to step 4.
      c.  Inner expression is now guaranteed to have either no parentheses, or (eventually),
          previously-validated parenthetical expressions.
   2. Process current expression to enforce full-parentheses while enforcing operator precedence:
      a.  Find left-most (unprocessed) high-precedence operator (^,@), and parenthesize that sub-expression.
          Repeat until there are no more unprocessed high-precedence operators in the sub-expression.
      b.  Repeat for medium precedence operators (*,/).
      c.  Repeat for medium precedence operators (+,-).
   3. Return to step 1.
   4. Repeat step 2 one last time for the entire expression.

This is accomplished by continuously re-doing the array of expression elements.  Anytime an expression has
been "processed", the processed sub-expression becomes a single element in the array.  Thus, for the 
eexpression '7+(6*(3+2)-4*(1-4))', after step 2a, the current representation will be the array:

    7, +, (, 6, *, (, 3, +, 2, ), -, 4, *, (, 1, -, 4, ), )

After step 2c is completed for the first time:

    7, +, (, 6, *, (3+2), -, 4, *, (, 1, -, 4, ), )

After the returning to step 1, then executing through step 2c again:

    7, +, (, 6, *, (3+2), -, 4, *, (1-4), )

Another pass through step 1, we get after the first part of 2b:

    7, +, (, (6*(3+2)), -, 4, *, (1-4), )

By the time this iteration of 2b is completed, we have:

    7, +, (, (6*(3+2)), -, (4*(1-4)), )

And once step 2c is completed:

    7, +, ((6*(3+2))-(4*(1-4)))

the next iteration of step 1 finds no "unprocessed inner parentheses", and continues to step 4, i.e.,
the final execution of step 2.  After 2c, we have the final result:

    (7+((6*(3+2))-(4*(1-4))))

which is the fully parenthesized expression.

=end pod

sub find_inner_parens (@c) {
  my $pstart;
  my $level=0;
  for ^@c -> $i { 
    if (@c[$i] eq '(') {
      $pstart=$i;
      $level++;
    }
    elsif (@c[$i] eq ')') {
      $level--;
      return (Nil,Nil) if $level < 0;  # unbalanced parens
      return ($pstart,$i);           # balanced inner parens found
    }
  }
  return (Nil,Nil) if $level;        # unbalanced parens
  return (-1,-1);                    # no parens
}

# argument is array of elements with no parens

sub parens_for_op {
  my $op_regex='^<[\\'~shift(@_)~']>$';
  my @c=@_;
  while (grep m{$op_regex}, @c) {  # single element operator, highest precedence
    my $i=0; while (!(@c[$i]~~m{$op_regex})) { $i++ }  # guaranteed to find one here
    my @r;
    for 0..$i-3 { push @r,@c[$_] }
    push @r,@c[$i-2] unless ($i-2 < 0     ) || (@c[$i-2] eq '(');
    push @r, join('',|('(',@c[$i-1..$i+1],')'));
    push @r,@c[$i+2] unless ($i+2 > @c.end) || (@c[$i+2] eq ')');
    for $i+3..@c.end { push @r, @c[$_] } 
    @c=@r;
  }
  return @c;
}

sub parens_on_ops {
  my @c=@_;
  @c=parens_for_op('@^',@c);
  @c=parens_for_op('*/',@c);
  @c=parens_for_op('+-',@c);
  return @c;
}

sub process_inner_parens(@c) {
  while (my ($pstart,$pstop)=find_inner_parens(@c)) {
    last if $pstart < 0;
    return Nil unless ~$pstart;
    my $nc=@c.elems;
    my @before = ($pstart > 0)    ?? @c[0..$pstart-1]    !! ();
    my @after  = ($pstop < $nc-1) ?? @c[$pstop+1..$nc-1] !! ();
    if ($pstop-$pstart==2) { # detected redundant parens -- single number or paren expression 
      @c=(@before,@c[$pstart+1],@after);  # maybe use splice here?
    } else {
      @c=(@before,join('',parens_on_ops(@c[$pstart..$pstop])),@after);
    }
  }
  return False;
}

sub full_parens ($s) {
  $s.=trim;
  # first (recursively) regularize all existing parenthesized expressions.
  my @c=$s.comb;
  return $s unless @c.elems>=3;
  while (process_inner_parens(@c)) {}   # returns false when no substitutions are left
  return join('',parens_on_ops(@c));
}

method new ($rpn) { return self.bless(:$rpn) }

method new_from_aos ($aos) { 
  my $rpn=aos_to_rpn($aos); 
  return Nil unless $rpn.defined;
  return self.bless(:$rpn);
}

# need to figure this one out...
# use overload '""' => \&display, '0+' => \&value, fallback => 1;

my %num= ^10 X=> 1;

method value {
  return %RPN_CACHE{$!rpn} if ~%RPN_CACHE{$!rpn};
  my @list=$!rpn.comb;
  return 0 unless (+@list);
  my @stack;
  my $bos;
  while (+@list and push @stack, $bos=shift @list) {
    next if ~%num{$bos};  # fastest way to do matching -- much better than =~/\d/
    my $op=pop @stack;
    my $n2=pop @stack;
    my $n1=pop @stack;
    my $v=calc($n1,$op,$n2);
    return %RPN_CACHE{$!rpn}=Nil unless $v.defined;
    push @stack, $v;
  }
  return %RPN_CACHE{$!rpn}=shift @stack;
}

method display { $!rpn }

method aos { return rpn_to_aos($!rpn) }

method list { $!rpn.comb }

}
