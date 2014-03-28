package Bag;

=head1 NAME

Bag.pm - Container for objects with multiplicity

=head1 DESCRIPTION

  Bag can be used with type which can be used as a key in a hash.  Thus, references
  won't work as currently implemented.  (Could imagine requiring and referring to
  a $_->key() function for complex types to be handled by Bag.)

  * Constructors return a reference to a new Bag object without changing their source

  Accessors:
  * Boolean member functions answer truth questions about the Bag
  * Count-Based Selector member functions return items matching count-based selection criteria
  * Multiplicity member function returns the count for an item, or for all items together

  Mutators:
  * Execute bag-operations on the Bag (add, remove, remove_all, union), returning a reference
    to the modified Bag

=cut

use strict;
use warnings;
use feature qw( switch say );
use Data::Dumper;
use Globals;

=head2 Constructors

  new(Arg(s)) - returns a reference to a Bag constructed from the Arguments, which may be:
                  Hash  -- keys are items with values as multiplicities
                  Array -- identical elements will have multiplicity in the bag
                  Bag   -- Makes a copy
  copy()      - returns a reference to a copy of Bag Self

=cut

sub new {
  my $class=shift;
  my $self={ _=>{} };  # empty bag
  my $l=listref_from_any(@_);
  for (@$l) { $self->{_}{$_}++ }
  return bless $self, $class;
}
sub copy { my $self=shift; return Bag->new($self->{_}) }

=head2 Boolean Accessor member functions

  empty()           - returns true if nothing in the bag, false otherwise
  
The arguments list for the functions below may be any combination of elements, arrays of elements, Bags of elements,
references to arrays of elements, and references to hashes of elements (where the values of the hash
should be the multiplicity of the elements).

  contains(Args) - returns true if there is at least one identical element in Self
                   for every element in the argument list
  equals(Args)   - returns true if Argument list is exact match for Self, including multiplicities
  matches(Args)  - returns true if Argument list and Self have the same items as each other, ignoring multiplicities

=cut

sub empty { return scalar(keys %{shift()->{_}})==0 }

sub contains {
  my $self=shift;
  my $b=bag_from_any(@_);
  for ($b->set()) { return 0 if $self->n($_)<$b->n($_) }
  return 1;
}

sub equals {
  my $self=shift;
  my $b=bag_from_any(@_);
  my @set=$b->set();
  return 0 unless scalar(@set)==scalar(@{[ keys %{$self->{_}} ]});
  for (@set) { return 0 unless $b->n($_)==$self->n($_) }
  return 1;
}

sub matches {
  my $self=shift;
  my $b=bag_from_any(@_);
  my @set=$b->set();
  return 0 unless scalar(@set)==scalar(@{[ keys %{$self->{_}} ]});
  for (@set) { return 0 unless $self->n($_) }
  return 1;
}

=head2 Count-based Selector member functions

  count_is(n)  - elements in Self for which the multiplicity is n
  count_gt(n)  - elements in Self for which the multiplicity is (strictly) greater than n
  count_lt(n)  - elements in Self for which the multiplicity is (strictly) less than n
  	       
  singletons() - elements in Self for which the multiplicity is exactly one [ =count_is(1) ]
  multiples()  - synonym for count_gt(1)

=cut

sub count_is { my $self=shift; my $n=shift() // 0; return grep { $self->{_}{$_} == $n } keys %{$self->{_}} }
sub count_gt { my $self=shift; my $n=shift() // 0; return grep { $self->{_}{$_}  > $n } keys %{$self->{_}} }
sub count_lt { my $self=shift; my $n=shift() // 0; return grep { $self->{_}{$_}  < $n } keys %{$self->{_}} }

sub singletons { return $_[0]->count_is(1) }
sub multiples  { return $_[0]->count_gt(1) }

=head2 Count Accessor member functions 

  n(item) - Returns the multiplicity for item in Self
            If no argument, returns the number of items (including multiplicity) in Self
            If item is not in Bag, multiplicity returned is zero.

=cut

sub n { 
  my $self=shift; 
  my $k=shift; 
  if (defined $k) { 
    return $self->{_}{$k} if exists $self->{_}{$k}; 
    return 0;  # missing element
  }
  my $n=0; for (keys %{$self->{_}}) { $n+=$self->{_}{$_} } return $n;
}

=head2 Element Accessor member functions

  list() - return a list (no ordering) of contents of Bag (includes duplicates, if any)
  set()  - returns a list of the elements of bag, with duplicates removed

  random_item() - choose an item at random from Self.  Not fast, but weighted by multiplicity.

=cut

sub list { my $self=shift; return map { ($_)x$self->{_}{$_} } keys %{$self->{_}} }
sub set  { my $self=shift; return keys %{$self->{_}} }

sub random_item { return ${ [ $_[0]->list() ] }[int(rand()*$_[0]->n())] }

=head2 Mutator member functions

Each of these modifies Self, and returns a reference to the modified Self (for use in chaining).
The arguments list for each may be any combination of elements, arrays of elements, Bags of elements,
references to arrays of elements, and references to hashes of elements (where the values of the hash
should be the multiplicity of the elements).

  remove(Args)     - removes one element in Self for every identical element in Bag.
                     Elements in Bag which are not in Self are ignored.  If there are
                     multiple elements in Self matching the elements in Bag, only as
                     many elements as are in Bag will be removed from Self.

  remove_all(Args) - removes every element in Self matching an element in Bag.
                     if multiple elements in Self match an element in Bag,
                     all such elements in Self are removed, no matter how
                     many are in Bag.

  add(Args)        - adds the contents of Bag to Self

  union(Args)      - the multiplicity for each item in Self is increased
                     to match that of the corresponding element in Bag.

  intersect(Args)  - truncates the bag to the minimum of the count in the current bag
                     and the argument


=cut

sub remove {
  my $self=shift;
  for (@{listref_from_any(@_)}) { 
    if (exists $self->{_}{$_}) {
      $self->{_}{$_}--;
      delete $self->{_}{$_} if $self->{_}{$_}<=0;
    }
  }
  return $self;
}

sub remove_all { 
  my $self=shift; 
  for (bag_from_any(@_)->set()) { delete $self->{_}{$_} if exists $self->{_}{$_} };
  return $self;
}

sub add {
  my $self=shift;
  my $b=bag_from_any(@_);
  for ($b->set()) { $self->{_}{$_}+=$b->n($_) }  # initialization done by perl
  return $self;
}

sub union {
  my $self=shift;
  my $b=bag_from_any(@_);
  for ($b->set()) { $self->{_}{$_}=::max($self->n($_),$b->n($_)) }
  for ($self->set()) { delete $self->{_}{$_} if $self->{_}{$_}<=0 }  # not sure this is necessary
  return $self;
}

sub intersect {
  my $self=shift;
  my $b=bag_from_any(@_);
  for (   $b->set()) { $self->{_}{$_}=::min($self->n($_),$b->n($_)) }
  for ($self->set()) { $self->{_}{$_}=::min($self->n($_),$b->n($_)) }
  for ($self->set()) { delete $self->{_}{$_} if $self->{_}{$_}<=0 }  # not sure this is necessary
  return $self;
}


=head2 Package Translator functions

These are not member-functions, but are used by the package
to unify and simplify the behavior of the member functions for
different argument types.

   * bag_from_any     - almost identical to constructor, but pass-through for a bag-reference
   * listref_from_any - convert argument of any supported type to a listref

=cut

# what about longer argument list?  # should we accumulate them?
sub bag_from_any { return $_[0] if ref($_[0]) eq 'Bag'; return Bag->new(@_) }

# used by Bag->new(), so can't call Bag constructor here, or it gets circular
sub listref_from_any {
  my $l=[];  # list reference we'll return
  for my $a (@_) { 
    given (ref $a) {
      when ('Bag')   { push @$l, $a->list() }
      when ('HASH')  { push @$l, map { ($_)x$a->{$_} } keys %$a }
      when ('ARRAY') { push @$l, @$a }
      when ('')      { push @$l, $a }  # not a reference
      default        { die "listref_from_any cannot digest ref type $_" }
    }
  }
  return $l;
}

sub display {
  my $self=shift;
  return join(' ',sort { $a cmp $b } $self->list());
}

1;

