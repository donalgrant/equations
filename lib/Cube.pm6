use v6;

class Cube {

  has @!faces;
  has $!showing;

  method roll    { $!showing=@!faces.pick }
  method showing { $!showing }
  method faces   { @!faces }

  method BUILD( @faces ) { @!faces=@faces; .roll }
  
}

=head1 NAME

=begin pod

Cube.pm6 - Cube for use in Equations

=end pod

=head1 DESCRIPTION

=head2 Constructors
=begin pod

  Cube::new(@) - construct a die with faces as given by the argument list.  Any number 
                 or value of faces can be used.  The constructor does a "die roll" before
                 returning a reference to the object.

    Red_Cube::new(@) - constructor for a die with faces 0,1,2,3,+,-
   Blue_Cube::new(@) - constructor for a die with faces 0,1,2,3,*,/
  Green_Cube::new(@) - constructor for a die with faces 4,5,6,^,-,/
  Black_cube::new(@) - constructor for a die with faces 7,8,9,@,-,/
=end pod
=head2 Mutators
=begin pod
  roll() - randomly selects one of the faces to be displayed with the showing() method
           this changes the internal state of the object.  A reference to the object is returned.
           So, to roll and get the result, use $cube->roll()->showing()
=end pod
=head2 Accessor
=begin pod
  showing() - displays the upward face (set by a call to roll()
    faces() - array of all faces on the cube
=end pod
=finish

package Red_Cube;  
push @Red_Cube::ISA, qw( Cube );
sub new { my $class=shift; return $class->SUPER::new('0','1','2','3','+','-') }

package Blue_Cube;  
push @Blue_Cube::ISA, qw( Cube );
sub new { my $class=shift; return $class->SUPER::new('0','1','2','3','*','/') }

package Green_Cube;  
push @Green_Cube::ISA, qw( Cube );
sub new { my $class=shift; return $class->SUPER::new('4','5','6','^','*','-') }

package Black_Cube;  
push @Black_Cube::ISA, qw( Cube );
sub new { my $class=shift; return $class->SUPER::new('7','8','9','/','-','@') }


=head2

  Cube_Bag package manages a collection of Cubes of any sort.  The methods are:
 
       new(@) -- constructor, argument is an array of cubes
      roll()  -- call the roll() method on each Cube
   showing()  -- returns an array of the faces on the Cubes
    unique()  -- returns the faces on all the cubes, but with no duplicates

=cut

package Cube_Bag;
sub new     { my $class=shift; return bless { dice=>[ @_ ] }, $class }
sub roll    { my $self=shift;  for (@{$self->{dice}}) { $_->roll() } return $self }
sub showing { my $self=shift;  return map { $_->showing() } @{$self->{dice}} }
sub unique  { my $self=shift;  return ::unique $self->showing() }

1;
