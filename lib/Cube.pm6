use v6;

class Cube {

  has $.faces;    # List of faces -- won't change after initialization
  has $.showing;  # Which of the faces is up any time -- changed by .roll method

  method roll                  { $!showing=$!faces.pick; self }
  method new ( $faces )        { self.bless( :$faces ) }
  submethod BUILD ( :$!faces ) { self.roll }
  
}

class Eq_Cube is Cube {

  # could use unicode for @ and *...
  
  my %cube_faces = 'red' => qw< 0 1 2 3 + - >,
     		  'blue' => qw< 0 1 2 3 * / >,
		 'green' => qw< 4 5 6 ^ - / >,
		 'black' => qw< 7 8 9 @ - / >;

  method new( Str $color ) { callwith(%cube_faces{$color}) }
  
}

class   Red_Cube is Eq_Cube { method new () { callwith('red')   } }
class  Blue_Cube is Eq_Cube { method new () { callwith('blue')  } }
class Green_Cube is Eq_Cube { method new () { callwith('green') } }
class Black_Cube is Eq_Cube { method new () { callwith('black') } }

class Cube_Bag {

  has Cube @.dice;

  method new ( @dice )       { self.bless( :@dice ) }
  method roll ()             { for @!dice { .roll } }
  method showing ()          { @!dice.map(*.showing) }
  method unique ()           { self.showing.Set.keys }
}

=head1 NAME

=begin pod

Cube.pm6 - Cube for use in Equations

=end pod

=head1 DESCRIPTION

=head2 Constructors
=begin pod

  Cube.new($list) - construct a die with faces as given by the argument list.  Any number 
                 or value of faces can be used.  The constructor does a "die roll" before
                 returning a reference to the object.

    Red_Cube.new() - constructor for a die with faces 0,1,2,3,+,-
   Blue_Cube.new() - constructor for a die with faces 0,1,2,3,*,/
  Green_Cube.new() - constructor for a die with faces 4,5,6,^,-,/
  Black_cube.new() - constructor for a die with faces 7,8,9,@,-,/

  The colored cubes are built using the Eq_Cube class which inherits from Cube and has
  a class hash with the faces encoded for every color:

  Eq_Cube.new(Str $color) - constructs an Equations Die of the color given by Str.
                          -->Should be able to limit the values of Str to just the four
			     as part of the Signature for new.
			     
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

=begin pod

  Cube_Bag package manages a collection of Cubes of any sort.  The methods are:
 
      new(@Cubes) -- constructor, argument is an array of cubes
           roll() -- call the roll() method on each Cube
        showing() -- returns an array of the faces on the Cubes
         unique() -- returns the faces on all the cubes, but with no duplicates

=end pod


=finish

package Cube_Bag;
sub new     { my $class=shift; return bless { dice=>[ @_ ] }, $class }
sub roll    { my $self=shift;  for (@{$self->{dice}}) { $_->roll() } return $self }
sub showing { my $self=shift;  return map { $_->showing() } @{$self->{dice}} }
sub unique  { my $self=shift;  return ::unique $self->showing() }

1;
