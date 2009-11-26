package Mouse;
use 5.006_002;

use Mouse::Exporter; # enables strict and warnings

our $VERSION = '0.40';

use Carp         qw(confess);
use Scalar::Util qw(blessed);

use Mouse::Util qw(load_class is_class_loaded get_code_package not_supported);

use Mouse::Meta::Module;
use Mouse::Meta::Class;
use Mouse::Meta::Role;
use Mouse::Meta::Attribute;
use Mouse::Object;
use Mouse::Util::TypeConstraints ();

Mouse::Exporter->setup_import_methods(
    as_is => [qw(
        extends with
        has
        before after around
        override super
        augment  inner
    ),
        \&Scalar::Util::blessed,
        \&Carp::confess,
   ],
);

# XXX: for backward compatibility
our @EXPORT = qw(
    extends with
    has
    before after around
    override super
    augment  inner
    blessed confess
);

sub extends { Mouse::Meta::Class->initialize(scalar caller)->superclasses(@_) }

sub has {
    my $meta = Mouse::Meta::Class->initialize(scalar caller);
    my $name = shift;

    $meta->throw_error(q{Usage: has 'name' => ( key => value, ... )})
        if @_ % 2; # odd number of arguments

    $meta->add_attribute($_ => @_) for ref($name) ? @{$name} : $name;
}

sub before {
    my $meta = Mouse::Meta::Class->initialize(scalar caller);

    my $code = pop;

    for (@_) {
        $meta->add_before_method_modifier($_ => $code);
    }
}

sub after {
    my $meta = Mouse::Meta::Class->initialize(scalar caller);

    my $code = pop;

    for (@_) {
        $meta->add_after_method_modifier($_ => $code);
    }
}

sub around {
    my $meta = Mouse::Meta::Class->initialize(scalar caller);

    my $code = pop;

    for (@_) {
        $meta->add_around_method_modifier($_ => $code);
    }
}

sub with {
    Mouse::Util::apply_all_roles(scalar(caller), @_);
}

our $SUPER_PACKAGE;
our $SUPER_BODY;
our @SUPER_ARGS;

sub super {
    # This check avoids a recursion loop - see
    # t/100_bugs/020_super_recursion.t
    return if  defined $SUPER_PACKAGE && $SUPER_PACKAGE ne caller();
    return if !defined $SUPER_BODY;
    $SUPER_BODY->(@SUPER_ARGS);
}

sub override {
    # my($name, $method) = @_;
    Mouse::Meta::Class->initialize(scalar caller)->add_override_method_modifier(@_);
}

our %INNER_BODY;
our %INNER_ARGS;

sub inner {
    my $pkg = caller();
    if ( my $body = $INNER_BODY{$pkg} ) {
        my $args = $INNER_ARGS{$pkg};
        local $INNER_ARGS{$pkg};
        local $INNER_BODY{$pkg};
        return $body->(@{$args});
    }
    else {
        return;
    }
}

sub augment {
    #my($name, $method) = @_;
    Mouse::Meta::Class->initialize(scalar caller)->add_augment_method_modifier(@_);
}

sub init_meta {
    shift;
    my %args = @_;

    my $class = $args{for_class}
                    or confess("Cannot call init_meta without specifying a for_class");

    my $base_class = $args{base_class} || 'Mouse::Object';
    my $metaclass  = $args{metaclass}  || 'Mouse::Meta::Class';

    my $meta = $metaclass->initialize($class);

    $meta->add_method(meta => sub{
        return $metaclass->initialize(ref($_[0]) || $_[0]);
    });

    $meta->superclasses($base_class)
        unless $meta->superclasses;

    # make a class type for each Mouse class
    Mouse::Util::TypeConstraints::class_type($class)
        unless Mouse::Util::TypeConstraints::find_type_constraint($class);

    return $meta;
}


1;
__END__

=head1 NAME

Mouse - Moose minus the antlers

=head1 VERSION

This document describes Mouse version 0.40

=head1 SYNOPSIS

    package Point;
    use Mouse; # automatically turns on strict and warnings

    has 'x' => (is => 'rw', isa => 'Int');
    has 'y' => (is => 'rw', isa => 'Int');

    sub clear {
        my $self = shift;
        $self->x(0);
        $self->y(0);
    }

    package Point3D;
    use Mouse;

    extends 'Point';

    has 'z' => (is => 'rw', isa => 'Int');

    after 'clear' => sub {
        my $self = shift;
        $self->z(0);
    };

=head1 DESCRIPTION

L<Moose> is wonderful. B<Use Moose instead of Mouse.>

Unfortunately, Moose has a compile-time penalty. Though significant progress
has been made over the years, the compile time penalty is a non-starter for
some very specific applications. If you are writing a command-line application
or CGI script where startup time is essential, you may not be able to use
Moose. We recommend that you instead use L<HTTP::Engine> and FastCGI for the
latter, if possible.

Mouse aims to alleviate this by providing a subset of Moose's functionality,
faster.

We're also going as light on dependencies as possible. Mouse currently has
B<no dependencies> except for testing modules.

=head2 MOOSE COMPATIBILITY

Compatibility with Moose has been the utmost concern. Fewer than 1% of the
tests fail when run against Moose instead of Mouse. Mouse code coverage is also
over 96%. Even the error messages are taken from Moose. The Mouse code just
runs the test suite 4x faster.

The idea is that, if you need the extra power, you should be able to run
C<s/Mouse/Moose/g> on your codebase and have nothing break. To that end,
we have written L<Any::Moose> which will act as Mouse unless Moose is loaded,
in which case it will act as Moose. Since Mouse is a little sloppier than
Moose, if you run into weird errors, it would be worth running:

    ANY_MOOSE=Moose perl your-script.pl

to see if the bug is caused by Mouse. Moose's diagnostics and validation are
also much better.

See also L<Mouse::Spec> for compatibility and incompatibility with Moose.

=head2 MouseX

Please don't copy MooseX code to MouseX. If you need extensions, you really
should upgrade to Moose. We don't need two parallel sets of extensions!

If you really must write a Mouse extension, please contact the Moose mailing
list or #moose on IRC beforehand.

=head1 KEYWORDS

=head2 C<< $object->meta -> Mouse::Meta::Class >>

Returns this class' metaclass instance.

=head2 C<< extends superclasses >>

Sets this class' superclasses.

=head2 C<< before (method|methods) => CodeRef >>

Installs a "before" method modifier. See L<Moose/before> or
L<Class::Method::Modifiers/before>.

Use of this feature requires L<Class::Method::Modifiers>!

=head2 C<< after (method|methods) => CodeRef >>

Installs an "after" method modifier. See L<Moose/after> or
L<Class::Method::Modifiers/after>.

Use of this feature requires L<Class::Method::Modifiers>!

=head2 C<< around (method|methods) => CodeRef >>

Installs an "around" method modifier. See L<Moose/around> or
L<Class::Method::Modifiers/around>.

Use of this feature requires L<Class::Method::Modifiers>!

=head2 C<< has (name|names) => parameters >>

Adds an attribute (or if passed an arrayref of names, multiple attributes) to
this class. Options:

=over 4

=item C<< is => ro|rw|bare >>

If specified, inlines a read-only/read-write accessor with the same name as
the attribute.

=item C<< isa => TypeConstraint >>

Provides type checking in the constructor and accessor. The following types are
supported. Any unknown type is taken to be a class check
(e.g. C<< isa => 'DateTime' >> would accept only L<DateTime> objects).

    Any Item Bool Undef Defined Value Num Int Str ClassName
    Ref ScalarRef ArrayRef HashRef CodeRef RegexpRef GlobRef
    FileHandle Object

For more documentation on type constraints, see L<Mouse::Util::TypeConstraints>.


=item C<< required => Bool >>

Whether this attribute is required to have a value. If the attribute is lazy or
has a builder, then providing a value for the attribute in the constructor is
optional.

=item C<< init_arg => Str | Undef >>

Allows you to use a different key name in the constructor.  If undef, the
attribute can't be passed to the constructor.

=item C<< default => Value | CodeRef >>

Sets the default value of the attribute. If the default is a coderef, it will
be invoked to get the default value. Due to quirks of Perl, any bare reference
is forbidden, you must wrap the reference in a coderef. Otherwise, all
instances will share the same reference.

=item C<< lazy => Bool >>

If specified, the default is calculated on demand instead of in the
constructor.

=item C<< predicate => Str >>

Lets you specify a method name for installing a predicate method, which checks
that the attribute has a value. It will not invoke a lazy default or builder
method.

=item C<< clearer => Str >>

Lets you specify a method name for installing a clearer method, which clears
the attribute's value from the instance. On the next read, lazy or builder will
be invoked.

=item C<< handles => HashRef|ArrayRef >>

Lets you specify methods to delegate to the attribute. ArrayRef forwards the
given method names to method calls on the attribute. HashRef maps local method
names to remote method names called on the attribute. Other forms of
L</handles>, such as regular expression and coderef, are not yet supported.

=item C<< weak_ref => Bool >>

Lets you automatically weaken any reference stored in the attribute.

Use of this feature requires L<Scalar::Util>!

=item C<< trigger => CodeRef >>

Any time the attribute's value is set (either through the accessor or the constructor), the trigger is called on it. The trigger receives as arguments the instance, the new value, and the attribute instance.

=item C<< builder => Str >>

Defines a method name to be called to provide the default value of the
attribute. C<< builder => 'build_foo' >> is mostly equivalent to
C<< default => sub { $_[0]->build_foo } >>.

=item C<< auto_deref => Bool >>

Allows you to automatically dereference ArrayRef and HashRef attributes in list
context. In scalar context, the reference is returned (NOT the list length or
bucket status). You must specify an appropriate type constraint to use
auto_deref.

=item C<< lazy_build => Bool >>

Automatically define the following options:

    has $attr => (
        # ...
        lazy      => 1
        builder   => "_build_$attr",
        clearer   => "clear_$attr",
        predicate => "has_$attr",
    );

=back

=head2 C<< confess(message) -> BOOM >>

L<Carp/confess> for your convenience.

=head2 C<< blessed(value) -> ClassName | undef >>

L<Scalar::Util/blessed> for your convenience.

=head1 MISC

=head2 import

Importing Mouse will default your class' superclass list to L<Mouse::Object>.
You may use L</extends> to replace the superclass list.

=head2 unimport

Please unimport Mouse (C<no Mouse>) so that if someone calls one of the
keywords (such as L</extends>) it will break loudly instead breaking subtly.

=head1 SOURCE CODE ACCESS

We have a public git repository:

 git clone git://jules.scsys.co.uk/gitmo/Mouse.git

=head1 DEPENDENCIES

Perl 5.6.2 or later.

=head1 SEE ALSO

L<Mouse::Spec>

L<Moose>

L<Class::MOP>

=head1 AUTHORS

Shawn M Moore, E<lt>sartak at gmail.comE<gt>

Yuval Kogman, E<lt>nothingmuch at woobling.orgE<gt>

tokuhirom

Yappo

wu-lee

Goro Fuji (gfx) E<lt>gfuji at cpan.orgE<gt>

with plenty of code borrowed from L<Class::MOP> and L<Moose>

=head1 BUGS

All complex software has bugs lurking in it, and this module is no exception.
Please report any bugs to C<bug-mouse at rt.cpan.org>, or through the web
interface at L<http://rt.cpan.org/Public/Dist/Display.html?Name=Mouse>

=head1 COPYRIGHT AND LICENSE

Copyright 2008-2009 Infinity Interactive, Inc.

http://www.iinteractive.com/

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

