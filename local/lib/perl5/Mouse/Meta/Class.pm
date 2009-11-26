package Mouse::Meta::Class;
use Mouse::Util qw/:meta get_linear_isa not_supported/; # enables strict and warnings

use Scalar::Util qw/blessed weaken/;

use Mouse::Meta::Method::Constructor;
use Mouse::Meta::Method::Destructor;
use Mouse::Meta::Module;
our @ISA = qw(Mouse::Meta::Module);

sub method_metaclass()    { 'Mouse::Meta::Method'    }
sub attribute_metaclass() { 'Mouse::Meta::Attribute' }

sub constructor_class()   { 'Mouse::Meta::Method::Constructor' }
sub destructor_class()    { 'Mouse::Meta::Method::Destructor'  }

sub _construct_meta {
    my($class, %args) = @_;

    $args{attributes} ||= {};
    $args{methods}    ||= {};
    $args{roles}      ||= [];

    $args{superclasses} = do {
        no strict 'refs';
        \@{ $args{package} . '::ISA' };
    };

    my $self = bless \%args, ref($class) || $class;
    if(ref($self) ne __PACKAGE__){
        $self->meta->_initialize_object($self, \%args);
    }
    return $self;
}

sub create_anon_class{
    my $self = shift;
    return $self->create(undef, @_);
}

sub is_anon_class{
    return exists $_[0]->{anon_serial_id};
}

sub roles { $_[0]->{roles} }

sub calculate_all_roles {
    my $self = shift;
    my %seen;
    return grep { !$seen{ $_->name }++ }
           map  { $_->calculate_all_roles } @{ $self->roles };
}

sub superclasses {
    my $self = shift;

    if (@_) {
        foreach my $super(@_){
            Mouse::Util::load_class($super);
            my $meta = Mouse::Util::get_metaclass_by_name($super);
            if($meta && $meta->isa('Mouse::Meta::Role')){
                $self->throw_error("You cannot inherit from a Mouse Role ($super)");
            }
        }
        @{ $self->{superclasses} } = @_;
    }

    return @{ $self->{superclasses} };
}

sub find_method_by_name{
    my($self, $method_name) = @_;
    defined($method_name)
        or $self->throw_error('You must define a method name to find');

    foreach my $class( $self->linearized_isa ){
        my $method = $self->initialize($class)->get_method($method_name);
        return $method if defined $method;
    }
    return undef;
}

sub get_all_methods {
    my($self) = @_;
    return map{ $self->find_method_by_name($_) } $self->get_all_method_names;
}

sub get_all_method_names {
    my $self = shift;
    my %uniq;
    return grep { $uniq{$_}++ == 0 }
            map { Mouse::Meta::Class->initialize($_)->get_method_list() }
            $self->linearized_isa;
}

sub find_attribute_by_name{
    my($self, $name) = @_;
    my $attr;
    foreach my $class($self->linearized_isa){
        my $meta = Mouse::Util::get_metaclass_by_name($class) or next;
        $attr = $meta->get_attribute($name) and last;
    }
    return $attr;
}

sub add_attribute {
    my $self = shift;

    my($attr, $name);

    if(blessed $_[0]){
        $attr = $_[0];

        $attr->isa('Mouse::Meta::Attribute')
            || $self->throw_error("Your attribute must be an instance of Mouse::Meta::Attribute (or a subclass)");

        $name = $attr->name;
    }
    else{
        # _process_attribute
        $name = shift;

        my %args = (@_ == 1) ? %{$_[0]} : @_;

        defined($name)
            or $self->throw_error('You must provide a name for the attribute');

        if ($name =~ s/^\+//) { # inherited attributes
            my $inherited_attr = $self->find_attribute_by_name($name)
                or $self->throw_error("Could not find an attribute by the name of '$name' to inherit from in ".$self->name);

            $attr = $inherited_attr->clone_and_inherit_options(%args);
        }
        else{
            my($attribute_class, @traits) = $self->attribute_metaclass->interpolate_class(\%args);
            $args{traits} = \@traits if @traits;

            $attr = $attribute_class->new($name, %args);
        }
    }

    weaken( $attr->{associated_class} = $self );

    $self->{attributes}{$attr->name} = $attr;
    $attr->install_accessors();

    if(_MOUSE_VERBOSE && !$attr->{associated_methods} && ($attr->{is} || '') ne 'bare'){
        Carp::cluck(qq{Attribute (}.$attr->name.qq{) of class }.$self->name.qq{ has no associated methods (did you mean to provide an "is" argument?)});
    }
    return $attr;
}

sub compute_all_applicable_attributes {
    Carp::cluck('compute_all_applicable_attributes() has been deprecated')
        if _MOUSE_VERBOSE;
    return shift->get_all_attributes(@_)
}

sub get_all_attributes {
    my $self = shift;
    my (@attr, %seen);

    for my $class ($self->linearized_isa) {
        my $meta = Mouse::Util::get_metaclass_by_name($class)
            or next;

        for my $name ($meta->get_attribute_list) {
            next if $seen{$name}++;
            push @attr, $meta->get_attribute($name);
        }
    }

    return @attr;
}

sub linearized_isa { @{ get_linear_isa($_[0]->name) } }

sub new_object {
    my $self = shift;
    my %args = (@_ == 1 ? %{$_[0]} : @_);

    my $object = bless {}, $self->name;

    $self->_initialize_object($object, \%args);
    return $object;
}

sub _initialize_object{
    my($self, $object, $args) = @_;

    my @triggers_queue;

    foreach my $attribute ($self->get_all_attributes) {
        my $from = $attribute->init_arg;
        my $key  = $attribute->name;

        if (defined($from) && exists($args->{$from})) {
            $object->{$key} = $attribute->_coerce_and_verify($args->{$from}, $object);

            weaken($object->{$key})
                if ref($object->{$key}) && $attribute->is_weak_ref;

            if ($attribute->has_trigger) {
                push @triggers_queue, [ $attribute->trigger, $object->{$key} ];
            }
        }
        else {
            if ($attribute->has_default || $attribute->has_builder) {
                unless ($attribute->is_lazy) {
                    my $default = $attribute->default;
                    my $builder = $attribute->builder;
                    my $value =   $builder                ? $object->$builder()
                                : ref($default) eq 'CODE' ? $object->$default()
                                :                           $default;

                    $object->{$key} = $attribute->_coerce_and_verify($value, $object);

                    weaken($object->{$key})
                        if ref($object->{$key}) && $attribute->is_weak_ref;
                }
            }
            else {
                if ($attribute->is_required) {
                    $self->throw_error("Attribute (".$attribute->name.") is required");
                }
            }
        }
    }

    foreach my $trigger_and_value(@triggers_queue){
        my($trigger, $value) = @{$trigger_and_value};
        $trigger->($object, $value);
    }

    if($self->is_anon_class){
        $object->{__METACLASS__} = $self;
    }

    return $object;
}

sub clone_object {
    my $class  = shift;
    my $object = shift;
    my %params = (@_ == 1) ? %{$_[0]} : @_;

    (blessed($object) && $object->isa($class->name))
        || $class->throw_error("You must pass an instance of the metaclass (" . $class->name . "), not ($object)");

    my $cloned = bless { %$object }, ref $object;
    $class->_initialize_object($cloned, \%params);

    return $cloned;
}

sub clone_instance {
    my ($class, $instance, %params) = @_;

    Carp::cluck('clone_instance has been deprecated. Use clone_object instead')
        if _MOUSE_VERBOSE;
    return $class->clone_object($instance, %params);
}

sub make_immutable {
    my $self = shift;
    my %args = (
        inline_constructor => 1,
        inline_destructor  => 1,
        constructor_name   => 'new',
        @_,
    );

    $self->{is_immutable}++;

    if ($args{inline_constructor}) {
        $self->add_method($args{constructor_name} =>
            $self->constructor_class->_generate_constructor($self, \%args));
    }

    if ($args{inline_destructor}) {
        $self->add_method(DESTROY =>
            $self->destructor_class->_generate_destructor($self, \%args));
    }

    # Moose's make_immutable returns true allowing calling code to skip setting an explicit true value
    # at the end of a source file. 
    return 1;
}

sub make_mutable { not_supported }

sub is_immutable {  $_[0]->{is_immutable} }
sub is_mutable   { !$_[0]->{is_immutable} }

sub _install_modifier_pp{
    my( $self, $type, $name, $code ) = @_;
    my $into = $self->name;

    my $original = $into->can($name)
        or $self->throw_error("The method '$name' is not found in the inheritance hierarchy for class $into");

    my $modifier_table = $self->{modifiers}{$name};

    if(!$modifier_table){
        my(@before, @after, @around, $cache, $modified);

        $cache = $original;

        $modified = sub {
            for my $c (@before) { $c->(@_) }

            if(wantarray){ # list context
                my @rval = $cache->(@_);

                for my $c(@after){ $c->(@_) }
                return @rval;
            }
            elsif(defined wantarray){ # scalar context
                my $rval = $cache->(@_);

                for my $c(@after){ $c->(@_) }
                return $rval;
            }
            else{ # void context
                $cache->(@_);

                for my $c(@after){ $c->(@_) }
                return;
            }
        };

        $self->{modifiers}{$name} = $modifier_table = {
            original => $original,

            before   => \@before,
            after    => \@after,
            around   => \@around,

            cache    => \$cache, # cache for around modifiers
        };

        $self->add_method($name => $modified);
    }

    if($type eq 'before'){
        unshift @{$modifier_table->{before}}, $code;
    }
    elsif($type eq 'after'){
        push @{$modifier_table->{after}}, $code;
    }
    else{ # around
        push @{$modifier_table->{around}}, $code;

        my $next = ${ $modifier_table->{cache} };
        ${ $modifier_table->{cache} } = sub{ $code->($next, @_) };
    }

    return;
}

sub _install_modifier {
    my ( $self, $type, $name, $code ) = @_;

    # load Class::Method::Modifiers first
    my $no_cmm_fast = do{
        local $@;
        eval q{ require Class::Method::Modifiers::Fast };
        $@;
    };

    my $impl;
    if($no_cmm_fast){
        $impl = \&_install_modifier_pp;
    }
    else{
        my $install_modifier = Class::Method::Modifiers::Fast->can('_install_modifier');
        $impl = sub {
            my ( $self, $type, $name, $code ) = @_;
            my $into = $self->name;
            $install_modifier->($into, $type, $name, $code);

            $self->add_method($name => do{
                no strict 'refs';
                \&{ $into . '::' . $name };
            });
            return;
        };
    }

    # replace this method itself :)
    {
        no warnings 'redefine';
        *_install_modifier = $impl;
    }

    $self->$impl( $type, $name, $code );
}

sub add_before_method_modifier {
    my ( $self, $name, $code ) = @_;
    $self->_install_modifier( 'before', $name, $code );
}

sub add_around_method_modifier {
    my ( $self, $name, $code ) = @_;
    $self->_install_modifier( 'around', $name, $code );
}

sub add_after_method_modifier {
    my ( $self, $name, $code ) = @_;
    $self->_install_modifier( 'after', $name, $code );
}

sub add_override_method_modifier {
    my ($self, $name, $code) = @_;

    if($self->has_method($name)){
        $self->throw_error("Cannot add an override method if a local method is already present");
    }

    my $package = $self->name;

    my $super_body = $package->can($name)
        or $self->throw_error("You cannot override '$name' because it has no super method");

    $self->add_method($name => sub {
        local $Mouse::SUPER_PACKAGE = $package;
        local $Mouse::SUPER_BODY    = $super_body;
        local @Mouse::SUPER_ARGS    = @_;

        $code->(@_);
    });
    return;
}

sub add_augment_method_modifier {
    my ($self, $name, $code) = @_;
    if($self->has_method($name)){
        $self->throw_error("Cannot add an augment method if a local method is already present");
    }

    my $super = $self->find_method_by_name($name)
        or $self->throw_error("You cannot augment '$name' because it has no super method");

    my $super_package = $super->package_name;
    my $super_body    = $super->body;

    $self->add_method($name => sub{
        local $Mouse::INNER_BODY{$super_package} = $code;
        local $Mouse::INNER_ARGS{$super_package} = [@_];
        $super_body->(@_);
    });
    return;
}

sub does_role {
    my ($self, $role_name) = @_;

    (defined $role_name)
        || $self->throw_error("You must supply a role name to look for");

    for my $class ($self->linearized_isa) {
        my $meta = Mouse::Util::get_metaclass_by_name($class)
            or next;

        for my $role (@{ $meta->roles }) {

            return 1 if $role->does_role($role_name);
        }
    }

    return 0;
}

1;
__END__

=head1 NAME

Mouse::Meta::Class - The Mouse class metaclass

=head1 VERSION

This document describes Mouse version 0.40

=head1 METHODS

=head2 C<< initialize(ClassName) -> Mouse::Meta::Class >>

Finds or creates a C<Mouse::Meta::Class> instance for the given ClassName. Only
one instance should exist for a given class.

=head2 C<< name -> ClassName >>

Returns the name of the owner class.

=head2 C<< superclasses -> ClassNames >> C<< superclass(ClassNames) >>

Gets (or sets) the list of superclasses of the owner class.

=head2 C<< add_method(name => CodeRef) >>

Adds a method to the owner class.

=head2 C<< has_method(name) -> Bool >>

Returns whether we have a method with the given name.

=head2 C<< get_method(name) -> Mouse::Meta::Method | undef >>

Returns a L<Mouse::Meta::Method> with the given name.

Note that you can also use C<< $metaclass->name->can($name) >> for a method body.

=head2 C<< get_method_list -> Names >>

Returns a list of method names which are defined in the local class.
If you want a list of all applicable methods for a class, use the
C<get_all_methods> method.

=head2 C<< get_all_methods -> (Mouse::Meta::Method) >>

Return the list of all L<Mouse::Meta::Method> instances associated with
the class and its superclasses.

=head2 C<< add_attribute(name => spec | Mouse::Meta::Attribute) >>

Begins keeping track of the existing L<Mouse::Meta::Attribute> for the owner
class.

=head2 C<< has_attribute(Name) -> Bool >>

Returns whether we have a L<Mouse::Meta::Attribute> with the given name.

=head2 C<< get_attribute Name -> Mouse::Meta::Attribute | undef >>

Returns the L<Mouse::Meta::Attribute> with the given name.

=head2 C<< get_attribute_list -> Names >>

Returns a list of attribute names which are defined in the local
class. If you want a list of all applicable attributes for a class,
use the C<get_all_attributes> method.

=head2 C<< get_all_attributes -> (Mouse::Meta::Attribute) >>

Returns the list of all L<Mouse::Meta::Attribute> instances associated with
this class and its superclasses.

=head2 C<< linearized_isa -> [ClassNames] >>

Returns the list of classes in method dispatch order, with duplicates removed.

=head2 C<< new_object(Parameters) -> Instance >>

Creates a new instance.

=head2 C<< clone_object(Instance, Parameters) -> Instance >>

Clones the given instance which must be an instance governed by this
metaclass.

=head2 C<< throw_error(Message, Parameters) >>

Throws an error with the given message.

=head1 SEE ALSO

L<Moose::Meta::Class>

L<Class::MOP::Class>

=cut

