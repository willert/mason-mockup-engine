#!env perl
use 5.8.4;
BEGIN{
 $INC{q{Params/ValidatePP.pm}} = 1;
 $INC{q{Data/AsObject/Array.pm}} = 1;
 $INC{q{Data/AsObject/Hash.pm}} = 1;
 $INC{q{HTML/Mason.pm}} = 1;
 $INC{q{Path/Class/File.pm}} = 1;
 $INC{q{Path/Class/Dir.pm}} = 1;
 $ENV{PERL_JSON_BACKEND} = "JSON::PP";
 $DB::sub;
}
BEGIN { $INC{q{Params/Validate.pm}} = 1;
package Params::Validate;
use strict;
BEGIN
{
 use Exporter;
 use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS %OPTIONS $options $NO_VALIDATION);

@ISA = 'Exporter';

$VERSION = '0.92';

my %tags =
 ( types =>
 [ qw(SCALAR ARRAYREF HASHREF CODEREF GLOB GLOBREF SCALARREF HANDLE BOOLEAN UNDEF OBJECT) ],
 );

%EXPORT_TAGS =
 ( 'all' => [ qw(validate validate_pos validation_options validate_with),
 map { @{ $tags{$_} } } keys %tags ],
 %tags,
 );

@EXPORT_OK = ( @{ $EXPORT_TAGS{all} }, 'set_options' );
 @EXPORT = qw(validate validate_pos);

$NO_VALIDATION = $ENV{PERL_NO_VALIDATION};

eval { require Params::ValidateXS; } unless $ENV{PV_TEST_PERL};

if ( $@ || $ENV{PV_TEST_PERL} )
 {
 require Params::ValidatePP;
 }
}
1;
}
BEGIN { $INC{q{Params/ValidatePP.pm}} = 1;
package Params::Validate;
use strict;
use warnings;
use Scalar::Util ();
no warnings 'redefine';
BEGIN
{
 sub SCALAR () { 1 }
 sub ARRAYREF () { 2 }
 sub HASHREF () { 4 }
 sub CODEREF () { 8 }
 sub GLOB () { 16 }
 sub GLOBREF () { 32 }
 sub SCALARREF () { 64 }
 sub UNKNOWN () { 128 }
 sub UNDEF () { 256 }
 sub OBJECT () { 512 }

sub HANDLE () { 16 | 32 }
 sub BOOLEAN () { 1 | 256 }
}
sub validate_pos (\@@)
{
 return if $NO_VALIDATION && ! defined wantarray;

my $p = shift;

my @specs = @_;

my @p = @$p;
 if ( $NO_VALIDATION )
 {
 for ( my $x = $#p + 1; $x <= $#specs; $x++ )
 {
 $p[$x] =
 $specs[$x]->{default}
 if ref $specs[$x] && exists $specs[$x]->{default};
 }

return wantarray ? @p : \@p;
 }
 local $options ||= _get_options( (caller(0))[0] )
 unless defined $options;

my $min = 0;

while (1)
 {
 last unless ( ref $specs[$min] ?
 ! ( exists $specs[$min]->{default} || $specs[$min]->{optional} ) :
 $specs[$min] );

$min++;
 }

my $max = scalar @specs;

my $actual = scalar @p;
 unless ($actual >= $min && ( $options->{allow_extra} || $actual <= $max ) )
 {
 my $minmax =
 ( $options->{allow_extra} ?
 "at least $min" :
 ( $min != $max ? "$min - $max" : $max ) );

my $val = $options->{allow_extra} ? $min : $max;
 $minmax .= $val != 1 ? ' were' : ' was';

my $called = _get_called();

$options->{on_fail}->
 ( "$actual parameter" .
 ($actual != 1 ? 's' : '') .
 " " .
 ($actual != 1 ? 'were' : 'was' ) .
 " passed to $called but $minmax expected\n" );
 }

my $bigger = $#p > $#specs ? $#p : $#specs;
 foreach ( 0..$bigger )
 {
 my $spec = $specs[$_];

next unless ref $spec;

if ( $_ <= $#p )
 {
 my $value = defined $p[$_] ? qq|"$p[$_]"| : 'undef';
 _validate_one_param( $p[$_], \@p, $spec, "Parameter #" . ($_ + 1) . " ($value)");
 }

$p[$_] = $spec->{default} if $_ > $#p && exists $spec->{default};
 }

_validate_pos_depends(\@p, \@specs);

foreach ( grep { defined $p[$_] && ! ref $p[$_]
 && ref $specs[$_] && $specs[$_]{untaint} }
 0..$bigger )
 {
 ($p[$_]) = $p[$_] =~ /(.+)/;
 }

return wantarray ? @p : \@p;
}
sub _validate_pos_depends
{
 my ( $p, $specs ) = @_;

for my $p_idx ( 0..$#$p )
 {
 my $spec = $specs->[$p_idx];

next unless $spec && UNIVERSAL::isa( $spec, 'HASH' ) && exists $spec->{depends};

my $depends = $spec->{depends};

if ( ref $depends )
 {
 require Carp;
 local $Carp::CarpLevel = 2;
 Carp::croak( "Arguments to 'depends' for validate_pos() must be a scalar" )
 }

my $p_size = scalar @$p;
 if ( $p_size < $depends - 1 )
 {
 my $error = ( "Parameter #" . ($p_idx + 1) . " depends on parameter #" .
 $depends . ", which was not given" );

$options->{on_fail}->($error);
 }
 }
 return 1;
}
sub _validate_named_depends
{
 my ( $p, $specs ) = @_;

foreach my $pname ( keys %$p )
 {
 my $spec = $specs->{$pname};

next unless $spec && UNIVERSAL::isa( $spec, 'HASH' ) && $spec->{depends};

unless ( UNIVERSAL::isa( $spec->{depends}, 'ARRAY' ) || ! ref $spec->{depends} )
 {
 require Carp;
 local $Carp::CarpLevel = 2;
 Carp::croak( "Arguments to 'depends' must be a scalar or arrayref" );
 }

foreach my $depends_name ( ref $spec->{depends}
 ? @{ $spec->{depends} }
 : $spec->{depends} )
 {
 unless ( exists $p->{$depends_name} )
 {
 my $error = ( "Parameter '$pname' depends on parameter '" .
 $depends_name . "', which was not given" );

$options->{on_fail}->($error);
 }
 }
 }
}
sub validate (\@$)
{
 return if $NO_VALIDATION && ! defined wantarray;

my $p = $_[0];

my $specs = $_[1];
 local $options = _get_options( (caller(0))[0] ) unless defined $options;

if ( ref $p eq 'ARRAY' )
 {
 if ( ref $p->[0] )
 {
 $p = $p->[0];
 }
 elsif ( @$p % 2 )
 {
 my $called = _get_called();

$options->{on_fail}->
 ( "Odd number of parameters in call to $called " .
 "when named parameters were expected\n" );
 }
 else
 {
 $p = {@$p};
 }
 }

if ( $options->{normalize_keys} )
 {
 $specs = _normalize_callback( $specs, $options->{normalize_keys} );
 $p = _normalize_callback( $p, $options->{normalize_keys} );
 }
 elsif ( $options->{ignore_case} || $options->{strip_leading} )
 {
 $specs = _normalize_named($specs);
 $p = _normalize_named($p);
 }

if ($NO_VALIDATION)
 {
 return
 ( wantarray ?
 (
 ( map { $_ => $specs->{$_}->{default} }
 grep { ref $specs->{$_} && exists $specs->{$_}->{default} }
 keys %$specs
 ),
 ( ref $p eq 'ARRAY' ?
 ( ref $p->[0] ?
 %{ $p->[0] } :
 @$p ) :
 %$p
 )
 ) :
 do
 {
 my $ref =
 ( ref $p eq 'ARRAY' ?
 ( ref $p->[0] ?
 $p->[0] :
 {@$p} ) :
 $p
 );

foreach ( grep { ref $specs->{$_} && exists $specs->{$_}->{default} }
 keys %$specs )
 {
 $ref->{$_} = $specs->{$_}->{default}
 unless exists $ref->{$_};
 }

return $ref;
 }
 );
 }

_validate_named_depends($p, $specs);

unless ( $options->{allow_extra} )
 {
 if ( my @unmentioned = grep { ! exists $specs->{$_} } keys %$p )
 {
 my $called = _get_called();

$options->{on_fail}->
 ( "The following parameter" . (@unmentioned > 1 ? 's were' : ' was') .
 " passed in the call to $called but " .
 (@unmentioned > 1 ? 'were' : 'was') .
 " not listed in the validation options: @unmentioned\n" );
 }
 }

my @missing;
 keys %$specs;
 OUTER:
 while ( my ($key, $spec) = each %$specs )
 {
 if ( ! exists $p->{$key} &&
 ( ref $spec
 ? ! (
 do
 {
 if ( exists $spec->{default} )
 {
 $p->{$key} = $spec->{default};
 next OUTER;
 }
 }
 ||
 do
 {
 next OUTER if $spec->{optional};
 }
 )
 : $spec
 )
 )
 {
 push @missing, $key;
 }
 elsif (ref $spec)
 {
 my $value = defined $p->{$key} ? qq|"$p->{$key}"| : 'undef';
 _validate_one_param( $p->{$key}, $p, $spec, "The '$key' parameter ($value)" );
 }
 }

if (@missing)
 {
 my $called = _get_called();

my $missing = join ', ', map {"'$_'"} @missing;
 $options->{on_fail}->
 ( "Mandatory parameter" .
 (@missing > 1 ? 's': '') .
 " $missing missing in call to $called\n" );
 }
 foreach my $key ( grep { defined $p->{$_} && ! ref $p->{$_}
 && ref $specs->{$_} && $specs->{$_}{untaint} }
 keys %$p )
 {
 ($p->{$key}) = $p->{$key} =~ /(.+)/;
 }

return wantarray ? %$p : $p;
}
sub validate_with
{
 return if $NO_VALIDATION && ! defined wantarray;

my %p = @_;

local $options = _get_options( (caller(0))[0], %p );

unless ( $NO_VALIDATION )
 {
 unless ( exists $options->{called} )
 {
 $options->{called} = (caller( $options->{stack_skip} ))[3];
 }

}

if ( UNIVERSAL::isa( $p{spec}, 'ARRAY' ) )
 {
 return validate_pos( @{ $p{params} }, @{ $p{spec} } );
 }
 else
 {
 return &validate( $p{params}, $p{spec} );
 }
}
sub _normalize_callback
{
 my ( $p, $func ) = @_;

my %new;

foreach my $key ( keys %$p )
 {
 my $new_key = $func->( $key );

unless ( defined $new_key )
 {
 die "The normalize_keys callback did not return a defined value when normalizing the key '$key'";
 }

if ( exists $new{$new_key} )
 {
 die "The normalize_keys callback returned a key that already exists, '$new_key', when normalizing the key '$key'";
 }

$new{$new_key} = $p->{ $key };
 }

return \%new;
}
sub _normalize_named
{
 my %h = ( ref $_[0] ) =~ /ARRAY/ ? @{ $_[0] } : %{ $_[0] };

if ( $options->{ignore_case} )
 {
 $h{ lc $_ } = delete $h{$_} for keys %h;
 }

if ( $options->{strip_leading} )
 {
 foreach my $key (keys %h)
 {
 my $new;
 ($new = $key) =~ s/^\Q$options->{strip_leading}\E//;
 $h{$new} = delete $h{$key};
 }
 }

return \%h;
}
sub _validate_one_param
{
 my ($value, $params, $spec, $id) = @_;

if ( exists $spec->{type} )
 {
 unless ( defined $spec->{type}
 && Scalar::Util::looks_like_number( $spec->{type} )
 && $spec->{type} > 0 )
 {
 my $msg = "$id has a type specification which is not a number. It is ";
 if ( defined $spec->{type} )
 {
 $msg .= "a string - $spec->{type}";
 }
 else
 {
 $msg .= "undef";
 }

$msg .= ".\n Use the constants exported by Params::Validate to declare types.";

$options->{on_fail}->($msg);
 }

unless ( _get_type($value) & $spec->{type} )
 {
 my $type = _get_type($value);

my @is = _typemask_to_strings($type);
 my @allowed = _typemask_to_strings($spec->{type});
 my $article = $is[0] =~ /^[aeiou]/i ? 'an' : 'a';

my $called = _get_called(1);

$options->{on_fail}->
 ( "$id to $called was $article '@is', which " .
 "is not one of the allowed types: @allowed\n" );
 }
 }
 return unless ( $spec->{isa} || $spec->{can} ||
 $spec->{callbacks} || $spec->{regex} );

if ( exists $spec->{isa} )
 {
 foreach ( ref $spec->{isa} ? @{ $spec->{isa} } : $spec->{isa} )
 {
 unless ( eval { $value->isa($_) } )
 {
 my $is = ref $value ? ref $value : 'plain scalar';
 my $article1 = $_ =~ /^[aeiou]/i ? 'an' : 'a';
 my $article2 = $is =~ /^[aeiou]/i ? 'an' : 'a';

my $called = _get_called(1);

$options->{on_fail}->
 ( "$id to $called was not $article1 '$_' " .
 "(it is $article2 $is)\n" );
 }
 }
 }

if ( exists $spec->{can} )
 {
 foreach ( ref $spec->{can} ? @{ $spec->{can} } : $spec->{can} )
 {
 unless ( eval { $value->can($_) } )
 {
 my $called = _get_called(1);

$options->{on_fail}->( "$id to $called does not have the method: '$_'\n" );
 }
 }
 }

if ( $spec->{callbacks} )
 {
 unless ( UNIVERSAL::isa( $spec->{callbacks}, 'HASH' ) )
 {
 my $called = _get_called(1);

$options->{on_fail}->
 ( "'callbacks' validation parameter for $called must be a hash reference\n" );
 }

foreach ( keys %{ $spec->{callbacks} } )
 {
 unless ( UNIVERSAL::isa( $spec->{callbacks}{$_}, 'CODE' ) )
 {
 my $called = _get_called(1);

$options->{on_fail}->( "callback '$_' for $called is not a subroutine reference\n" );
 }

unless ( $spec->{callbacks}{$_}->($value, $params) )
 {
 my $called = _get_called(1);

$options->{on_fail}->( "$id to $called did not pass the '$_' callback\n" );
 }
 }
 }

if ( exists $spec->{regex} )
 {
 unless ( ( defined $value ? $value : '' ) =~ /$spec->{regex}/ )
 {
 my $called = _get_called(1);

$options->{on_fail}->( "$id to $called did not pass regex check\n" );
 }
 }
}
{
 my %isas = ( 'ARRAY' => ARRAYREF,
 'HASH' => HASHREF,
 'CODE' => CODEREF,
 'GLOB' => GLOBREF,
 'SCALAR' => SCALARREF,
 );
 my %simple_refs = map { $_ => 1 } keys %isas;

sub _get_type
 {
 return UNDEF unless defined $_[0];

my $ref = ref $_[0];
 unless ($ref)
 {
 return GLOB if UNIVERSAL::isa( \$_[0], 'GLOB' );
 return SCALAR;
 }

return $isas{$ref} if $simple_refs{$ref};

foreach ( keys %isas )
 {
 return $isas{$_} | OBJECT if UNIVERSAL::isa( $_[0], $_ );
 }
 return UNKNOWN;
 }
}
{
 my %type_to_string = ( SCALAR() => 'scalar',
 ARRAYREF() => 'arrayref',
 HASHREF() => 'hashref',
 CODEREF() => 'coderef',
 GLOB() => 'glob',
 GLOBREF() => 'globref',
 SCALARREF() => 'scalarref',
 UNDEF() => 'undef',
 OBJECT() => 'object',
 UNKNOWN() => 'unknown',
 );

sub _typemask_to_strings
 {
 my $mask = shift;

my @types;
 foreach ( SCALAR, ARRAYREF, HASHREF, CODEREF, GLOB, GLOBREF,
 SCALARREF, UNDEF, OBJECT, UNKNOWN )
 {
 push @types, $type_to_string{$_} if $mask & $_;
 }
 return @types ? @types : ('unknown');
 }
}
{
 my %defaults = ( ignore_case => 0,
 strip_leading => 0,
 allow_extra => 0,
 on_fail => sub { require Carp;
 Carp::confess($_[0]) },
 stack_skip => 1,
 normalize_keys => undef,
 );

*set_options = \&validation_options;
 sub validation_options
 {
 my %opts = @_;

my $caller = caller;

foreach ( keys %defaults )
 {
 $opts{$_} = $defaults{$_} unless exists $opts{$_};
 }

$OPTIONS{$caller} = \%opts;
 }

sub _get_options
 {
 my $caller = shift;

if (@_)
 {

return
 ( $OPTIONS{$caller} ?
 { %{ $OPTIONS{$caller} },
 @_ } :
 { %defaults, @_ }
 );
 }
 else
 {
 return
 ( exists $OPTIONS{$caller} ?
 $OPTIONS{$caller} :
 \%defaults );
 }
 }
}
sub _get_called
{
 my $extra_skip = $_[0] || 0;
 $extra_skip++;

my $called =
 ( exists $options->{called} ?
 $options->{called} :
 ( caller( $options->{stack_skip} + $extra_skip ) )[3]
 );

$called = 'N/A' unless defined $called;

return $called;
}
1;
}
BEGIN { $INC{q{Path/Class/Entity.pm}} = 1;
package Path::Class::Entity;
$VERSION = '0.17';
use strict;
use File::Spec;
use File::stat ();
use Cwd;
use overload
 (
 q[""] => 'stringify',
 'bool' => 'boolify',
 fallback => 1,
 );
sub new {
 my $from = shift;
 my ($class, $fs_class) = (ref($from)
 ? (ref $from, $from->{file_spec_class})
 : ($from, $Path::Class::Foreign));
 return bless {file_spec_class => $fs_class}, $class;
}
sub is_dir { 0 }
sub _spec_class {
 my ($class, $type) = @_;

die "Invalid system type '$type'" unless ($type) = $type =~ /^(\w+)$/; 
 my $spec = "File::Spec::$type";
 eval "require $spec; 1" or die $@;
 return $spec;
}
sub new_foreign {
 my ($class, $type) = (shift, shift);
 local $Path::Class::Foreign = $class->_spec_class($type);
 return $class->new(@_);
}
sub _spec { $_[0]->{file_spec_class} || 'File::Spec' }
sub boolify { 1 }
sub is_absolute {
 $_[0]->_spec->file_name_is_absolute($_[0]->stringify)
}
sub is_relative { ! $_[0]->is_absolute }
sub cleanup {
 my $self = shift;
 my $cleaned = $self->new( $self->_spec->canonpath($self) );
 %$self = %$cleaned;
 return $self;
}
sub resolve {
 my $self = shift;
 my $cleaned = $self->new( Cwd::realpath($self->stringify) );
 $cleaned = $cleaned->relative if $self->is_relative;

%$self = %$cleaned;
 return $self;
}
sub absolute {
 my $self = shift;
 return $self if $self->is_absolute;
 return $self->new($self->_spec->rel2abs($self->stringify, @_));
}
sub relative {
 my $self = shift;
 return $self->new($self->_spec->abs2rel($self->stringify, @_));
}
sub stat { File::stat::stat("$_[0]") }
sub lstat { File::stat::lstat("$_[0]") }
1;
}
BEGIN { $INC{q{Path/Class/File.pm}} = 1;
package Path::Class::File;
$VERSION = '0.17';
use strict;
use Path::Class::Dir;
use Path::Class::Entity;
use base qw(Path::Class::Entity);
use IO::File ();
sub new {
 my $self = shift->SUPER::new;
 my $file = pop();
 my @dirs = @_;

my ($volume, $dirs, $base) = $self->_spec->splitpath($file);

if (length $dirs) {
 push @dirs, $self->_spec->catpath($volume, $dirs, '');
 }

$self->{dir} = @dirs ? Path::Class::Dir->new(@dirs) : undef;
 $self->{file} = $base;

return $self;
}
sub as_foreign {
 my ($self, $type) = @_;
 local $Path::Class::Foreign = $self->_spec_class($type);
 my $foreign = ref($self)->SUPER::new;
 $foreign->{dir} = $self->{dir}->as_foreign($type) if defined $self->{dir};
 $foreign->{file} = $self->{file};
 return $foreign;
}
sub stringify {
 my $self = shift;
 return $self->{file} unless defined $self->{dir};
 return $self->_spec->catfile($self->{dir}->stringify, $self->{file});
}
sub dir {
 my $self = shift;
 return $self->{dir} if defined $self->{dir};
 return Path::Class::Dir->new($self->_spec->curdir);
}
BEGIN { *parent = \&dir; }
sub volume {
 my $self = shift;
 return '' unless defined $self->{dir};
 return $self->{dir}->volume;
}
sub basename { shift->{file} }
sub open { IO::File->new(@_) }
sub openr { $_[0]->open('r') or die "Can't read $_[0]: $!" }
sub openw { $_[0]->open('w') or die "Can't write $_[0]: $!" }
sub touch {
 my $self = shift;
 if (-e $self) {
 my $now = time();
 utime $now, $now, $self;
 } else {
 $self->openw;
 }
}
sub slurp {
 my ($self, %args) = @_;
 my $fh = $self->openr;

if ($args{chomped} or $args{chomp}) {
 chomp( my @data = <$fh> );
 return wantarray ? @data : join '', @data;
 }

local $/ unless wantarray;
 return <$fh>;
}
sub remove {
 my $file = shift->stringify;
 return unlink $file unless -e $file; 
 1 while unlink $file;
 return not -e $file;
}
1;
}
BEGIN { $INC{q{Path/Class/Dir.pm}} = 1;
package Path::Class::Dir;
$VERSION = '0.17';
use strict;
use Path::Class::File;
use Path::Class::Entity;
use Carp();
use base qw(Path::Class::Entity);
use IO::Dir ();
use File::Path ();
sub new {
 my $self = shift->SUPER::new();
 return if @_==1 && !defined($_[0]);

my $s = $self->_spec;

my $first = (@_ == 0 ? $s->curdir :
 $_[0] eq '' ? (shift, $s->rootdir) :
 shift()
 );

($self->{volume}, my $dirs) = $s->splitpath( $s->canonpath($first) , 1);
 $self->{dirs} = [$s->splitdir($s->catdir($dirs, @_))];

return $self;
}
sub is_dir { 1 }
sub as_foreign {
 my ($self, $type) = @_;

my $foreign = do {
 local $self->{file_spec_class} = $self->_spec_class($type);
 $self->SUPER::new;
 };
 $foreign->{volume} = $self->{volume};
 my ($u, $fu) = ($self->_spec->updir, $foreign->_spec->updir);
 $foreign->{dirs} = [ map {$_ eq $u ? $fu : $_} @{$self->{dirs}}];
 return $foreign;
}
sub stringify {
 my $self = shift;
 my $s = $self->_spec;
 return $s->catpath($self->{volume},
 $s->catdir(@{$self->{dirs}}),
 '');
}
sub volume { shift()->{volume} }
sub file {
 local $Path::Class::Foreign = $_[0]->{file_spec_class} if $_[0]->{file_spec_class};
 return Path::Class::File->new(@_);
}
sub dir_list {
 my $self = shift;
 my $d = $self->{dirs};
 return @$d unless @_;

my $offset = shift;
 if ($offset < 0) { $offset = $#$d + $offset + 1 }

return wantarray ? @$d[$offset .. $#$d] : $d->[$offset] unless @_;

my $length = shift;
 if ($length < 0) { $length = $#$d + $length + 1 - $offset }
 return @$d[$offset .. $length + $offset - 1];
}
sub subdir {
 my $self = shift;
 return $self->new($self, @_);
}
sub parent {
 my $self = shift;
 my $dirs = $self->{dirs};
 my ($curdir, $updir) = ($self->_spec->curdir, $self->_spec->updir);

if ($self->is_absolute) {
 my $parent = $self->new($self);
 pop @{$parent->{dirs}};
 return $parent;

} elsif ($self eq $curdir) {
 return $self->new($updir);

} elsif (!grep {$_ ne $updir} @$dirs) { 
 return $self->new($self, $updir); 

} elsif (@$dirs == 1) {
 return $self->new($curdir);

} else {
 my $parent = $self->new($self);
 pop @{$parent->{dirs}};
 return $parent;
 }
}
sub relative {
 my $self = shift;
 my $rel = $self->_spec->abs2rel($self->stringify, @_);
 return $self->new( length $rel ? $rel : $self->_spec->curdir );
}
sub open { IO::Dir->new(@_) }
sub mkpath { File::Path::mkpath(shift()->stringify, @_) }
sub rmtree { File::Path::rmtree(shift()->stringify, @_) }
sub remove {
 rmdir( shift() );
}
sub recurse {
 my $self = shift;
 my %opts = (preorder => 1, depthfirst => 0, @_);

my $callback = $opts{callback}
 or Carp::croak( "Must provide a 'callback' parameter to recurse()" );

my @queue = ($self);

my $visit_entry;
 my $visit_dir =
 $opts{depthfirst} && $opts{preorder}
 ? sub {
 my $dir = shift;
 $callback->($dir);
 unshift @queue, $dir->children;
 }
 : $opts{preorder}
 ? sub {
 my $dir = shift;
 $callback->($dir);
 push @queue, $dir->children;
 }
 : sub {
 my $dir = shift;
 $visit_entry->($_) foreach $dir->children;
 $callback->($dir);
 };

$visit_entry = sub {
 my $entry = shift;
 if ($entry->is_dir) { $visit_dir->($entry) } 
 else { $callback->($entry) }
 };

while (@queue) {
 $visit_entry->( shift @queue );
 }
}
sub children {
 my ($self, %opts) = @_;

my $dh = $self->open or Carp::croak( "Can't open directory $self: $!" );

my @out;
 while (my $entry = $dh->read) {
 next if (!$opts{all} && ($entry eq '.' || $entry eq '..'));
 push @out, $self->file($entry);
 $out[-1] = $self->subdir($entry) if -d $out[-1];
 }
 return @out;
}
sub next {
 my $self = shift;
 unless ($self->{dh}) {
 $self->{dh} = $self->open or Carp::croak( "Can't open directory $self: $!" );
 }

my $next = $self->{dh}->read;
 unless (defined $next) {
 delete $self->{dh};
 return undef;
 }
 my $file = $self->file($next);
 $file = $self->subdir($next) if -d $file;
 return $file;
}
sub subsumes {
 my ($self, $other) = @_;
 die "No second entity given to subsumes()" unless $other;

$other = $self->new($other) unless UNIVERSAL::isa($other, __PACKAGE__);
 $other = $other->dir unless $other->is_dir;

if ($self->is_absolute) {
 $other = $other->absolute;
 } elsif ($other->is_absolute) {
 $self = $self->absolute;
 }

$self = $self->cleanup;
 $other = $other->cleanup;

if ($self->volume) {
 return 0 unless $other->volume eq $self->volume;
 }
 return 1 if "@{$self->{dirs}}" eq "@{$self->new('')->{dirs}}";

my $i = 0;
 while ($i <= $#{ $self->{dirs} }) {
 return 0 if $i > $#{ $other->{dirs} };
 return 0 if $self->{dirs}[$i] ne $other->{dirs}[$i];
 $i++;
 }
 return 1;
}
sub contains {
 my ($self, $other) = @_;
 return !!(-d $self and (-e $other or -l $other) and $self->subsumes($other));
}
1;
}
BEGIN { $INC{q{Path/Class.pm}} = 1;
package Path::Class;
$VERSION = '0.17';
@ISA = qw(Exporter);
@EXPORT = qw(file dir);
@EXPORT_OK = qw(file dir foreign_file foreign_dir);
use strict;
use Exporter;
use Path::Class::File;
use Path::Class::Dir;
sub file { Path::Class::File->new(@_) }
sub dir { Path::Class::Dir ->new(@_) }
sub foreign_file { Path::Class::File->new_foreign(@_) }
sub foreign_dir { Path::Class::Dir ->new_foreign(@_) }
1;
}
BEGIN { $INC{q{AutoLoader.pm}} = 1;
package AutoLoader;
use strict;
use 5.006_001;
our($VERSION, $AUTOLOAD);
my $is_dosish;
my $is_epoc;
my $is_vms;
my $is_macos;
BEGIN {
 $is_dosish = $^O eq 'dos' || $^O eq 'os2' || $^O eq 'MSWin32' || $^O eq 'NetWare';
 $is_epoc = $^O eq 'epoc';
 $is_vms = $^O eq 'VMS';
 $is_macos = $^O eq 'MacOS';
 $VERSION = '5.70';
}
AUTOLOAD {
 my $sub = $AUTOLOAD;
 my $filename = AutoLoader::find_filename( $sub );

my $save = $@;
 local $!; 
 eval { local $SIG{__DIE__}; require $filename };
 if ($@) {
 if (substr($sub,-9) eq '::DESTROY') {
 no strict 'refs';
 *$sub = sub {};
 $@ = undef;
 } elsif ($@ =~ /^Can't locate/) {
 if ($filename =~ s/(\w{12,})\.al$/substr($1,0,11).".al"/e){
 eval { local $SIG{__DIE__}; require $filename };
 }
 }
 if ($@){
 $@ =~ s/ at .*\n//;
 my $error = $@;
 require Carp;
 Carp::croak($error);
 }
 }
 $@ = $save;
 goto &$sub;
}
sub find_filename {
 my $sub = shift;
 my $filename;
 {

my ($pkg,$func) = ($sub =~ /(.*)::([^:]+)$/);
 $pkg =~ s#::#/#g;
 if (defined($filename = $INC{"$pkg.pm"})) {
 if ($is_macos) {
 $pkg =~ tr#/#:#;
 $filename = undef
 unless $filename =~ s#^(.*)$pkg\.pm\z#$1auto:$pkg:$func.al#s;
 } else {
 $filename = undef
 unless $filename =~ s#^(.*)$pkg\.pm\z#$1auto/$pkg/$func.al#s;
 }

if (defined $filename and -r $filename) {
 unless ($filename =~ m|^/|s) {
 if ($is_dosish) {
 unless ($filename =~ m{^([a-z]:)?[\\/]}is) {
 if ($^O ne 'NetWare') {
 $filename = "./$filename";
 } else {
 $filename = "$filename";
 }
 }
 }
 elsif ($is_epoc) {
 unless ($filename =~ m{^([a-z?]:)?[\\/]}is) {
 $filename = "./$filename";
 }
 }
 elsif ($is_vms) {
 $filename = "./$filename";
 }
 elsif (!$is_macos) {
 $filename = "./$filename";
 }
 }
 }
 else {
 $filename = undef;
 }
 }
 unless (defined $filename) {
 $filename = "auto/$sub.al";
 $filename =~ s#::#/#g;
 }
 }
 return $filename;
}
sub import {
 my $pkg = shift;
 my $callpkg = caller;

if ($pkg eq 'AutoLoader') {
 if ( @_ and $_[0] =~ /^&?AUTOLOAD$/ ) {
 no strict 'refs';
 *{ $callpkg . '::AUTOLOAD' } = \&AUTOLOAD;
 }
 }

(my $calldir = $callpkg) =~ s#::#/#g;
 my $path = $INC{$calldir . '.pm'};
 if (defined($path)) {
 my $replaced_okay;
 if ($is_macos) {
 (my $malldir = $calldir) =~ tr#/#:#;
 $replaced_okay = ($path =~ s#^(.*)$malldir\.pm\z#$1auto:$malldir:autosplit.ix#s);
 } else {
 $replaced_okay = ($path =~ s#^(.*)$calldir\.pm\z#$1auto/$calldir/autosplit.ix#);
 }

eval { require $path; } if $replaced_okay;
 if (!$replaced_okay or $@) {
 $path ="auto/$calldir/autosplit.ix";
 eval { require $path; };
 }
 if ($@) {
 my $error = $@;
 require Carp;
 Carp::carp($error);
 }
 }
}
sub unimport {
 my $callpkg = caller;

no strict 'refs';

for my $exported (qw(AUTOLOAD)) {
 my $symname = $callpkg . '::' . $exported;
 undef *{ $symname } if \&{ $symname } == \&{ $exported };
 *{ $symname } = \&{ $symname };
 }
}
1;
}
BEGIN { $INC{q{Class/Container.pm}} = 1;
package Class::Container;
$VERSION = '0.12';
$VERSION = eval $VERSION if $VERSION =~ /_/;
my $HAVE_WEAKEN;
BEGIN {
 eval {
 require Scalar::Util;
 Scalar::Util->import('weaken');
 $HAVE_WEAKEN = 1;
 };

*weaken = sub {} unless defined &weaken;
}
use strict;
use Carp;
use Params::Validate qw(:all);
Params::Validate::validation_options( on_fail => sub { die @_ } );
my %VALID_PARAMS = ();
my %CONTAINED_OBJECTS = ();
my %VALID_CACHE = ();
my %CONTAINED_CACHE = ();
my %DECORATEES = ();
sub new
{
 my $proto = shift;
 my $class = ref($proto) || $proto;
 my $self = bless scalar validate_with
 (
 params => $class->create_contained_objects(@_),
 spec => $class->validation_spec,
 called => "$class->new()",
 ), $class;
 if ($HAVE_WEAKEN) {
 my $c = $self->get_contained_object_spec;
 foreach my $name (keys %$c) {
 next if $c->{$name}{delayed};
 $self->{$name}{container}{container} = $self;
 weaken $self->{$name}{container}{container};
 }
 }
 return $self;
}
sub all_specs
{
 require B::Deparse;
 my %out;

foreach my $class (sort keys %VALID_PARAMS)
 {
 my $params = $VALID_PARAMS{$class};

foreach my $name (sort keys %$params)
 {
 my $spec = $params->{$name};
 my ($type, $default);
 if ($spec->{isa}) {
 my $obj_class;

$type = 'object';

if (exists $CONTAINED_OBJECTS{$class}{$name}) {
 $default = "$CONTAINED_OBJECTS{$class}{$name}{class}->new";
 }
 } else {
 ($type, $default) = ($spec->{parse}, $spec->{default});
 }

if (ref($default) eq 'CODE') {
 $default = 'sub ' . B::Deparse->new->coderef2text($default);
 $default =~ s/\s+/ /g;
 } elsif (ref($default) eq 'ARRAY') {
 $default = '[' . join(', ', map "'$_'", @$default) . ']';
 } elsif (ref($default) eq 'Regexp') {
 $type = 'regex';
 $default =~ s,^\(\?(\w*)-\w*:(.*)\),/$2/$1,;
 $default = "qr$default";
 }
 unless ($type) {
 $type = ($spec->{type} & ARRAYREF ? 'list' :
 $spec->{type} & SCALAR ? 'string' :
 $spec->{type} & CODEREF ? 'code' :
 $spec->{type} & HASHREF ? 'hash' :
 undef); 
 }

my $descr = $spec->{descr} || '(No description available)';
 $out{$class}{valid_params}{$name} = { type => $type,
 pv_type => $spec->{type},
 default => $default,
 descr => $descr,
 required => defined $default || $spec->{optional} ? 0 : 1,
 public => exists $spec->{public} ? $spec->{public} : 1,
 };
 }

$out{$class}{contained_objects} = {};
 next unless exists $CONTAINED_OBJECTS{$class};
 my $contains = $CONTAINED_OBJECTS{$class};

foreach my $name (sort keys %$contains)
 {
 $out{$class}{contained_objects}{$name}
 = {map {$_, $contains->{$name}{$_}} qw(class delayed descr)};
 }
 }

return %out;
}
sub dump_parameters {
 my $self = shift;
 my $class = ref($self) || $self;

my %params;
 foreach my $param (keys %{ $class->validation_spec }) {
 next if $param eq 'container';
 my $spec = $class->validation_spec->{$param};
 if (ref($self) and defined $self->{$param}) {
 $params{$param} = $self->{$param};
 } else {
 $params{$param} = $spec->{default} if exists $spec->{default};
 }
 }

foreach my $name (keys %{ $class->get_contained_object_spec }) {
 next unless ref($self);
 my $contained = ($self->{container}{contained}{$name}{delayed} ?
 $self->delayed_object_class($name) :
 $params{$name});

my $subparams = UNIVERSAL::isa($contained, __PACKAGE__) ? $contained->dump_parameters : {};

my $more = $self->{container}{contained}{$name}{args} || {};
 $subparams->{$_} = $more->{$_} foreach keys %$more;

@params{ keys %$subparams } = values %$subparams;
 delete $params{$name};
 }
 return \%params;
}
sub show_containers {
 my $self = shift;
 my $name = shift;
 my %args = (indent => '', @_);

$name = defined($name) ? "$name -> " : "";

my $out = "$args{indent}$name$self";
 $out .= " (delayed)" if $args{delayed};
 $out .= "\n";
 return $out unless $self->isa(__PACKAGE__);

my $specs = ref($self) ? $self->{container}{contained} : $self->get_contained_object_spec;

while (my ($name, $spec) = each %$specs) {
 my $class = $args{args}{"${name}_class"} || $spec->{class};
 $self->_load_module($class);

if ($class->isa(__PACKAGE__)) {
 $out .= $class->show_containers($name,
 indent => "$args{indent}  ",
 args => $spec->{args},
 delayed => $spec->{delayed});
 } else {
 $out .= "$args{indent}  $name -> $class\n";
 }
 }

return $out;
}
sub _expire_caches {
 %VALID_CACHE = %CONTAINED_CACHE = ();
}
sub valid_params {
 my $class = shift;
 if (@_) {
 $class->_expire_caches;
 $VALID_PARAMS{$class} = @_ == 1 && !defined($_[0]) ? {} : {@_};
 }
 return $VALID_PARAMS{$class} ||= {};
}
sub contained_objects
{
 my $class = shift;
 $class->_expire_caches;
 $CONTAINED_OBJECTS{$class} = {};
 while (@_) {
 my ($name, $spec) = (shift, shift);
 $CONTAINED_OBJECTS{$class}{$name} = ref($spec) ? $spec : { class => $spec };
 }
}
sub _decorator_AUTOLOAD {
 my $self = shift;
 no strict 'vars';
 my ($method) = $AUTOLOAD =~ /([^:]+)$/;
 return if $method eq 'DESTROY';
 die qq{Can't locate object method "$method" via package $self} unless ref($self);
 my $subr = $self->{_decorates}->can($method)
 or die qq{Can't locate object method "$method" via package } . ref($self);
 unshift @_, $self->{_decorates};
 goto $subr;
}
sub _decorator_CAN {
 my ($self, $method) = @_;
 return $self->SUPER::can($method) if $self->SUPER::can($method);
 if (ref $self) {
 return $self->{_decorates}->can($method) if $self->{_decorates};
 return undef;
 } else {
 return $DECORATEES{$self}->can($method);
 }
}
sub decorates {
 my ($class, $super) = @_;

no strict 'refs';
 $super ||= ${$class . '::ISA'}[0];
 *{$class . '::AUTOLOAD'} = \&_decorator_AUTOLOAD;
 *{$class . '::can'} = \&_decorator_CAN;

$DECORATEES{$class} = $super;
 $VALID_PARAMS{$class}{_decorates} = { isa => $super, optional => 1 };
}
sub container {
 my $self = shift;
 die "The ", ref($self), "->container() method requires installation of Scalar::Util" unless $HAVE_WEAKEN;
 return $self->{container}{container};
}
sub call_method {
 my ($self, $name, $method, @args) = @_;

my $class = $self->contained_class($name)
 or die "Unknown contained item '$name'";

$self->_load_module($class);
 return $class->$method( %{ $self->{container}{contained}{$name}{args} }, @args );
}
sub create_contained_objects
{
 my $class = shift;

my $c = $class->get_contained_object_spec;
 return {@_, container => {}} unless %$c or $DECORATEES{$class};

my %args = @_;

if ($DECORATEES{$class}) {
 $args{decorate_class} = [$args{decorate_class}]
 if $args{decorate_class} and !ref($args{decorate_class});
 my $decorate;
 if (my $c = $args{decorate_class}) {
 $decorate = @$c ? shift @$c : undef;
 delete $args{decorate_class} unless @$c;
 }
 $c->{_decorates} = { class => $decorate } if $decorate;
 }
 my $container_stuff = delete($args{container}) || {};

keys %$c; 
 my %contained_args;
 my %to_create;

while (my ($name, $spec) = each %$c) {
 my ($contained_class, $c_args) = $class->_get_contained_args($name, \%args);
 @contained_args{ keys %$c_args } = (); 
 $to_create{$name} = { class => $contained_class,
 args => $c_args };
 }

while (my ($name, $spec) = each %$c) {
 delete $args{"${name}_class"};

if ($spec->{delayed}) {
 $container_stuff->{contained}{$name} = $to_create{$name};
 $container_stuff->{contained}{$name}{delayed} = 1;
 } else {
 $args{$name} ||= $to_create{$name}{class}->new(%{$to_create{$name}{args}});
 $container_stuff->{contained}{$name}{class} = ref $args{$name};
 }
 }
 my $my_spec = $class->validation_spec;
 delete @args{ grep {!exists $my_spec->{$_}} keys %contained_args };
 delete $c->{_decorates} if $DECORATEES{$class};

$args{container} = $container_stuff;
 return \%args;
}
sub create_delayed_object
{
 my ($self, $name) = (shift, shift);
 croak "Unknown delayed item '$name'" unless $self->{container}{contained}{$name}{delayed};

if ($HAVE_WEAKEN) {
 push @_, container => {container => $self};
 weaken $_[-1]->{container};
 }
 return $self->call_method($name, 'new', @_);
}
sub delayed_object_class
{
 my $self = shift;
 my $name = shift;
 croak "Unknown delayed item '$name'"
 unless $self->{container}{contained}{$name}{delayed};

return $self->{container}{contained}{$name}{class};
}
sub contained_class
{
 my ($self, $name) = @_;
 croak "Unknown contained item '$name'"
 unless my $spec = $self->{container}{contained}{$name};
 return $spec->{class};
}
sub delayed_object_params
{
 my ($self, $name) = (shift, shift);
 croak "Unknown delayed object '$name'"
 unless $self->{container}{contained}{$name}{delayed};

if (@_ == 1) {
 return $self->{container}{contained}{$name}{args}{$_[0]};
 }

my %args = @_;

if (keys %args)
 {
 @{ $self->{container}{contained}{$name}{args} }{ keys %args } = values %args;
 }

return %{ $self->{container}{contained}{$name}{args} };
}
sub _get_contained_args
{
 my ($class, $name, $args) = @_;

my $spec = $class->get_contained_object_spec->{$name}
 or croak "Unknown contained object '$name'";

my $contained_class = $args->{"${name}_class"} || $spec->{class};
 croak "Invalid class name '$contained_class'"
 unless $contained_class =~ /^[\w:]+$/;

$class->_load_module($contained_class);
 return ($contained_class, {}) unless $contained_class->isa(__PACKAGE__);

my $allowed = $contained_class->allowed_params($args);

my %contained_args;
 foreach (keys %$allowed) {
 $contained_args{$_} = $args->{$_} if exists $args->{$_};
 }
 return ($contained_class, \%contained_args);
}
sub _load_module {
 my ($self, $module) = @_;

unless ( eval { $module->can('new') } )
 {
 no strict 'refs';
 eval "use $module";
 croak $@ if $@;
 }
}
sub allowed_params
{
 my $class = shift;
 my $args = ref($_[0]) ? shift : {@_};

my $c = $class->get_contained_object_spec;
 my %p = %{ $class->validation_spec };

foreach my $name (keys %$c)
 {
 next if exists $args->{$name};
 my $contained_class;
 if ( exists $args->{"${name}_class"} ) {
 $contained_class = $args->{"${name}_class"};
 $p{"${name}_class"} = { type => SCALAR }; 
 } else {
 $contained_class = $c->{$name}{class};
 }
 $class->_load_module($contained_class);
 next unless $contained_class->can('allowed_params');

my $subparams = $contained_class->allowed_params($args);

foreach (keys %$subparams) {
 $p{$_} ||= $subparams->{$_};
 }
 }

return \%p;
}
sub _iterate_ISA {
 my ($class, $look_in, $cache_in, $add) = @_;

return $cache_in->{$class} if $cache_in->{$class};

my %out;

no strict 'refs';
 foreach my $superclass (@{ "${class}::ISA" }) {
 next unless $superclass->isa(__PACKAGE__);
 my $superparams = $superclass->_iterate_ISA($look_in, $cache_in, $add);
 @out{keys %$superparams} = values %$superparams;
 }
 if (my $x = $look_in->{$class}) {
 @out{keys %$x} = values %$x;
 }

@out{keys %$add} = values %$add if $add;

return $cache_in->{$class} = \%out;
}
sub get_contained_object_spec {
 return (ref($_[0]) || $_[0])->_iterate_ISA(\%CONTAINED_OBJECTS, \%CONTAINED_CACHE);
}
sub validation_spec {
 return (ref($_[0]) || $_[0])->_iterate_ISA(\%VALID_PARAMS, \%VALID_CACHE, { container => {type => HASHREF} });
}
1;
}
BEGIN { $INC{q{Class/Data/Inheritable.pm}} = 1;
package Class::Data::Inheritable;
use strict qw(vars subs);
use vars qw($VERSION);
$VERSION = '0.08';
sub mk_classdata {
 my ($declaredclass, $attribute, $data) = @_;

if( ref $declaredclass ) {
 require Carp;
 Carp::croak("mk_classdata() is a class method, not an object method");
 }

my $accessor = sub {
 my $wantclass = ref($_[0]) || $_[0];

return $wantclass->mk_classdata($attribute)->(@_)
 if @_>1 && $wantclass ne $declaredclass;

$data = $_[1] if @_>1;
 return $data;
 };

my $alias = "_${attribute}_accessor";
 *{$declaredclass.'::'.$attribute} = $accessor;
 *{$declaredclass.'::'.$alias} = $accessor;
}
1;
}
BEGIN { $INC{q{Config/Any.pm}} = 1;
package Config::Any;
use strict;
use warnings;
use Carp;
use Module::Pluggable::Object ();
our $VERSION = '0.17';
sub load_files {
 my ( $class, $args ) = @_;

unless ( $args && exists $args->{ files } ) {
 warn "No files specified!";
 return;
 }

return $class->_load( $args );
}
sub load_stems {
 my ( $class, $args ) = @_;

unless ( $args && exists $args->{ stems } ) {
 warn "No stems specified!";
 return;
 }

my $stems = delete $args->{ stems };
 my @files;
 for my $s ( @$stems ) {
 for my $ext ( $class->extensions ) {
 push @files, "$s.$ext";
 }
 }

$args->{ files } = \@files;
 return $class->_load( $args );
}
sub _load {
 my ( $class, $args ) = @_;
 croak "_load requires a arrayref of file paths" unless $args->{ files };

my $force = defined $args->{ force_plugins };
 if ( !$force and !defined $args->{ use_ext } ) {
 warn
 "use_ext argument was not explicitly set, as of 0.09, this is true by default";
 $args->{ use_ext } = 1;
 }
 my @plugins = $force
 ? map { eval "require $_;"; $_; } @{ $args->{ force_plugins } }
 : $class->plugins;
 my ( %extension_lut, $extension_re );
 my $use_ext_lut = !$force && $args->{ use_ext };
 if ( $use_ext_lut ) {
 for my $plugin ( @plugins ) {
 for ( $plugin->extensions ) {
 $extension_lut{ $_ } ||= [];
 push @{ $extension_lut{ $_ } }, $plugin;
 }
 }

$extension_re = join( '|', keys %extension_lut );
 }
 my $base_class = __PACKAGE__;
 my %loader_args;
 for my $plugin ( @plugins ) {
 $plugin =~ m{^$base_class\::(.+)};
 $loader_args{ $plugin } = $args->{ driver_args }->{ $1 } || {};
 }

my @results;

for my $filename ( @{ $args->{ files } } ) {
 next unless -f $filename;

my @try_plugins = @plugins;

if ( $use_ext_lut ) {
 $filename =~ m{\.($extension_re)\z};

if ( !$1 ) {
 $filename =~ m{\.([^.]+)\z};
 croak "There are no loaders available for .${1} files";
 }

@try_plugins = @{ $extension_lut{ $1 } };
 }
 my $supported = $use_ext_lut ? 0 : 1;
 for my $loader ( @try_plugins ) {
 next unless $loader->is_supported;
 $supported = 1;
 my @configs
 = eval { $loader->load( $filename, $loader_args{ $loader } ); };
 croak "Error parsing $filename: $@" if $@ and $use_ext_lut;
 next if $@ or !@configs;
 if ( $args->{ filter } ) {
 $args->{ filter }->( $_ ) for @configs;
 }

push @results,
 { $filename => @configs == 1 ? $configs[ 0 ] : \@configs };
 last;
 }

if ( !$supported ) {
 croak
 "Cannot load $filename: required support modules are not available.\nPlease install "
 . join( " OR ", map { _support_error( $_ ) } @try_plugins );
 }
 }

if ( defined $args->{ flatten_to_hash } ) {
 my %flattened = map { %$_ } @results;
 return \%flattened;
 }

return \@results;
}
sub _support_error {
 my $module = shift;
 if ( $module->can( 'requires_all_of' ) ) {
 return join( ' and ',
 map { ref $_ ? join( ' ', @$_ ) : $_ } $module->requires_all_of );
 }
 if ( $module->can( 'requires_any_of' ) ) {
 return 'one of '
 . join( ' or ',
 map { ref $_ ? join( ' ', @$_ ) : $_ } $module->requires_any_of );
 }
}
sub finder {
 my $class = shift;
 my $finder = Module::Pluggable::Object->new(
 search_path => [ __PACKAGE__ ],
 except => [ __PACKAGE__ . '::Base' ],
 require => 1
 );
 return $finder;
}
sub plugins {
 my $class = shift;
 return grep { $_->isa( 'Config::Any::Base' ) } $class->finder->plugins;
}
sub extensions {
 my $class = shift;
 my @ext
 = map { $_->extensions } $class->plugins;
 return wantarray ? @ext : \@ext;
}
"Drink more beer";
}
BEGIN { $INC{q{Config/Any/Base.pm}} = 1;
package Config::Any::Base;
use strict;
use warnings;
sub is_supported {
 my ( $class ) = shift;
 if ( $class->can( 'requires_all_of' ) ) {
 eval join( '', map { _require_line( $_ ) } $class->requires_all_of );
 return $@ ? 0 : 1;
 }
 if ( $class->can( 'requires_any_of' ) ) {
 for ( $class->requires_any_of ) {
 eval _require_line( $_ );
 return 1 unless $@;
 }
 return 0;
 }
 return 1;
}
sub _require_line {
 my ( $input ) = shift;
 my ( $module, $version ) = ( ref $input ? @$input : $input );
 return "require $module;"
 . ( $version ? "${module}->VERSION('${version}');" : '' );
}
1;
}
BEGIN { $INC{q{Config/Any/JSON.pm}} = 1;
package Config::Any::JSON;
use strict;
use warnings;
use base 'Config::Any::Base';
sub extensions {
 return qw(json jsn);
}
sub load {
 my $class = shift;
 my $file = shift;

open( my $fh, $file ) or die $!;
 my $content = do { local $/; <$fh> };
 close $fh;

eval { require JSON::XS; };
 unless( $@ ) {
 return JSON::XS::decode_json( $content );
 }

eval { require JSON::Syck; };
 unless( $@ ) {
 return JSON::Syck::Load( $content );
 }

require JSON;
 eval { JSON->VERSION( 2 ); };
 return $@ ? JSON::jsonToObj( $content ) : JSON::from_json( $content );
}
sub requires_any_of { 'JSON::XS', 'JSON::Syck', 'JSON' }
1;
}
BEGIN { $INC{q{Data/AsObject.pm}} = 1;
package Data::AsObject;
use warnings;
use strict;
use Carp;
use Scalar::Util qw(reftype blessed);
use Data::AsObject::Hash;
use Data::AsObject::Array;
use base 'Exporter';
our @EXPORT = qw(dao);
our $VERSION = '0.05';
our $__check_type = sub {
 my $data = shift;
 return unless $data;

my $type = reftype($data);

if (defined $type) {
 if ( $type eq "ARRAY" && ( !blessed($data) || ref($data) eq "Data::AsObject::Array" ) ) {
 return "ARRAY";
 } elsif ( $type eq "HASH" && ( !blessed($data) || ref($data) eq "Data::AsObject::Hash" ) ) {
 return "HASH";
 } else {
 return "";
 }
 } else {
 return "";
 }
};
sub dao {
 my @args = @_;
 my @result;

foreach my $data (@args) {

my $type = reftype($data);
 my $dao;

if ($type eq "ARRAY") {
 $dao = bless $data, "Data::AsObject::Array";
 } elsif ($type eq "HASH") {
 $dao = bless $data, "Data::AsObject::Hash";
 } else {
 carp "Invalid argument to dao: must be hashref or arrayref!";
 $dao = undef;
 }
 push @result, $dao;
 }

return wantarray ? @result : $result[0];
}
1;
}
BEGIN { $INC{q{Data/AsObject/Array.pm}} = 1;
package Data::AsObject::Array;
use strict;
use warnings;
use Carp;
use Scalar::Util qw(reftype blessed);
use Data::AsObject;
sub get {
 my $self = shift;
 my $index = shift;
 if (defined $index) {
 if ( exists $self->[$index] ) {
 my $data = $self->[$index];

if ( $Data::AsObject::__check_type->($data) eq "ARRAY" ) {
 bless $data, "Data::AsObject::Array";
 return wantarray ? $data->all : $data;
 } elsif ( $Data::AsObject::__check_type->($data) eq "HASH" ) {
 return wantarray ? %{$data} : bless $data, "Data::AsObject::Hash";
 } else {
 return $data;
 }
 } else {
 carp "Attempting to access non-existing array index [$index]!";
 return;
 }
 } else {
 carp "Array accessor get requires index argument!"
 }
}
sub all {
 my $self = shift;

return map { $Data::AsObject::__check_type->($_) ? Data::AsObject::dao($_) : $_} @{$self};
}
1;
}
BEGIN { $INC{q{Data/AsObject/Hash.pm}} = 1;
package Data::AsObject::Hash;
use strict;
use warnings;
use Carp;
use Scalar::Util qw(reftype blessed);
use Data::AsObject ();
use Data::AsObject::Array ();
our $AUTOLOAD;
sub AUTOLOAD {
 my $self = shift;
 my $index = shift;

my $key = $AUTOLOAD;
 $key =~ s/.*:://;
 undef $AUTOLOAD;

if ($key eq "can" && defined $index && $index != /\d+/) {
 return undef;
 }

if ($key eq "isa" && defined $index && $index != /\d+/) {
 $index eq "Data::AsObject::Hash" or $index eq "UNIVERSAL"
 ? return 1
 : return 0;
 }

my $data;

if ( exists $self->{$key} ) {
 $data = $self->{$key};
 } else {
 my $key_regex = $key;
 my $has_colon_or_dash = $key_regex =~ s/_/[-:]/g;
 my @matches = grep(/$key_regex/, keys %$self) if $has_colon_or_dash;

if (@matches == 1) {
 $data = $self->{$matches[0]};
 } elsif (@matches > 1) {
 carp "Attempt to disambiguate hash key $key returns multiple matches!";
 return;
 } else {
 carp "Attempting to access non-existing hash key $key!" unless $key eq "DESTROY";
 return;
 }
 }

if ( $data ) {
 if (
 defined $index
 && $index =~ /\d+/
 && $Data::AsObject::__check_type->($data) eq "ARRAY"
 && exists $data->[$index]
 )
 {
 $data = $data->[$index];
 }

if ( $Data::AsObject::__check_type->($data) eq "ARRAY" ) {
 bless $data, "Data::AsObject::Array";
 return wantarray ? $data->all : $data;
 } elsif ( $Data::AsObject::__check_type->($data) eq "HASH" ) {
 return wantarray ? %{$data} : bless $data, "Data::AsObject::Hash";
 } else {
 return $data;
 }
 }
}
1;
}
BEGIN { $INC{q{Devel/StackTrace.pm}} = 1;
package Devel::StackTrace;
use 5.006;
use strict;
use warnings;
use File::Spec;
use Scalar::Util qw(blessed);
use overload
 '""' => \&as_string,
 fallback => 1;
our $VERSION = '1.22';
sub new
{
 my $class = shift;
 my %p = @_;
 $p{no_refs} = delete $p{no_object_refs}
 if exists $p{no_object_refs};

my $self =
 bless { index => undef,
 frames => [],
 raw => [],
 %p,
 }, $class;

$self->_record_caller_data;

return $self;
}
sub _record_caller_data
{
 my $self = shift;
 my $x = 1;
 while ( my @c =
 do { package DB; @DB::args = (); caller($x++) } )
 {
 my @a = @DB::args;

if ( $self->{no_refs} )
 {
 @a = map { ref $_ ? $self->_ref_to_string($_) : $_ } @a;
 }

push @{ $self->{raw} },
 { caller => \@c,
 args => \@a,
 };
 }
}
sub _ref_to_string
{
 my $self = shift;
 my $ref = shift;

return overload::AddrRef($ref)
 if blessed $ref && $ref->isa('Exception::Class::Base');

return overload::AddrRef($ref) unless $self->{respect_overload};

local $@;
 local $SIG{__DIE__};

my $str = eval { $ref . '' };

return $@ ? overload::AddrRef($ref) : $str;
}
sub _make_frames
{
 my $self = shift;

my $filter = $self->_make_frame_filter;

my $raw = delete $self->{raw};
 for my $r ( @{$raw} )
 {
 next unless $filter->($r);

$self->_add_frame( $r->{caller}, $r->{args} );
 }
}
my $default_filter = sub { 1 };
sub _make_frame_filter
{
 my $self = shift;

my (@i_pack_re, %i_class);
 if ( $self->{ignore_package} )
 {
 $self->{ignore_package} =
 [ $self->{ignore_package} ] unless UNIVERSAL::isa( $self->{ignore_package}, 'ARRAY' );

@i_pack_re = map { ref $_ ? $_ : qr/^\Q$_\E$/ } @{ $self->{ignore_package} };
 }

my $p = __PACKAGE__;
 push @i_pack_re, qr/^\Q$p\E$/;

if ( $self->{ignore_class} )
 {
 $self->{ignore_class} = [ $self->{ignore_class} ] unless ref $self->{ignore_class};
 %i_class = map {$_ => 1} @{ $self->{ignore_class} };
 }

my $user_filter = $self->{frame_filter};

return sub
 {
 return 0 if grep { $_[0]{caller}[0] =~ /$_/ } @i_pack_re;
 return 0 if grep { $_[0]{caller}[0]->isa($_) } keys %i_class;

if ( $user_filter )
 {
 return $user_filter->( $_[0] );
 }

return 1;
 };
}
sub _add_frame
{
 my $self = shift;
 my $c = shift;
 my $args = shift;
 push @$c, (undef, undef) if scalar @$c == 6;

if ( $self->{no_refs} )
 {
 }

push @{ $self->{frames} },
 Devel::StackTraceFrame->new( $c, $args,
 $self->{respect_overload}, $self->{max_arg_length} );
}
sub next_frame
{
 my $self = shift;
 $self->{index} = -1 unless defined $self->{index};

my @f = $self->frames;
 if ( defined $f[ $self->{index} + 1 ] )
 {
 return $f[ ++$self->{index} ];
 }
 else
 {
 $self->{index} = undef;
 return undef;
 }
}
sub prev_frame
{
 my $self = shift;

my @f = $self->frames;
 $self->{index} = scalar @f unless defined $self->{index};

if ( defined $f[ $self->{index} - 1 ] && $self->{index} >= 1 )
 {
 return $f[ --$self->{index} ];
 }
 else
 {
 $self->{index} = undef;
 return undef;
 }
}
sub reset_pointer
{
 my $self = shift;

$self->{index} = undef;
}
sub frames
{
 my $self = shift;

$self->_make_frames if $self->{raw};

return @{ $self->{frames} };
}
sub frame
{
 my $self = shift;
 my $i = shift;

return unless defined $i;

return ( $self->frames )[$i];
}
sub frame_count
{
 my $self = shift;

return scalar ( $self->frames );
}
sub as_string
{
 my $self = shift;

my $st = '';
 my $first = 1;
 foreach my $f ( $self->frames )
 {
 $st .= $f->as_string($first) . "\n";
 $first = 0;
 }

return $st;
}
package
 Devel::StackTraceFrame;
use strict;
use warnings;
our $VERSION = $Devel::StackTrace::VERSION;
BEGIN
{
 no strict 'refs';
 foreach my $f ( qw(package filename line subroutine hasargs wantarray evaltext is_require hints bitmask args) )
 {
 next if $f eq 'args';
 *{$f} = sub { my $s = shift; return $s->{$f} };
 }
}
{
 my @fields =
 ( qw(package filename line subroutine hasargs wantarray evaltext is_require hints bitmask) );

sub new
 {
 my $proto = shift;
 my $class = ref $proto || $proto;

my $self = bless {}, $class;

@{ $self }{ @fields } = @{$_[0]};
 $self->{filename} = File::Spec->canonpath( $self->{filename} );

$self->{args} = $_[1];

$self->{respect_overload} = $_[2];

$self->{max_arg_length} = $_[3];

return $self;
 }
}
sub args
{
 my $self = shift;

return @{ $self->{args} };
}
sub as_string
{
 my $self = shift;
 my $first = shift;

my $sub = $self->subroutine;
 if ($first)
 {
 $sub = 'Trace begun';
 }
 else
 {
 if (my $eval = $self->evaltext)
 {
 if ($self->is_require)
 {
 $sub = "require $eval";
 }
 else
 {
 $eval =~ s/([\\\'])/\\$1/g;
 $sub = "eval '$eval'";
 }
 }
 elsif ($sub eq '(eval)')
 {
 $sub = 'eval {...}';
 }
 if ( my @a = $self->args )
 {
 for (@a)
 {
 $_ = "undef", next unless defined $_;
 $_ = $self->Devel::StackTrace::_ref_to_string($_)
 if ref $_;

eval
 {
 if ( $self->{max_arg_length}
 && length $_ > $self->{max_arg_length} )
 {
 substr( $_, $self->{max_arg_length} ) = '...';
 }

s/'/\\'/g;
 $_ = "'$_'" unless /^-?[\d.]+$/;
 s/([\200-\377])/sprintf("M-%c",ord($1)&0177)/eg;
 s/([\0-\37\177])/sprintf("^%c",ord($1)^64)/eg;
 };

if ( my $e = $@ )
 {
 $_ = $e =~ /malformed utf-8/i ? '(bad utf-8)' : '?';
 }
 }
 $sub .= '(' . join(', ', @a) . ')';
 $sub .= ' called';
 }
 }

return "$sub at " . $self->filename . ' line ' . $self->line;
}
1;
}
BEGIN { $INC{q{Exception/Class/Base.pm}} = 1;
package Exception::Class::Base;
use strict;
use warnings;
our $VERSION = '1.20';
use Class::Data::Inheritable;
use Devel::StackTrace 1.20;
use base qw(Class::Data::Inheritable);
BEGIN
{
 __PACKAGE__->mk_classdata('Trace');
 __PACKAGE__->mk_classdata('NoRefs');
 __PACKAGE__->NoRefs(1);

__PACKAGE__->mk_classdata('RespectOverload');
 __PACKAGE__->RespectOverload(0);

__PACKAGE__->mk_classdata('MaxArgLength');
 __PACKAGE__->MaxArgLength(0);

sub Fields { () }
}
use overload
 bool => sub { 1 },
 '""' => 'as_string',
 fallback => 1;
BEGIN
{
 my @fields = qw(message pid uid euid gid egid time trace);

foreach my $f (@fields)
 {
 my $sub = sub { my $s = shift; return $s->{$f}; };

no strict 'refs';
 *{$f} = $sub;
 }
 *error = \&message;

my %trace_fields =
 ( package => 'package',
 file => 'filename',
 line => 'line',
 );

while ( my ( $f, $m ) = each %trace_fields )
 {
 my $sub = sub
 {
 my $s = shift;
 return $s->{$f} if exists $s->{$f};

my $frame = $s->trace->frame(0);

return $s->{$f} = $frame ? $frame->$m() : undef;
 };
 no strict 'refs';
 *{$f} = $sub;
 }
}
1;
sub Classes { Exception::Class::Classes() }
sub throw
{
 my $proto = shift;

$proto->rethrow if ref $proto;

die $proto->new(@_);
}
sub rethrow
{
 my $self = shift;

die $self;
}
sub new
{
 my $proto = shift;
 my $class = ref $proto || $proto;

my $self = bless {}, $class;

$self->_initialize(@_);

return $self;
}
sub _initialize
{
 my $self = shift;
 my %p = @_ == 1 ? ( error => $_[0] ) : @_;

$self->{message} = $p{message} || $p{error} || '';

$self->{show_trace} = $p{show_trace} if exists $p{show_trace};
 $self->{time} = CORE::time();
 $self->{pid} = $$;
 $self->{uid} = $<;
 $self->{euid} = $>;
 $self->{gid} = $(;
 $self->{egid} = $);

my @ignore_class = (__PACKAGE__);
 my @ignore_package = 'Exception::Class';

if ( my $i = delete $p{ignore_class} )
 {
 push @ignore_class, ( ref($i) eq 'ARRAY' ? @$i : $i );
 }

if ( my $i = delete $p{ignore_package} )
 {
 push @ignore_package, ( ref($i) eq 'ARRAY' ? @$i : $i );
 }

$self->{trace} =
 Devel::StackTrace->new( ignore_class => \@ignore_class,
 ignore_package => \@ignore_package,
 no_refs => $self->NoRefs,
 respect_overload => $self->RespectOverload,
 max_arg_length => $self->MaxArgLength,
 );

my %fields = map { $_ => 1 } $self->Fields;
 while ( my ($key, $value) = each %p )
 {
 next if $key =~ /^(?:error|message|show_trace)$/;

if ( $fields{$key})
 {
 $self->{$key} = $value;
 }
 else
 {
 Exception::Class::Base->throw
 ( error =>
 "unknown field $key passed to constructor for class " . ref $self );
 }
 }
}
sub description
{
 return 'Generic exception';
}
sub show_trace
{
 my $self = shift;

if (@_)
 {
 $self->{show_trace} = shift;
 }

return exists $self->{show_trace} ? $self->{show_trace} : $self->Trace;
}
sub as_string
{
 my $self = shift;

my $str = $self->full_message;
 $str .= "\n\n" . $self->trace->as_string
 if $self->show_trace;

return $str;
}
sub full_message { $_[0]->{message} }
eval <<'EOF' if $] == 5.006;
sub isa
{
    my ($inheritor, $base) = @_;
    $inheritor = ref($inheritor) if ref($inheritor);

    my %seen;

    no strict 'refs';
    my @parents = ($inheritor, @{"$inheritor\::ISA"});
    while (my $class = shift @parents)
    {
        return 1 if $class eq $base;

        push @parents, grep {!$seen{$_}++} @{"$class\::ISA"};
    }
    return 0;
}
EOF
sub caught
{
 return Exception::Class->caught(shift);
}
1;
}
BEGIN { $INC{q{Exception/Class.pm}} = 1;
package Exception::Class;
use 5.008001;
use strict;
use Exception::Class::Base;
use Scalar::Util qw(blessed);
our $BASE_EXC_CLASS;
BEGIN { $BASE_EXC_CLASS ||= 'Exception::Class::Base'; }
our $VERSION = '1.29';
our %CLASSES;
sub import
{
 my $class = shift;

local $Exception::Class::Caller = caller();

my %c;

my %needs_parent;
 while (my $subclass = shift)
 {
 my $def = ref $_[0] ? shift : {};
 $def->{isa} = $def->{isa} ? ( ref $def->{isa} ? $def->{isa} : [$def->{isa}] ) : [];

$c{$subclass} = $def;
 }
 MAKE_CLASSES:
 foreach my $subclass ( sort { length $a <=> length $b } keys %c )
 {
 my $def = $c{$subclass};
 next if $CLASSES{$subclass};

{
 no strict 'refs';
 foreach my $parent (@{ $def->{isa} })
 {
 unless ( keys %{"$parent\::"} )
 {
 $needs_parent{$subclass} = { parents => $def->{isa},
 def => $def };
 next MAKE_CLASSES;
 }
 }
 }

$class->_make_subclass( subclass => $subclass,
 def => $def || {},
 );
 }

foreach my $subclass (keys %needs_parent)
 {
 my %seen;
 $class->_make_parents( \%needs_parent, $subclass, \%seen );
 }
}
sub _make_parents
{
 my $class = shift;
 my $needs = shift;
 my $subclass = shift;
 my $seen = shift;
 my $child = shift; 

no strict 'refs';
 die "Class $subclass appears to be a typo as it is only specified in the 'isa' param for $child\n"
 unless exists $needs->{$subclass} || $CLASSES{$subclass} || keys %{"$subclass\::"};

foreach my $c ( @{ $needs->{$subclass}{parents} } )
 {
 next if $CLASSES{$c} || keys %{"$c\::"};

die "There appears to be some circularity involving $subclass\n"
 if $seen->{$subclass};

$seen->{$subclass} = 1;

$class->_make_parents( $needs, $c, $seen, $subclass );
 }

return if $CLASSES{$subclass} || keys %{"$subclass\::"};

$class->_make_subclass( subclass => $subclass,
 def => $needs->{$subclass}{def} );
}
sub _make_subclass
{
 my $class = shift;
 my %p = @_;

my $subclass = $p{subclass};
 my $def = $p{def};

my $isa;
 if ($def->{isa})
 {
 $isa = ref $def->{isa} ? join ' ', @{ $def->{isa} } : $def->{isa};
 }
 $isa ||= $BASE_EXC_CLASS;

my $version_name = 'VERSION';

my $code = <<"EOPERL";
package $subclass;

use base qw($isa);

our \$$version_name = '1.1';

1;

EOPERL

if ($def->{description})
 {
 (my $desc = $def->{description}) =~ s/([\\\'])/\\$1/g;
 $code .= <<"EOPERL";
sub description
{
    return '$desc';
}
EOPERL
 }

my @fields;
 if ( my $fields = $def->{fields} )
 {
 @fields = UNIVERSAL::isa($fields, 'ARRAY') ? @$fields : $fields;

$code .=
 "sub Fields { return (\$_[0]->SUPER::Fields, " .
 join(", ", map { "'$_'" } @fields) . ") }\n\n";

foreach my $field (@fields)
 {
 $code .= sprintf("sub %s { \$_[0]->{%s} }\n", $field, $field);
 }
 }

if ( my $alias = $def->{alias} )
 {
 die "Cannot make alias without caller"
 unless defined $Exception::Class::Caller;

no strict 'refs';
 *{"$Exception::Class::Caller\::$alias"} = sub { $subclass->throw(@_) };
 }

eval $code;

die $@ if $@;

$CLASSES{$subclass} = 1;
}
sub caught
{
 my $e = $@;

return $e unless $_[1];

return unless blessed($e) && $e->isa( $_[1] );
 return $e;
}
sub Classes { sort keys %Exception::Class::CLASSES }
1;
}
BEGIN { $INC{q{File/Temp.pm}} = 1;
package File::Temp;
use 5.004;
use strict;
use Carp;
use File::Spec 0.8;
use File::Path qw/rmtree/;
use Fcntl 1.03;
use IO::Seekable; 
use Errno;
require VMS::Stdio if $^O eq 'VMS';
eval { require Carp::Heavy; };
require Symbol if $] < 5.006;
use base qw/IO::Handle IO::Seekable/;
use overload '""' => "STRINGIFY", fallback => 1;
use vars qw($VERSION @EXPORT_OK %EXPORT_TAGS $DEBUG $KEEP_ALL);
$DEBUG = 0;
$KEEP_ALL = 0;
use base qw/Exporter/;
@EXPORT_OK = qw{tempfile tempdir tmpnam tmpfile mktemp mkstemp mkstemps mkdtemp unlink0 cleanup SEEK_SET SEEK_CUR SEEK_END};
%EXPORT_TAGS = (
 'POSIX' => [qw/tmpnam tmpfile/],
 'mktemp' => [qw/mktemp mkstemp mkstemps mkdtemp/],
 'seekable' => [qw/SEEK_SET SEEK_CUR SEEK_END/],
 );
Exporter::export_tags('POSIX','mktemp','seekable');
$VERSION = '0.22';
my @CHARS = (qw/A B C D E F G H I J K L M N O P Q R S T U V W X Y Z a b c d e f g h i j k l m n o p q r s t u v w x y z 0 1 2 3 4 5 6 7 8 9 _/);
use constant MAX_TRIES => 1000;
use constant MINX => 4;
use constant TEMPXXX => 'X' x 10;
use constant STANDARD => 0;
use constant MEDIUM => 1;
use constant HIGH => 2;
my $OPENFLAGS = O_CREAT | O_EXCL | O_RDWR;
my $LOCKFLAG;
unless ($^O eq 'MacOS') {
 for my $oflag (qw/NOFOLLOW BINARY LARGEFILE NOINHERIT/) {
 my ($bit, $func) = (0, "Fcntl::O_" . $oflag);
 no strict 'refs';
 $OPENFLAGS |= $bit if eval {
 local $SIG{__DIE__} = sub {};
 local $SIG{__WARN__} = sub {};
 $bit = &$func();
 1;
 };
 }
 $LOCKFLAG = eval {
 local $SIG{__DIE__} = sub {};
 local $SIG{__WARN__} = sub {};
 &Fcntl::O_EXLOCK();
 };
}
my $OPENTEMPFLAGS = $OPENFLAGS;
unless ($^O eq 'MacOS') {
 for my $oflag (qw/TEMPORARY/) {
 my ($bit, $func) = (0, "Fcntl::O_" . $oflag);
 local($@);
 no strict 'refs';
 $OPENTEMPFLAGS |= $bit if eval {
 local $SIG{__DIE__} = sub {};
 local $SIG{__WARN__} = sub {};
 $bit = &$func();
 1;
 };
 }
}
my %FILES_CREATED_BY_OBJECT;
sub _gettemp {

croak 'Usage: ($fh, $name) = _gettemp($template, OPTIONS);'
 unless scalar(@_) >= 1;
 my $tempErrStr;
 my %options = (
 "open" => 0,
 "mkdir" => 0,
 "suffixlen" => 0,
 "unlink_on_close" => 0,
 "use_exlock" => 1,
 "ErrStr" => \$tempErrStr,
 );
 my $template = shift;
 if (ref($template)) {
 carp "File::Temp::_gettemp: template must not be a reference";
 return ();
 }
 if (scalar(@_) % 2 != 0) {
 carp "File::Temp::_gettemp: Must have even number of options";
 return ();
 }
 %options = (%options, @_) if @_;
 ${$options{ErrStr}} = undef;
 if ($options{"open"} && $options{"mkdir"}) {
 ${$options{ErrStr}} = "doopen and domkdir can not both be true\n";
 return ();
 }
 my $start = length($template) - 1 - $options{"suffixlen"};

if (substr($template, $start - MINX + 1, MINX) ne 'X' x MINX) {
 ${$options{ErrStr}} = "The template must end with at least ".
 MINX . " 'X' characters\n";
 return ();
 }

my $path = _replace_XX($template, $options{"suffixlen"});

my ($volume, $directories, $file);
 my $parent; 
 if ($options{"mkdir"}) {
 ($volume, $directories, $file) = File::Spec->splitpath( $path, 1);
 my @dirs = File::Spec->splitdir($directories);
 if ($#dirs == 0) {
 $parent = File::Spec->curdir;
 } else {

if ($^O eq 'VMS') { 
 $parent = File::Spec->catdir($volume, @dirs[0..$#dirs-1]);
 $parent = 'sys$disk:[]' if $parent eq '';
 } else {
 $parent = File::Spec->catdir(@dirs[0..$#dirs-1]);
 $parent = File::Spec->catpath($volume, $parent, '');
 }

}

} else {
 ($volume, $directories, $file) = File::Spec->splitpath( $path );
 $parent = File::Spec->catpath($volume,$directories,'');
 $parent = File::Spec->curdir
 unless $directories ne '';

}

unless (-e $parent) {
 ${$options{ErrStr}} = "Parent directory ($parent) does not exist";
 return ();
 }
 unless (-d $parent) {
 ${$options{ErrStr}} = "Parent directory ($parent) is not a directory";
 return ();
 }

if (File::Temp->safe_level == MEDIUM) {
 my $safeerr;
 unless (_is_safe($parent,\$safeerr)) {
 ${$options{ErrStr}} = "Parent directory ($parent) is not safe ($safeerr)";
 return ();
 }
 } elsif (File::Temp->safe_level == HIGH) {
 my $safeerr;
 unless (_is_verysafe($parent, \$safeerr)) {
 ${$options{ErrStr}} = "Parent directory ($parent) is not safe ($safeerr)";
 return ();
 }
 }
 for (my $i = 0; $i < MAX_TRIES; $i++) {
 if ($options{"open"}) {
 my $fh;
 if ($] < 5.006) {
 $fh = &Symbol::gensym;
 }
 local $^F = 2;
 my $open_success = undef;
 if ( $^O eq 'VMS' and $options{"unlink_on_close"} && !$KEEP_ALL) {
 $fh = VMS::Stdio::vmssysopen($path, $OPENFLAGS, 0600, 'fop=dlt');
 $open_success = $fh;
 } else {
 my $flags = ( ($options{"unlink_on_close"} && !$KEEP_ALL) ?
 $OPENTEMPFLAGS :
 $OPENFLAGS );
 $flags |= $LOCKFLAG if (defined $LOCKFLAG && $options{use_exlock});
 $open_success = sysopen($fh, $path, $flags, 0600);
 }
 if ( $open_success ) {
 chmod(0600, $path);
 return ($fh, $path);

} else {
 unless ($!{EEXIST}) {
 ${$options{ErrStr}} = "Could not create temp file $path: $!";
 return ();
 }

}
 } elsif ($options{"mkdir"}) {
 if (mkdir( $path, 0700)) {
 chmod(0700, $path);

return undef, $path;
 } else {
 unless ($!{EEXIST}) {
 ${$options{ErrStr}} = "Could not create directory $path: $!";
 return ();
 }

}

} else {

return (undef, $path) unless -e $path;

}

my $original = $path;
 my $counter = 0; 
 my $MAX_GUESS = 50;

do {
 $path = _replace_XX($template, $options{"suffixlen"});

$counter++;

} until ($path ne $original || $counter > $MAX_GUESS);
 if ($counter > $MAX_GUESS) {
 ${$options{ErrStr}} = "Tried to get a new temp name different to the previous value $MAX_GUESS times.\nSomething wrong with template?? ($template)";
 return ();
 }

}
 ${ $options{ErrStr} } = "Have exceeded the maximum number of attempts ("
 . MAX_TRIES . ") to open temp file/dir";

return ();
}
sub _replace_XX {

croak 'Usage: _replace_XX($template, $ignore)'
 unless scalar(@_) == 2;

my ($path, $ignore) = @_;
 my $end = ( $] >= 5.006 ? "\\z" : "\\Z" );

if ($ignore) {
 substr($path, 0, - $ignore) =~ s/X(?=X*$end)/$CHARS[ int( rand( @CHARS ) ) ]/ge;
 } else {
 $path =~ s/X(?=X*$end)/$CHARS[ int( rand( @CHARS ) ) ]/ge;
 }
 return $path;
}
sub _force_writable {
 my $file = shift;
 chmod 0600, $file;
}
sub _is_safe {

my $path = shift;
 my $err_ref = shift;
 my @info = stat($path);
 unless (scalar(@info)) {
 $$err_ref = "stat(path) returned no values";
 return 0;
 }
 ;
 return 1 if $^O eq 'VMS'; 
 if ($info[4] > File::Temp->top_system_uid && $info[4] != $>) {

Carp::cluck(sprintf "uid=$info[4] topuid=%s euid=$> path='$path'",
 File::Temp->top_system_uid);

$$err_ref = "Directory owned neither by root nor the current user"
 if ref($err_ref);
 return 0;
 }
 if (($info[2] & &Fcntl::S_IWGRP) || 
 ($info[2] & &Fcntl::S_IWOTH) ) { 
 unless (-d $path) {
 $$err_ref = "Path ($path) is not a directory"
 if ref($err_ref);
 return 0;
 }
 unless (-k $path) {
 $$err_ref = "Sticky bit not set on $path when dir is group|world writable"
 if ref($err_ref);
 return 0;
 }
 }

return 1;
}
sub _is_verysafe {
 require POSIX;

my $path = shift;
 print "_is_verysafe testing $path\n" if $DEBUG;
 return 1 if $^O eq 'VMS'; 

my $err_ref = shift;
 local($@);
 my $chown_restricted;
 $chown_restricted = &POSIX::_PC_CHOWN_RESTRICTED()
 if eval { &POSIX::_PC_CHOWN_RESTRICTED(); 1};
 if (defined $chown_restricted) {
 return _is_safe($path,$err_ref) if POSIX::sysconf( $chown_restricted );

}
 unless (File::Spec->file_name_is_absolute($path)) {
 $path = File::Spec->rel2abs($path);
 }
 my ($volume, $directories, undef) = File::Spec->splitpath( $path, 1);
 my @dirs = File::Spec->splitdir($directories);
 foreach my $pos (0.. $#dirs) {
 my $dir = File::Spec->catpath($volume,
 File::Spec->catdir(@dirs[0.. $#dirs - $pos]),
 ''
 );

print "TESTING DIR $dir\n" if $DEBUG;
 return 0 unless _is_safe($dir,$err_ref);

}

return 1;
}
sub _can_unlink_opened_file {

if ($^O eq 'MSWin32' || $^O eq 'os2' || $^O eq 'VMS' || $^O eq 'dos' || $^O eq 'MacOS') {
 return 0;
 } else {
 return 1;
 }
}
sub _can_do_level {
 my $level = shift;
 return 1 if $level == STANDARD;
 if ( $^O eq 'MSWin32' || $^O eq 'os2' || $^O eq 'cygwin' || $^O eq 'dos' || $^O eq 'MacOS' || $^O eq 'mpeix') {
 return 0;
 } else {
 return 1;
 }
}
{
 my (%files_to_unlink, %dirs_to_unlink);
 END {
 local($., $@, $!, $^E, $?);
 cleanup();
 }
 sub cleanup {
 if (!$KEEP_ALL) {
 my @files = (exists $files_to_unlink{$$} ?
 @{ $files_to_unlink{$$} } : () );
 foreach my $file (@files) {
 close($file->[0]); 

if (-f $file->[1]) { 
 _force_writable( $file->[1] ); 
 unlink $file->[1] or warn "Error removing ".$file->[1];
 }
 }
 my @dirs = (exists $dirs_to_unlink{$$} ?
 @{ $dirs_to_unlink{$$} } : () );
 foreach my $dir (@dirs) {
 if (-d $dir) {
 eval { rmtree($dir, $DEBUG, 0); };
 warn $@ if ($@ && $^W);
 }
 }
 @{ $files_to_unlink{$$} } = ()
 if exists $files_to_unlink{$$};
 @{ $dirs_to_unlink{$$} } = ()
 if exists $dirs_to_unlink{$$};
 }
 }
 sub _deferred_unlink {

croak 'Usage:  _deferred_unlink($fh, $fname, $isdir)'
 unless scalar(@_) == 3;

my ($fh, $fname, $isdir) = @_;

warn "Setting up deferred removal of $fname\n"
 if $DEBUG;
 if ($isdir) {

if (-d $fname) {
 $fname = VMS::Filespec::vmspath($fname) if $^O eq 'VMS';
 $dirs_to_unlink{$$} = []
 unless exists $dirs_to_unlink{$$};
 push (@{ $dirs_to_unlink{$$} }, $fname);

} else {
 carp "Request to remove directory $fname could not be completed since it does not exist!\n" if $^W;
 }

} else {

if (-f $fname) {
 $files_to_unlink{$$} = []
 unless exists $files_to_unlink{$$};
 push(@{ $files_to_unlink{$$} }, [$fh, $fname]);

} else {
 carp "Request to remove file $fname could not be completed since it is not there!\n" if $^W;
 }

}

}
}
sub new {
 my $proto = shift;
 my $class = ref($proto) || $proto;
 my %args = @_;
 %args = map { uc($_), $args{$_} } keys %args;
 my $unlink = (exists $args{UNLINK} ? $args{UNLINK} : 1 );
 delete $args{UNLINK};
 my @template = ( exists $args{TEMPLATE} ? $args{TEMPLATE} : () );
 delete $args{TEMPLATE};
 delete $args{OPEN};
 my ($fh, $path) = tempfile( @template, %args );

print "Tmp: $fh - $path\n" if $DEBUG;
 ${*$fh} = $path;
 $FILES_CREATED_BY_OBJECT{$$}{$path} = 1;
 %{*$fh} = %args;
 bless $fh, $class;
 $fh->unlink_on_destroy( $unlink );

return $fh;
}
sub newdir {
 my $self = shift;
 my $template = (scalar(@_) % 2 == 1 ? shift(@_) : undef );
 my %options = @_;
 my $cleanup = (exists $options{CLEANUP} ? $options{CLEANUP} : 1 );

delete $options{CLEANUP};

my $tempdir;
 if (defined $template) {
 $tempdir = tempdir( $template, %options );
 } else {
 $tempdir = tempdir( %options );
 }
 return bless { DIRNAME => $tempdir,
 CLEANUP => $cleanup,
 LAUNCHPID => $$,
 }, "File::Temp::Dir";
}
sub filename {
 my $self = shift;
 return ${*$self};
}
sub STRINGIFY {
 my $self = shift;
 return $self->filename;
}
sub unlink_on_destroy {
 my $self = shift;
 if (@_) {
 ${*$self}{UNLINK} = shift;
 }
 return ${*$self}{UNLINK};
}
sub DESTROY {
 local($., $@, $!, $^E, $?);
 my $self = shift;
 my $file = $self->filename;
 my $was_created_by_proc;
 if (exists $FILES_CREATED_BY_OBJECT{$$}{$file}) {
 $was_created_by_proc = 1;
 delete $FILES_CREATED_BY_OBJECT{$$}{$file};
 }

if (${*$self}{UNLINK} && !$KEEP_ALL) {
 print "# --------->   Unlinking $self\n" if $DEBUG;
 return unless $was_created_by_proc;
 _force_writable( $file ); 
 unlink1( $self, $file )
 or unlink($file);
 }
}
sub tempfile {
 my %options = (
 "DIR" => undef, 
 "SUFFIX" => '', 
 "UNLINK" => 0, 
 "OPEN" => 1, 
 "TMPDIR" => 0, 
 "EXLOCK" => 1, 
 );
 my $template = (scalar(@_) % 2 == 1 ? shift(@_) : undef);
 %options = (%options, @_) if @_;
 if (! $options{"OPEN"}) {

warn "tempfile(): temporary filename requested but not opened.\nPossibly unsafe, consider using tempfile() with OPEN set to true\n"
 if $^W;

}

if ($options{"DIR"} and $^O eq 'VMS') {
 $options{"DIR"} = VMS::Filespec::vmspath($options{"DIR"});
 }
 if (defined $template) {
 if ($options{"DIR"}) {

$template = File::Spec->catfile($options{"DIR"}, $template);

} elsif ($options{TMPDIR}) {

$template = File::Spec->catfile(File::Spec->tmpdir, $template );

}

} else {

if ($options{"DIR"}) {

$template = File::Spec->catfile($options{"DIR"}, TEMPXXX);

} else {

$template = File::Spec->catfile(File::Spec->tmpdir, TEMPXXX);

}

}
 $template .= $options{"SUFFIX"};
 my $unlink_on_close = ( wantarray ? 0 : 1);
 my ($fh, $path, $errstr);
 croak "Error in tempfile() using $template: $errstr"
 unless (($fh, $path) = _gettemp($template,
 "open" => $options{'OPEN'},
 "mkdir"=> 0 ,
 "unlink_on_close" => $unlink_on_close,
 "suffixlen" => length($options{'SUFFIX'}),
 "ErrStr" => \$errstr,
 "use_exlock" => $options{EXLOCK},
 ) );
 _deferred_unlink($fh, $path, 0) if $options{"UNLINK"};
 if (wantarray()) {

if ($options{'OPEN'}) {
 return ($fh, $path);
 } else {
 return (undef, $path);
 }

} else {
 unlink0($fh, $path) or croak "Error unlinking file $path using unlink0";
 return $fh;
 }
}
sub tempdir {
 my %options = (
 "CLEANUP" => 0, 
 "DIR" => '', 
 "TMPDIR" => 0, 
 );
 my $template = (scalar(@_) % 2 == 1 ? shift(@_) : undef );
 %options = (%options, @_) if @_;
 if (defined $template) {
 if ($options{'TMPDIR'} || $options{'DIR'}) {
 $template = VMS::Filespec::vmspath($template) if $^O eq 'VMS';
 my ($volume, $directories, undef) = File::Spec->splitpath( $template, 1);
 $template = (File::Spec->splitdir($directories))[-1];
 if ($options{"DIR"}) {

$template = File::Spec->catdir($options{"DIR"}, $template);

} elsif ($options{TMPDIR}) {
 $template = File::Spec->catdir(File::Spec->tmpdir, $template);

}

}

} else {

if ($options{"DIR"}) {

$template = File::Spec->catdir($options{"DIR"}, TEMPXXX);

} else {

$template = File::Spec->catdir(File::Spec->tmpdir, TEMPXXX);

}

}
 my $tempdir;
 my $suffixlen = 0;
 if ($^O eq 'VMS') { 
 $template =~ m/([\.\]:>]+)$/;
 $suffixlen = length($1);
 }
 if ( ($^O eq 'MacOS') && (substr($template, -1) eq ':') ) {
 ++$suffixlen;
 }

my $errstr;
 croak "Error in tempdir() using $template: $errstr"
 unless ((undef, $tempdir) = _gettemp($template,
 "open" => 0,
 "mkdir"=> 1 ,
 "suffixlen" => $suffixlen,
 "ErrStr" => \$errstr,
 ) );
 if ( $options{'CLEANUP'} && -d $tempdir) {
 _deferred_unlink(undef, $tempdir, 1);
 }
 return $tempdir;
}
sub mkstemp {

croak "Usage: mkstemp(template)"
 if scalar(@_) != 1;

my $template = shift;

my ($fh, $path, $errstr);
 croak "Error in mkstemp using $template: $errstr"
 unless (($fh, $path) = _gettemp($template,
 "open" => 1,
 "mkdir"=> 0 ,
 "suffixlen" => 0,
 "ErrStr" => \$errstr,
 ) );

if (wantarray()) {
 return ($fh, $path);
 } else {
 return $fh;
 }
}
sub mkstemps {

croak "Usage: mkstemps(template, suffix)"
 if scalar(@_) != 2;

my $template = shift;
 my $suffix = shift;

$template .= $suffix;

my ($fh, $path, $errstr);
 croak "Error in mkstemps using $template: $errstr"
 unless (($fh, $path) = _gettemp($template,
 "open" => 1,
 "mkdir"=> 0 ,
 "suffixlen" => length($suffix),
 "ErrStr" => \$errstr,
 ) );

if (wantarray()) {
 return ($fh, $path);
 } else {
 return $fh;
 }
}
sub mkdtemp {

croak "Usage: mkdtemp(template)"
 if scalar(@_) != 1;

my $template = shift;
 my $suffixlen = 0;
 if ($^O eq 'VMS') { 
 $template =~ m/([\.\]:>]+)$/;
 $suffixlen = length($1);
 }
 if ( ($^O eq 'MacOS') && (substr($template, -1) eq ':') ) {
 ++$suffixlen;
 }
 my ($junk, $tmpdir, $errstr);
 croak "Error creating temp directory from template $template\: $errstr"
 unless (($junk, $tmpdir) = _gettemp($template,
 "open" => 0,
 "mkdir"=> 1 ,
 "suffixlen" => $suffixlen,
 "ErrStr" => \$errstr,
 ) );

return $tmpdir;
}
sub mktemp {

croak "Usage: mktemp(template)"
 if scalar(@_) != 1;

my $template = shift;

my ($tmpname, $junk, $errstr);
 croak "Error getting name to temp file from template $template: $errstr"
 unless (($junk, $tmpname) = _gettemp($template,
 "open" => 0,
 "mkdir"=> 0 ,
 "suffixlen" => 0,
 "ErrStr" => \$errstr,
 ) );

return $tmpname;
}
sub tmpnam {
 my $tmpdir = File::Spec->tmpdir;

croak "Error temporary directory is not writable"
 if $tmpdir eq '';
 my $template = File::Spec->catfile($tmpdir, TEMPXXX);

if (wantarray() ) {
 return mkstemp($template);
 } else {
 return mktemp($template);
 }
}
sub tmpfile {
 my ($fh, $file) = tmpnam();
 unlink0($fh, $file)
 or return undef;

return $fh;
}
sub tempnam {

croak 'Usage tempnam($dir, $prefix)' unless scalar(@_) == 2;

my ($dir, $prefix) = @_;
 $prefix .= 'XXXXXXXX';
 my $template = File::Spec->catfile($dir, $prefix);

return mktemp($template);
}
sub unlink0 {

croak 'Usage: unlink0(filehandle, filename)'
 unless scalar(@_) == 2;
 my ($fh, $path) = @_;

cmpstat($fh, $path) or return 0;
 if (_can_unlink_opened_file()) {
 return 1 if $KEEP_ALL;
 croak "unlink0: $path has become a directory!" if -d $path;
 unlink($path) or return 0;
 my @fh = stat $fh;

print "Link count = $fh[3] \n" if $DEBUG;
 return ( $fh[3] == 0 or $^O eq 'cygwin' ? 1 : 0);

} else {
 _deferred_unlink($fh, $path, 0);
 return 1;
 }
}
sub cmpstat {

croak 'Usage: cmpstat(filehandle, filename)'
 unless scalar(@_) == 2;
 my ($fh, $path) = @_;

warn "Comparing stat\n"
 if $DEBUG;
 my @fh;
 {
 local ($^W) = 0;
 @fh = stat $fh;
 }
 return unless @fh;

if ($fh[3] > 1 && $^W) {
 carp "unlink0: fstat found too many links; SB=@fh" if $^W;
 }
 my @path = stat $path;

unless (@path) {
 carp "unlink0: $path is gone already" if $^W;
 return;
 }
 unless (-f $path) {
 confess "panic: $path is no longer a file: SB=@fh";
 }
 my @okstat = (0..$#fh); 
 if ($^O eq 'MSWin32') {
 @okstat = (1,2,3,4,5,7,8,9,10);
 } elsif ($^O eq 'os2') {
 @okstat = (0, 2..$#fh);
 } elsif ($^O eq 'VMS') { 
 @okstat = (0, 1);
 } elsif ($^O eq 'dos') {
 @okstat = (0,2..7,11..$#fh);
 } elsif ($^O eq 'mpeix') {
 @okstat = (0..4,8..10);
 }
 for (@okstat) {
 print "Comparing: $_ : $fh[$_] and $path[$_]\n" if $DEBUG;
 unless ($fh[$_] eq $path[$_]) {
 warn "Did not match $_ element of stat\n" if $DEBUG;
 return 0;
 }
 }

return 1;
}
sub unlink1 {
 croak 'Usage: unlink1(filehandle, filename)'
 unless scalar(@_) == 2;
 my ($fh, $path) = @_;

cmpstat($fh, $path) or return 0;
 close( $fh ) or return 0;
 _force_writable( $path );
 return 1 if $KEEP_ALL;
 return unlink($path);
}
{
 my $LEVEL = STANDARD;
 sub safe_level {
 my $self = shift;
 if (@_) {
 my $level = shift;
 if (($level != STANDARD) && ($level != MEDIUM) && ($level != HIGH)) {
 carp "safe_level: Specified level ($level) not STANDARD, MEDIUM or HIGH - ignoring\n" if $^W;
 } else {
 if ($] < 5.006 && $level != STANDARD) {
 croak "Currently requires perl 5.006 or newer to do the safe checks";
 }
 $LEVEL = $level if _can_do_level($level);
 }
 }
 return $LEVEL;
 }
}
{
 my $TopSystemUID = 10;
 $TopSystemUID = 197108 if $^O eq 'interix'; 
 sub top_system_uid {
 my $self = shift;
 if (@_) {
 my $newuid = shift;
 croak "top_system_uid: UIDs should be numeric"
 unless $newuid =~ /^\d+$/s;
 $TopSystemUID = $newuid;
 }
 return $TopSystemUID;
 }
}
package File::Temp::Dir;
use File::Path qw/rmtree/;
use strict;
use overload '""' => "STRINGIFY", fallback => 1;
sub dirname {
 my $self = shift;
 return $self->{DIRNAME};
}
sub STRINGIFY {
 my $self = shift;
 return $self->dirname;
}
sub unlink_on_destroy {
 my $self = shift;
 if (@_) {
 $self->{CLEANUP} = shift;
 }
 return $self->{CLEANUP};
}
sub DESTROY {
 my $self = shift;
 local($., $@, $!, $^E, $?);
 if ($self->unlink_on_destroy &&
 $$ == $self->{LAUNCHPID} && !$File::Temp::KEEP_ALL) {
 if (-d $self->{DIRNAME}) {
 eval { rmtree($self->{DIRNAME}, $File::Temp::DEBUG, 0); };
 warn $@ if ($@ && $^W);
 }
 }
}
1;
}
BEGIN { $INC{q{HTML/Entities.pm}} = 1;
package HTML::Entities;
use strict;
use vars qw(@ISA @EXPORT @EXPORT_OK $VERSION);
use vars qw(%entity2char %char2entity);
require 5.004;
require Exporter;
@ISA = qw(Exporter);
@EXPORT = qw(encode_entities decode_entities);
@EXPORT_OK = qw(%entity2char %char2entity);
$VERSION = sprintf("%d.%02d", q$Revision: 1.13 $ =~ /(\d+)\.(\d+)/);
sub Version { $VERSION; }
%entity2char = (
 amp => '&', 
'gt' => '>', 
'lt' => '<', 
 quot => '"', 
 AElig => 'Æ', 
 Aacute => 'Á', 
 Acirc => 'Â', 
 Agrave => 'À', 
 Aring => 'Å', 
 Atilde => 'Ã', 
 Auml => 'Ä', 
 Ccedil => 'Ç', 
 ETH => 'Ð', 
 Eacute => 'É', 
 Ecirc => 'Ê', 
 Egrave => 'È', 
 Euml => 'Ë', 
 Iacute => 'Í', 
 Icirc => 'Î', 
 Igrave => 'Ì', 
 Iuml => 'Ï', 
 Ntilde => 'Ñ', 
 Oacute => 'Ó', 
 Ocirc => 'Ô', 
 Ograve => 'Ò', 
 Oslash => 'Ø', 
 Otilde => 'Õ', 
 Ouml => 'Ö', 
 THORN => 'Þ', 
 Uacute => 'Ú', 
 Ucirc => 'Û', 
 Ugrave => 'Ù', 
 Uuml => 'Ü', 
 Yacute => 'Ý', 
 aacute => 'á', 
 acirc => 'â', 
 aelig => 'æ', 
 agrave => 'à', 
 aring => 'å', 
 atilde => 'ã', 
 auml => 'ä', 
 ccedil => 'ç', 
 eacute => 'é', 
 ecirc => 'ê', 
 egrave => 'è', 
 eth => 'ð', 
 euml => 'ë', 
 iacute => 'í', 
 icirc => 'î', 
 igrave => 'ì', 
 iuml => 'ï', 
 ntilde => 'ñ', 
 oacute => 'ó', 
 ocirc => 'ô', 
 ograve => 'ò', 
 oslash => 'ø', 
 otilde => 'õ', 
 ouml => 'ö', 
 szlig => 'ß', 
 thorn => 'þ', 
 uacute => 'ú', 
 ucirc => 'û', 
 ugrave => 'ù', 
 uuml => 'ü', 
 yacute => 'ý', 
 yuml => 'ÿ', 
 copy => '©', 
 reg => '®', 
 nbsp => "\240", 
 iexcl => '¡',
 cent => '¢',
 pound => '£',
 curren => '¤',
 yen => '¥',
 brvbar => '¦',
 sect => '§',
 uml => '¨',
 ordf => 'ª',
 laquo => '«',
'not' => '¬', 
 shy => '­',
 macr => '¯',
 deg => '°',
 plusmn => '±',
 sup1 => '¹',
 sup2 => '²',
 sup3 => '³',
 acute => '´',
 micro => 'µ',
 para => '¶',
 middot => '·',
 cedil => '¸',
 ordm => 'º',
 raquo => '»',
 frac14 => '¼',
 frac12 => '½',
 frac34 => '¾',
 iquest => '¿',
'times' => '×', 
 divide => '÷',
);
while (my($entity, $char) = each(%entity2char)) {
 $char2entity{$char} = "&$entity;";
}
for (0 .. 255) {
 next if exists $char2entity{chr($_)};
 $char2entity{chr($_)} = "&#$_;";
}
my %subst; 
sub decode_entities
{
 my $array;
 if (defined wantarray) {
 $array = [@_]; 
 } else {
 $array = \@_; 
 }
 my $c;
 for (@$array) {
 s/(&\#(\d+);?)/$2 < 256 ? chr($2) : $1/eg;
 s/(&\#[xX]([0-9a-fA-F]+);?)/$c = hex($2); $c < 256 ? chr($c) : $1/eg;
 s/(&(\w+);?)/$entity2char{$2} || $1/eg;
 }
 wantarray ? @$array : $array->[0];
}
sub encode_entities
{
 my $ref;
 if (defined wantarray) {
 my $x = $_[0];
 $ref = \$x; 
 } else {
 $ref = \$_[0]; 
 }
 if (defined $_[1]) {
 unless (exists $subst{$_[1]}) {
 $subst{$_[1]} =
 eval "sub {\$_[0] =~ s/([$_[1]])/\$char2entity{\$1}/g; }";
 die $@ if $@;
 }
 &{$subst{$_[1]}}($$ref);
 } else {
 $$ref =~ s/([^\n\t !\#\$%\'-;=?-~])/$char2entity{$1}/g;
 }
 $$ref;
}
*encode = \&encode_entities;
*decode = \&decode_entities;
1;
}
BEGIN { $INC{q{HTML/Mason/MethodMaker.pm}} = 1;
package HTML::Mason::MethodMaker;
use strict;
use warnings;
use Params::Validate qw(validate_pos);
sub import
{
 my $caller = caller;
 shift; 
 my %p = @_;

if ($p{read_only})
 {
 foreach my $ro ( ref $p{read_only} ? @{ $p{read_only} } : $p{read_only} )
 {
 no strict 'refs';
 *{"$caller\::$ro"} = sub { return $_[0]->{$ro} };
 }
 }
 if ($p{read_write})
 {
 foreach my $rw ( ref $p{read_write} ? @{ $p{read_write} } : $p{read_write} )
 {
 if (ref $rw)
 {
 my ($name, $spec) = @$rw;
 my $sub =
 sub { if (@_ > 1)
 {
 my $s = shift;
 validate_pos(@_, $spec);
 $s->{$name} = shift;
 return $s->{$name};
 }
 return $_[0]->{$name};
 };
 no strict 'refs';
 *{"$caller\::$name"} = $sub
 }
 else
 {
 my $sub =
 sub { if (@_ > 1)
 {
 $_[0]->{$rw} = $_[1];
 }
 return $_[0]->{$rw};
 };
 no strict 'refs';
 *{"$caller\::$rw"} = $sub;
 }
 }
 }

if ($p{read_write_contained})
 {
 foreach my $object (keys %{ $p{read_write_contained} })
 {
 foreach my $rwc (@{ $p{read_write_contained}{$object} })
 {
 if (ref $rwc)
 {
 my ($name, $spec) = @$rwc;
 my $sub =
 sub { my $s = shift;
 my %new;
 if (@_)
 {
 validate_pos(@_, $spec);
 %new = ( $name => $_[0] );
 }
 my %args = $s->delayed_object_params( $object,
 %new );
 return $args{$rwc};
 };
 no strict 'refs';
 *{"$caller\::$name"} = $sub;
 }
 else
 {
 my $sub =
 sub { my $s = shift;
 my %new = @_ ? ( $rwc => $_[0] ) : ();
 my %args = $s->delayed_object_params( $object,
 %new );
 return $args{$rwc};
 };
 no strict 'refs';
 *{"$caller\::$rwc"} = $sub;
 }
 }
 }
 }
}
1;
}
BEGIN { $INC{q{HTML/Mason/Exceptions.pm}} = 1;
package HTML::Mason::Exceptions;
use strict;
use warnings;
use vars qw($VERSION);
$VERSION = 1.43;
my %e;
BEGIN
{
 %e = ( 'HTML::Mason::Exception' =>
 { description => 'generic base class for all Mason exceptions',
 alias => 'error'},

'HTML::Mason::Exception::Abort' =>
 { isa => 'HTML::Mason::Exception',
 fields => [qw(aborted_value)],
 description => 'a component called $m->abort' },

'HTML::Mason::Exception::Decline' =>
 { isa => 'HTML::Mason::Exception',
 fields => [qw(declined_value)],
 description => 'a component called $m->decline' },

'HTML::Mason::Exception::Compiler' =>
 { isa => 'HTML::Mason::Exception',
 alias => 'compiler_error',
 description => 'error thrown from the compiler' },

'HTML::Mason::Exception::Compilation' =>
 { isa => 'HTML::Mason::Exception',
 alias => 'compilation_error',
 fields => [qw(filename)],
 description => "error thrown in eval of the code for a component" },

'HTML::Mason::Exception::Compilation::IncompatibleCompiler' =>
 { isa => 'HTML::Mason::Exception::Compilation',
 alias => 'wrong_compiler_error',
 description => "a component was compiled by a compiler/lexer with incompatible options.  recompilation is needed" },

'HTML::Mason::Exception::Params' =>
 { isa => 'HTML::Mason::Exception',
 alias => 'param_error',
 description => 'invalid parameters were given to a method/function' },

'HTML::Mason::Exception::Syntax' =>
 { isa => 'HTML::Mason::Exception',
 alias => 'syntax_error',
 fields => [qw(source_line comp_name line_number)],
 description => 'invalid syntax was found in a component' },

'HTML::Mason::Exception::System' =>
 { isa => 'HTML::Mason::Exception',
 alias => 'system_error',
 description => 'a system call of some sort failed' },

'HTML::Mason::Exception::TopLevelNotFound' =>
 { isa => 'HTML::Mason::Exception',
 alias => 'top_level_not_found_error',
 description => 'the top level component could not be found' },

'HTML::Mason::Exception::VirtualMethod' =>
 { isa => 'HTML::Mason::Exception',
 alias => 'virtual_error',
 description => 'a virtual method was not overridden' },

);
}
use Exception::Class (%e);
HTML::Mason::Exception->Trace(1);
HTML::Mason::Exception->NoRefs(1);
sub import
{
 my ($class, %args) = @_;

my $caller = caller;
 if ($args{abbr})
 {
 foreach my $name (@{$args{abbr}})
 {
 no strict 'refs';
 die "Unknown exception abbreviation '$name'" unless defined &{$name};
 *{"${caller}::$name"} = \&{$name};
 }
 }
 {
 no strict 'refs';
 *{"${caller}::isa_mason_exception"} = \&isa_mason_exception;
 *{"${caller}::rethrow_exception"} = \&rethrow_exception;
 }
}
sub isa_mason_exception
{
 my ($err, $name) = @_;
 return unless defined $err;

$name = $name ? "HTML::Mason::Exception::$name" : "HTML::Mason::Exception";
 no strict 'refs';
 die "no such exception class $name" unless $name->isa('HTML::Mason::Exception');

return UNIVERSAL::isa($err, $name);
}
sub rethrow_exception
{
 my ($err) = @_;
 return unless $err;

if ( UNIVERSAL::can($err, 'rethrow') ) {
 $err->rethrow;
 }
 elsif ( ref $err ) {
 die $err;
 }
 HTML::Mason::Exception->throw(error => $err);
}
package HTML::Mason::Exception;
use HTML::Mason::MethodMaker
 ( read_write => [ qw(format) ] );
sub new
{
 my ($class, %params) = @_;

my $self = $class->SUPER::new(%params);
 $self->format('text');
 return $self;
}
sub throw
{
 my $class = shift;
 my %params = @_ == 1 ? ( error => $_[0] ) : @_;

if (HTML::Mason::Exceptions::isa_mason_exception($params{error})) {
 $params{error} = $params{error}->error;
 }
 if (HTML::Mason::Exceptions::isa_mason_exception($params{message})) {
 $params{message} = $params{message}->error;
 }
 $class->SUPER::throw(%params);
}
sub filtered_frames
{
 my ($self) = @_;

my (@frames);
 my $trace = $self->trace;
 my %ignore_subs = map { $_ => 1 }
 qw[(eval) Exception::Class::Base::throw Exception::Class::__ANON__ HTML::Mason::Commands::__ANON__ HTML::Mason::Component::run HTML::Mason::Exception::throw HTML::Mason::Exceptions::__ANON__ HTML::Mason::Request::_run_comp];
 while (my $frame = $trace->next_frame)
 {
 last if ($frame->subroutine eq 'HTML::Mason::Request::exec');
 unless ($frame->filename =~ /Mason\/Exceptions\.pm/ or
 $ignore_subs{ $frame->subroutine } or
 ($frame->subroutine eq 'HTML::Mason::Request::comp' and $frame->filename =~ /Request\.pm/)) {
 push(@frames, $frame);
 }
 }
 @frames = grep { $_->filename !~ /Mason\/Exceptions\.pm/ } $trace->frames if !@frames;
 return @frames;
}
sub analyze_error
{
 my ($self) = @_;
 my ($file, @lines, @frames);

return $self->{_info} if $self->{_info};

@frames = $self->filtered_frames;
 if ($self->isa('HTML::Mason::Exception::Syntax')) {
 $file = $self->comp_name;
 push(@lines, $self->line_number);
 } elsif ($self->isa('HTML::Mason::Exception::Compilation')) {
 $file = $self->filename;
 my $msg = $self->full_message;
 while ($msg =~ /at .* line (\d+)./g) {
 push(@lines, $1);
 }
 } elsif (@frames) {
 $file = $frames[0]->filename;
 @lines = $frames[0]->line;
 }
 my @context;
 @context = $self->get_file_context($file, \@lines) if @lines;

$self->{_info} = {
 file => $file,
 frames => \@frames,
 lines => \@lines,
 context => \@context,
 };
 return $self->{_info};
}
sub get_file_context
{
 my ($self, $file, $line_nums) = @_;

my @context;
 my $fh = do { local *FH; *FH; };
 unless (defined($file) and open($fh, $file)) {
 @context = (['unable to open file', '']);
 } else {
 my @file = <$fh>;
 chomp(@file);
 unshift(@file, undef);
 my (%marks, %red);
 my $delta = 4;
 foreach my $line_num (@$line_nums) {
 foreach my $l (($line_num - $delta) .. ($line_num + $delta)) {
 next if ($l <= 0 or $l > @file);
 $marks{$l}++;
 }
 $red{$line_num} = 1;
 }
 my $last_num = 0;
 foreach my $line_num (sort { $a <=> $b } keys %marks) {
 push(@context, ["...", "", 0]) unless $last_num == ($line_num - 1);
 push(@context, ["$line_num:", $file[$line_num], $red{$line_num}]);;
 $last_num = $line_num;
 }
 push(@context, ["...", "", 0]) unless $last_num == @file;
 close $fh;
 }
 return @context;
}
sub raw_text
{
 my ($self) = @_;

return $self->full_message . "\n\n" . $self->trace->as_string;
}
sub as_string
{
 my ($self) = @_;

my $stringify_function = "as_" . $self->{format};
 return $self->$stringify_function();
}
sub as_brief
{
 my ($self) = @_;
 return $self->full_message;
}
sub as_line
{
 my ($self) = @_;
 my $info = $self->analyze_error;

(my $msg = $self->full_message) =~ s/\n/\t/g;
 my $stack = join(", ", map { sprintf("[%s:%d]", $_->filename, $_->line) } @{$info->{frames}});
 return sprintf("%s\tStack: %s\n", $msg, $stack);
}
sub as_text
{
 my ($self) = @_;
 my $info = $self->analyze_error;

my $msg = $self->full_message;
 my $stack = join("\n", map { sprintf("  [%s:%d]", $_->filename, $_->line) } @{$info->{frames}});
 return sprintf("%s\nStack:\n%s\n", $msg, $stack);
}
sub as_html
{
 my ($self) = @_;

my $out;
 my $interp = HTML::Mason::Interp->new(out_method => \$out);

my $comp = $interp->make_component(comp_source => <<'EOF');

<%args>
 $msg
 $info
 $error
</%args>
<%filter>
 s/(<td [^\>]+>)/$1<font face="Verdana, Arial, Helvetica, sans-serif" size="-2">/g;
 s/<\/td>/<\/font><\/td>/g;
</%filter>

% HTML::Mason::Escapes::basic_html_escape(\$msg);
% $msg =~ s/\n/<br>/g;

<html><body>

<p align="center"><font face="Verdana, Arial, Helvetica, sans-serif"><b>System error</b></font></p>
<table border="0" cellspacing="0" cellpadding="1">
 <tr>
  <td nowrap="nowrap" align="left" valign="top"><b>error:</b>&nbsp;</td>
  <td align="left" valign="top"><% $msg %></td>
 </tr>
 <tr>
  <td nowrap="nowrap" align="left" valign="top"><b>context:</b>&nbsp;</td>
  <td align="left" valign="top" nowrap="nowrap">
   <table border="0" cellpadding="0" cellspacing="0">

%   foreach my $entry (@{$info->{context}}) {
%       my ($line_num, $line, $highlight) = @$entry;
%       $line = '' unless defined $line;
%       HTML::Mason::Escapes::basic_html_escape(\$line);
    <tr>
     <td nowrap="nowrap" align="left" valign="top"><b><% $line_num %></b>&nbsp;</td>
     <td align="left" valign="top" nowrap="nowrap"><% $highlight ? "<font color=red>" : "" %><% $line %><% $highlight ? "</font>" : "" %></td>
    </tr>

%    }

   </table>
  </td>
 </tr>
 <tr>
  <td align="left" valign="top" nowrap="nowrap"><b>code stack:</b>&nbsp;</td>
  <td align="left" valign="top" nowrap="nowrap">
%    foreach my $frame (@{$info->{frames}}) {
%        my $f = $frame->filename; HTML::Mason::Escapes::basic_html_escape(\$f);
%        my $l = $frame->line; HTML::Mason::Escapes::basic_html_escape(\$l);
        <% $f %>:<% $l %><br>
%    }
  </td>
 </tr>
</table>

<a href="#raw">raw error</a><br>

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

% my $raw = $error->raw_text;
% HTML::Mason::Escapes::basic_html_escape(\$raw);
% $raw =~ s/\t//g;

<a name="raw"></a>

<pre><% $raw %></pre>

</body></html>
EOF

$interp->exec($comp,
 msg => $self->full_message,
 info => $self->analyze_error,
 error => $self);

return $out;
}
package HTML::Mason::Exception::Compilation;
sub full_message
{
 my $self = shift;

return sprintf("Error during compilation of %s:\n%s\n", $self->filename || '', $self->message || '');
}
package HTML::Mason::Exception::Syntax;
sub full_message
{
 my $self = shift;

return sprintf("%s at %s line %d", $self->message || '', $self->comp_name || '', $self->line_number);
}
1;
}
BEGIN { $INC{q{HTML/Mason/Tools.pm}} = 1;
package HTML::Mason::Tools;
use strict;
use warnings;
use Cwd;
use File::Spec;
use HTML::Mason::Exceptions( abbr => [qw(system_error param_error error)] );
require Exporter;
use vars qw(@ISA @EXPORT_OK);
@ISA = qw(Exporter);
@EXPORT_OK = qw(can_weaken read_file read_file_ref url_escape paths_eq compress_path mason_canonpath taint_is_on load_pkg pkg_loaded absolute_comp_path checksum);
BEGIN
{
 require Scalar::Util;

my $can_weaken = defined &Scalar::Util::weaken ? 1 : 0;

sub can_weaken () { $can_weaken }
}
sub read_file
{
 my $fh = _get_reading_handle(@_);
 return do {local $/; scalar <$fh>};
}
sub read_file_ref
{
 my $fh = _get_reading_handle(@_);
 my ($buffer, $retval) = ('');
 while (1) {
 $retval = read $fh, $buffer, 1024 * 16, length($buffer);
 system_error "read_file_ref: Couldn't read from '$_[0]': $!"
 unless defined $retval;
 last if !$retval;
 }
 return \$buffer;
}
sub _get_reading_handle {
 my ($file,$binmode) = @_;
 error "read_file: '$file' does not exist" unless -e $file;
 error "read_file: '$file' is a directory" if (-d _);
 open my $fh, "< $file"
 or system_error "read_file: could not open file '$file' for reading: $!";
 binmode $fh if $binmode;
 return $fh;
}
sub paths_eq {
 return File::Spec->case_tolerant ? (lc($_[0]) eq lc($_[1])) : $_[0] eq $_[1];
}
sub compress_path
{
 my ($path) = @_;
 for ($path) {
 s@^/@@;
 s/([^\w\.\-\~])/sprintf('+%02x', ord $1)/eg;
 }
 return $path;
}
sub absolute_comp_path
{
 my ($comp_path, $dir_path) = @_;

$comp_path = "$dir_path/$comp_path" if $comp_path !~ m@^/@;
 return mason_canonpath($comp_path);
}
sub mason_canonpath {
 my $path = shift;
 $path =~ s|/+|/|g; 
 $path =~ s|(?:/\.)+/|/|g; 
 {
 $path =~ s|^(?:\./)+||s unless $path eq "./"; 
 $path =~ s|^/(?:\.\./)+|/|s; 
 $path =~ s|/\Z(?!\n)|| unless $path eq "/"; 
 $path =~ s|/[^/]+/\.\.$|| && redo; 
 $path =~ s|[^/]+/\.\./|| && redo; 
 }
 return $path;
}
sub pkg_installed
{
 my ($pkg) = @_;

(my $pkgfile = "$pkg.pm") =~ s{::}{/}g;
 return grep(-f "$_/$pkgfile",@INC);
}
sub pkg_loaded
{
 my ($pkg) = @_;

my $varname = "${pkg}::VERSION";
 no strict 'refs';
 return $$varname ? 1 : 0;
}
sub load_pkg {
 my ($pkg, $nf_error) = @_;

my $file = File::Spec->catfile( split /::/, $pkg );
 $file .= '.pm';
 return 1 if exists $INC{$file};

eval "use $pkg";

if ($@) {
 if ($@ =~ /^Can\'t locate (.*) in \@INC/) {
 if (defined($nf_error)) {
 error sprintf("Can't locate %s in \@INC. %s\n(\@INC contains: %s)",
 $1, $nf_error, join(" ", @INC));
 } else {
 undef $@;
 return 0;
 }
 } else {
 error $@;
 }
 }
 return 1;
}
my $TaintIsOn;
sub taint_is_on
{
 return $TaintIsOn if defined $TaintIsOn;
 return $TaintIsOn = _taint_is_on();
}
sub _taint_is_on
{
 if ( $] >= 5.008 )
 {
 return eval '${^TAINT}' ? 1 : 0;
 }
 else
 {
 local $^W;
 eval { "+$0$^X" && eval 1 };
 return $@ ? 1 : 0;
 }
}
sub coerce_to_array
{
 my ($val, $name) = @_;

return ($val) unless ref $val;

if ( UNIVERSAL::isa( $val, 'ARRAY' ) )
 {
 return @$val;
 }
 elsif ( UNIVERSAL::isa( $val, 'HASH' ) )
 {
 return %$val;
 }

param_error "Cannot coerce $val to an array for '$name' parameter";
}
sub coerce_to_hash
{
 my ($val, $name) = @_;

param_error "Cannot convert a single value to a hash for '$name' parameter"
 unless ref $val;

if ( UNIVERSAL::isa( $val, 'ARRAY' ) )
 {
 return @$val;
 }
 elsif ( UNIVERSAL::isa( $val, 'HASH' ) )
 {
 return %$val;
 }

param_error "Cannot coerce $val to a hash";
}
sub checksum {
 my ($str) = @_;

my $s1 = 1;
 my $s2 = 1;
 for my $c (unpack("C*", $str)) {
 $s1 = ($s1 + $c ) % 65521;
 $s2 = ($s2 + $s1) % 65521;
 }
 return ($s2 << 16) + $s1;
}
1;
}
BEGIN { $INC{q{HTML/Mason/Utils.pm}} = 1;
package HTML::Mason::Utils;
use HTML::Mason::Tools qw(compress_path);
use strict;
use warnings;
require Exporter;
use vars qw(@ISA @EXPORT_OK);
@ISA = qw(Exporter);
@EXPORT_OK = qw(data_cache_namespace cgi_request_args);
sub data_cache_namespace
{
 my ($comp_id) = @_;
 return compress_path($comp_id);
}
sub cgi_request_args
{
 my ($q, $method) = @_;

my %args;
 my @methods =
 $method ne 'POST' || ! $ENV{QUERY_STRING} ? ( 'param' ) : ( 'param', 'url_param' );

foreach my $key ( map { $q->$_() } @methods ) {
 next if exists $args{$key};
 my @values = map { $q->$_($key) } @methods;
 $args{$key} = @values == 1 ? $values[0] : \@values;
 }

return wantarray ? %args : \%args;
}
1;
}
BEGIN { $INC{q{HTML/Mason/Escapes.pm}} = 1;
package HTML::Mason::Escapes;
use strict;
use warnings;
use HTML::Entities ();
my %html_escape = ('&' => '&amp;', '>'=>'&gt;', '<'=>'&lt;', '"'=>'&quot;');
my $html_escape = qr/([&<>"])/;
sub basic_html_escape
{
 return unless defined ${ $_[0] };

${ $_[0] } =~ s/$html_escape/$html_escape{$1}/mg;
}
sub html_entities_escape
{
 return unless defined ${ $_[0] };

HTML::Entities::encode_entities( ${ $_[0] } );
}
sub url_escape
{
 return unless defined ${ $_[0] };

use bytes;
 ${ $_[0] } =~ s/([^a-zA-Z0-9_.-])/uc sprintf("%%%02x",ord($1))/eg;
}
1;
}
BEGIN { $INC{q{HTML/Mason/Cache/BaseCache.pm}} = 1;
package HTML::Mason::Cache::BaseCache;
use strict;
use warnings;
sub get
{
 my ($self, $key, %params) = @_;
 die "must specify key" unless defined($key);

foreach my $param (keys(%params)) {
 unless ($param =~ /^(busy_lock|expire_if)$/) {
 die "unknown param '$param'";
 }
 }

$self->_conditionally_auto_purge_on_get;

if (my $sub = $params{expire_if}) {
 $self->expire_if($key, $sub);
 }

my $object = $self->get_object($key) or
 return undef;

if (Cache::BaseCache::Object_Has_Expired($object))
 {
 if ($params{busy_lock}) {
 my $busy_lock_time = Cache::BaseCache::Canonicalize_Expiration_Time($params{busy_lock});
 $object->set_expires_at(time + $busy_lock_time);
 $self->set_object($key, $object);
 } else {
 $self->remove($key);
 }
 return undef;
 }

return $object->get_data( );
}
sub expire
{
 my ($self, $key) = @_;

if (my $obj = $self->get_object($key)) {
 $obj->set_expires_at(time-1);
 $self->set_object($key, $obj);
 }
}
sub expire_if
{
 my ($self, $key, $sub) = @_;
 die "must specify subroutine" unless defined($sub) and ref($sub) eq 'CODE';

if (my $obj = $self->get_object($key)) {
 my $retval = $sub->($obj);
 if ($retval) {
 $self->expire($key);
 }
 return $retval;
 } else {
 return 1;
 }
}
1;
}
BEGIN { $INC{q{HTML/Mason/Plugin/Context.pm}} = 1;package HTML::Mason::Plugin::Context::StartRequest;
use base qw(HTML::Mason::Plugin::Context);
sub request { $_[0]->[0] }
sub args {
 if (wantarray) {
 return @{$_[0]->[1]};
 } else {
 return $_[0]->[1];
 }
}
package HTML::Mason::Plugin::Context::EndRequest;
use base qw(HTML::Mason::Plugin::Context);
sub request { $_[0]->[0] }
sub args {
 if (wantarray) {
 return @{$_[0]->[1]};
 } else {
 return $_[0]->[1];
 }
}
sub output { $_[0]->[2] }
sub wantarray { $_[0]->[3] }
sub result { $_[0]->[4] }
sub error { $_[0]->[5] }
package HTML::Mason::Plugin::Context::StartComponent;
use base qw(HTML::Mason::Plugin::Context);
sub request { $_[0]->[0] }
sub comp { $_[0]->[1] }
sub args { $_[0]->[2] }
package HTML::Mason::Plugin::Context::EndComponent;
use base qw(HTML::Mason::Plugin::Context);
sub request { $_[0]->[0] }
sub comp { $_[0]->[1] }
sub args { $_[0]->[2] }
sub wantarray { $_[0]->[3] }
sub result { $_[0]->[4] }
sub error { $_[0]->[5] }
1;
}
BEGIN { $INC{q{HTML/Mason/Request.pm}} = 1;
package HTML::Mason::Request;
use strict;
use warnings;
use File::Spec;
use HTML::Mason::Cache::BaseCache;
use HTML::Mason::Plugin::Context;
use HTML::Mason::Tools qw(can_weaken read_file compress_path load_pkg pkg_loaded absolute_comp_path);
use HTML::Mason::Utils;
use Class::Container;
use base qw(Class::Container);
use constant STACK_COMP => 0;
use constant STACK_ARGS => 1;
use constant STACK_BUFFER => 2;
use constant STACK_MODS => 3;
use constant STACK_PATH => 4;
use constant STACK_BASE_COMP => 5;
use constant STACK_IN_CALL_SELF => 6;
use constant STACK_BUFFER_IS_FLUSHABLE => 7;
use constant STACK_HIDDEN_BUFFER => 8;
use HTML::Mason::Exceptions( abbr => [qw(error param_error syntax_error top_level_not_found_error error)] );
use Params::Validate qw(:all);
Params::Validate::validation_options( on_fail => sub { param_error( join '', @_ ) } );
BEGIN
{
 __PACKAGE__->valid_params
 (
 args =>
 { type => ARRAYREF, default => [],
 descr => "Array of arguments to initial component",
 public => 0 },

autoflush =>
 { parse => 'boolean', default => 0, type => SCALAR,
 descr => "Whether output should be buffered or sent immediately" },

comp =>
 { type => SCALAR | OBJECT, optional => 0,
 descr => "Initial component, either an absolute path or a component object",
 public => 0 },

data_cache_api =>
 { parse => 'string', default => '1.1', type => SCALAR,
 regex => qr/^(?:1\.0|1\.1|chi)$/,
 descr => "Data cache API to use: 1.0, 1.1, or chi" },

data_cache_defaults =>
 { parse => 'hash_list', type => HASHREF|UNDEF, optional => 1,
 descr => "A hash of default parameters for Cache::Cache or CHI" },

declined_comps =>
 { type => HASHREF, optional => 1,
 descr => "Hash of components that have been declined in previous parent requests",
 public => 0 },

dhandler_name =>
 { parse => 'string', default => 'dhandler', type => SCALAR,
 descr => "The filename to use for Mason's 'dhandler' capability" },

interp =>
 { isa => 'HTML::Mason::Interp',
 descr => "An interpreter for Mason control functions",
 public => 0 },

error_format =>
 { parse => 'string', type => SCALAR, default => 'text',
 callbacks => { "HTML::Mason::Exception->can( method )'" =>
 sub { HTML::Mason::Exception->can("as_$_[0]"); } },
 descr => "How error conditions are returned to the caller (brief, text, line or html)" },

error_mode =>
 { parse => 'string', type => SCALAR, default => 'fatal',
 regex => qr/^(?:output|fatal)$/,
 descr => "How error conditions are manifest (output or fatal)" },

component_error_handler =>
 { parse => 'code', type => CODEREF|SCALAR, default => \&rethrow_exception,
 descr => "A subroutine reference called on component compilation or runtime errors" },

max_recurse =>
 { parse => 'string', default => 32, type => SCALAR,
 descr => "The maximum recursion depth for component, inheritance, and request stack" },

out_method =>
 { parse => 'code' ,type => CODEREF|SCALARREF,
 default => sub { print STDOUT $_[0] },
 descr => "A subroutine or scalar reference through which all output will pass" },
 parent_request =>
 { isa => __PACKAGE__,
 default => undef,
 public => 0,
 },

plugins =>
 { parse => 'list', default => [], type => ARRAYREF,
 descr => 'List of plugin classes or objects to run hooks around components and requests' },
 request_depth =>
 { type => SCALAR,
 default => 1,
 public => 0,
 },

);
}
my @read_write_params;
BEGIN { @read_write_params = qw(autoflush component_error_handler data_cache_api data_cache_defaults dhandler_name error_format error_mode max_recurse out_method); }
use HTML::Mason::MethodMaker
 ( read_only => [ qw(count dhandler_arg initialized interp parent_request plugin_instances request_depth request_comp) ],

read_write => [ map { [ $_ => __PACKAGE__->validation_spec->{$_} ] }
 @read_write_params ]
 );
sub _properties { @read_write_params }
sub new
{
 my $class = shift;
 my $self = $class->SUPER::new(@_);
 %$self = (%$self,
 dhandler_arg => undef,
 execd => 0,
 initialized => 0,
 stack => [],
 top_stack => undef,
 wrapper_chain => undef,
 wrapper_index => undef,
 notes => {},
 );

$self->{request_comp} = delete($self->{comp});
 $self->{request_args} = delete($self->{args});
 if (UNIVERSAL::isa($self->{request_args}, 'HASH')) {
 $self->{request_args} = [%{$self->{request_args}}];
 }
 $self->{count} = ++$self->{interp}{request_count};
 if (ref($self->{out_method}) eq 'SCALAR') {
 my $bufref = $self->{out_method};
 $self->{out_method} = sub { $$bufref .= $_[0] };
 }
 $self->{use_internal_component_caches} =
 $self->{interp}->use_internal_component_caches;
 $self->_initialize;

return $self;
}
sub instance {
 return $HTML::Mason::Commands::m; 
}
my %plugin_loaded;
sub _initialize {
 my ($self) = @_;

local $SIG{'__DIE__'} = $self->component_error_handler
 if $self->component_error_handler;

eval {
 $self->interp->check_static_source_touch_file;
 my $request_comp = $self->{request_comp};
 my ($path);
 if (!ref($request_comp)) {
 $request_comp =~ s{/+}{/}g;
 $self->{top_path} = $path = $request_comp;

my $retry_count = 0;
 search: {
 $request_comp = $self->interp->load($path);

last search unless $self->use_dhandlers;
 unless ($request_comp) {
 if ( $request_comp = $self->interp->find_comp_upwards($path, $self->dhandler_name) ) {
 my $parent_path = $request_comp->dir_path;
 ($self->{dhandler_arg} = $self->{top_path}) =~ s{^$parent_path/?}{};
 }
 }
 if ($request_comp and $self->{declined_comps}->{$request_comp->comp_id}) {
 $path = $request_comp->dir_path;
 if ($request_comp->name eq $self->dhandler_name) {
 if ($path eq '/') {
 undef $request_comp;
 last search; 
 } else {
 $path =~ s:/[^\/]+$::;
 $path ||= '/';
 }
 }
 if ($retry_count++ > $self->max_recurse) {
 error "could not find dhandler after " . $self->max_recurse . " tries (infinite loop bug?)";
 }
 redo search;
 }
 }

unless ($self->{request_comp} = $request_comp) {
 top_level_not_found_error "could not find component for initial path '$self->{top_path}' " .
 "(component roots are: " .
 join(", ", map { "'" . $_->[1] . "'" } $self->{interp}->comp_root_array) .
 ")";
 }

} elsif ( ! UNIVERSAL::isa( $request_comp, 'HTML::Mason::Component' ) ) {
 param_error "comp ($request_comp) must be a component path or a component object";
 }
 $self->{has_plugins} = 0;
 $self->{plugin_instances} = [];
 foreach my $plugin (@{ delete $self->{plugins} }) {
 $self->{has_plugins} = 1;
 my $plugin_instance = $plugin;
 unless (ref $plugin) {
 unless ($plugin_loaded{$plugin}) {
 {
 no strict 'refs';
 unless (defined(%{$plugin . "::"})) {
 eval "use $plugin;";
 die $@ if $@;
 }
 }
 $plugin_loaded{$plugin} = 1;
 }
 $plugin_instance = $plugin->new;
 }
 push @{$self->{plugin_instances}}, $plugin_instance;
 }
 $self->{plugin_instances_reverse} = [reverse(@{$self->{plugin_instances}})];
 if ($self->{autoflush} && !$self->interp->compiler->enable_autoflush) {
 die "Cannot use autoflush unless enable_autoflush is set";
 }

};

my $err = $@;
 if ($err and !$self->_aborted_or_declined($err)) {
 $self->_handle_error($err);
 } else {
 $self->{initialized} = 1;
 }
}
sub use_dhandlers
{
 my $self = shift;
 return defined $self->{dhandler_name} and length $self->{dhandler_name};
}
sub alter_superclass
{
 my $self = shift;
 my $new_super = shift;

my $class = caller;

my $isa_ref;
 {
 no strict 'refs';
 my @isa = @{ $class . '::ISA' };
 $isa_ref = \@isa;
 }
 for ( my $x = 0; $x <= $#{$isa_ref} ; $x++ )
 {
 if ( $isa_ref->[$x]->isa('HTML::Mason::Request') )
 {
 my $old_super = $isa_ref->[$x];

if ( $old_super ne $new_super )
 {
 $isa_ref->[$x] = $new_super;
 }

last;
 }
 }

{
 no strict 'refs';
 @{ $class . '::ISA' } = @{ $isa_ref };
 }

$class->valid_params( %{ $class->valid_params } );
}
sub exec {
 my ($self) = @_;
 return unless $self->initialized;

local $SIG{'__DIE__'} = $self->component_error_handler
 if $self->component_error_handler;
 if ($self->{execd}++) {
 error "Can only call exec() once for a given request object. Did you want to use a subrequest?";
 }
 error "subrequest depth > " . $self->max_recurse . " (infinite subrequest loop?)"
 if $self->request_depth > $self->max_recurse;
 local $HTML::Mason::Commands::m = $self;
 $self->{top_stack} = undef;
 my $wantarray = wantarray;
 my @result;
 $self->{request_buffer} = $self->interp->preallocated_output_buffer;
 $self->{request_buffer} = '';

eval {
 my $request_comp = $self->request_comp;
 my $first_comp;
 {
 my @wrapper_chain = ($request_comp);

for (my $parent = $request_comp->parent; $parent; $parent = $parent->parent) {
 unshift(@wrapper_chain,$parent);
 error "inheritance chain length > " . $self->max_recurse . " (infinite inheritance loop?)"
 if (@wrapper_chain > $self->max_recurse);
 }

$first_comp = $wrapper_chain[0];
 $self->{wrapper_chain} = [@wrapper_chain];
 $self->{wrapper_index} = { map
 { $wrapper_chain[$_]->comp_id => $_ }
 (0..$#wrapper_chain)
 };
 }
 my $request_args = $self->{request_args};
 {
 local *SELECTED;
 tie *SELECTED, 'Tie::Handle::Mason';

my $old = select SELECTED;
 my $mods = {base_comp => $request_comp, store => \($self->{request_buffer})};

if ($self->{has_plugins}) {
 my $context = bless
 [$self, $request_args],
 'HTML::Mason::Plugin::Context::StartRequest';
 eval {
 foreach my $plugin_instance (@{$self->plugin_instances}) {
 $plugin_instance->start_request_hook( $context );
 }
 };
 if ($@) {
 select $old;
 rethrow_exception $@;
 }
 }

if ($wantarray) {
 @result = eval {$self->comp($mods, $first_comp, @$request_args)};
 } elsif (defined($wantarray)) {
 $result[0] = eval {$self->comp($mods, $first_comp, @$request_args)};
 } else {
 eval {$self->comp($mods, $first_comp, @$request_args)};
 }

my $error = $@;

if ($self->{has_plugins}) {
 my $context = bless
 [$self, $request_args, \$self->{request_buffer}, $wantarray, \@result, \$error],
 'HTML::Mason::Plugin::Context::EndRequest';
 eval {
 foreach my $plugin_instance (@{$self->{plugin_instances_reverse}}) {
 $plugin_instance->end_request_hook( $context );
 }
 };
 if ($@) {
 $error = $@;
 }
 }

select $old;
 rethrow_exception $error;
 }
 };
 $self->interp->purge_code_cache;
 my $err = $@;
 if ($err and !$self->_aborted_or_declined($err)) {
 $self->_handle_error($err);
 return;
 }
 if (length($self->{request_buffer}) > 0) {
 $self->out_method->($self->{request_buffer});
 }
 @result = ($err->aborted_value) if $self->aborted($err);
 @result = ($err->declined_value) if $self->declined($err);
 return $wantarray ? @result : defined($wantarray) ? $result[0] : undef;
}
sub _handle_error
{
 my ($self, $err) = @_;

$self->interp->purge_code_cache;

rethrow_exception $err if $self->is_subrequest;
 if (UNIVERSAL::can($err, 'format')) {
 $err->format($self->error_format);
 }
 if ($self->error_mode eq 'fatal') {
 rethrow_exception $err;
 } else {
 if ( UNIVERSAL::isa( $self->out_method, 'CODE' ) ) {
 local $HTML::Mason::Commands::m ||= $self;
 $self->out_method->("$err");
 } else {
 ${ $self->out_method } = "$err";
 }
 }
}
sub subexec
{
 my $self = shift;
 my $comp = shift;

$self->make_subrequest(comp=>$comp, args=>\@_)->exec;
}
sub make_subrequest
{
 my ($self, %params) = @_;
 my $interp = $self->interp;
 $params{comp} = absolute_comp_path($params{comp}, $self->current_comp->dir_path)
 if exists $params{comp} && !ref($params{comp});
 my %defaults = map { ($_, $self->$_()) } $self->_properties;

unless ( $params{out_method} )
 {
 $defaults{out_method} = sub {
 $self->print($_[0]);
 };
 }
 my $subreq =
 $interp->make_request(%defaults, %params,
 parent_request => $self,
 request_depth => $self->request_depth + 1);

return $subreq;
}
sub is_subrequest
{
 my ($self) = @_;

return $self->parent_request ? 1 : 0;
}
sub clear_and_abort
{
 my $self = shift;

$self->clear_buffer;
 $self->abort(@_);
}
sub abort
{
 my ($self, $aborted_value) = @_;
 HTML::Mason::Exception::Abort->throw( error => 'Request->abort was called', aborted_value => $aborted_value );
}
sub aborted {
 my ($self, $err) = @_;
 $err = $@ if !defined($err);
 return isa_mason_exception( $err, 'Abort' );
}
sub declined {
 my ($self, $err) = @_;
 $err = $@ if !defined($err);
 return isa_mason_exception( $err, 'Decline' );
}
sub _aborted_or_declined {
 my ($self, $err) = @_;
 return $self->aborted($err) || $self->declined($err);
}
sub cache
{
 my ($self, %options) = @_;
 my %old_cache_options;
 if ($self->data_cache_api eq '1.0') {
 %old_cache_options = %options;
 %options = ();
 }
 if ($self->data_cache_defaults) {
 %options = (%{$self->data_cache_defaults}, %options);
 }
 if ($self->data_cache_api eq 'chi') {
 my $chi_root_class = delete($options{chi_root_class}) || 'CHI';
 load_pkg($chi_root_class);
 if (!exists($options{namespace})) {
 $options{namespace} = $self->current_comp->comp_id;
 }
 if (!exists($options{driver})) {
 $options{driver} = $self->interp->cache_dir ? 'File' : 'Memory';
 }
 $options{root_dir} ||= $self->interp->cache_dir;
 return $chi_root_class->new(%options);
 }

$options{cache_root} ||= $self->interp->cache_dir;
 $options{namespace} ||= compress_path($self->current_comp->comp_id);
 my $cache_class = $self->interp->cache_dir ? 'Cache::FileCache' : 'Cache::MemoryCache';
 if ($options{cache_class}) {
 $cache_class = $options{cache_class};
 $cache_class = "Cache::$cache_class" unless $cache_class =~ /::/;
 delete($options{cache_class});
 }
 my $mason_cache_class = "HTML::Mason::$cache_class";
 unless (pkg_loaded($mason_cache_class)) {
 load_pkg('Cache::Cache', '$m->cache requires the Cache::Cache module, available from CPAN.');
 load_pkg($cache_class, 'Fix your Cache::Cache installation or choose another cache class.');
 eval sprintf('package %s; use base qw(HTML::Mason::Cache::BaseCache %s); use vars qw($' . 'VERSION); $' . 'VERSION = 1.0;',
 $mason_cache_class, $cache_class);
 error "Error constructing mason cache class $mason_cache_class: $@" if $@;
 }

my $cache = $mason_cache_class->new (\%options)
 or error "could not create cache object";
 if ($self->data_cache_api eq '1.0') {
 return $self->_cache_1_x($cache, %old_cache_options);
 } else {
 return $cache;
 }
}
sub _cache_1_x
{
 my ($self, $cache, %options) = @_;

my $action = $options{action} || 'retrieve';
 my $key = $options{key} || 'main';

if ($action eq 'retrieve') {
 if (my @invalids = grep(!/^(expire_if|action|key|busy_lock|keep_in_memory|tie_class)$/, keys(%options))) {
 param_error "cache: invalid parameter '$invalids[0]' for action '$action'\n";
 }
 if (my $sub = $options{expire_if}) {
 if (my $obj = $cache->get_object($key)) {
 if ($sub->($obj->get_created_at)) {
 $cache->expire($key);
 }
 }
 }
 if (my $result = $cache->get($key, ($options{busy_lock} ? (busy_lock=>$options{busy_lock}) : ()))) {
 return $result;
 } else {
 return undef;
 }

} elsif ($action eq 'store') {
 if (my @invalids = grep(!/^(expire_(at|next|in)|action|key|value|keep_in_memory|tie_class)$/, keys(%options))) {
 param_error "cache: invalid parameter '$invalids[0]' for action '$action'\n";
 }
 param_error "cache: no store value provided" unless exists($options{value});
 my $expires_in;
 my $time = time;
 if (exists($options{expire_at})) {
 param_error "cache: invalid expire_at value '$options{expire_at}' - must be a numeric time value\n" if $options{expire_at} !~ /^[0-9]+$/;
 $expires_in = $options{expire_at} - $time;
 } elsif (exists($options{expire_next})) {
 my $term = $options{expire_next};
 my ($sec, $min, $hour) = localtime($time);
 if ($term eq 'hour') {
 $expires_in = 60*(59-$min)+(60-$sec);
 } elsif ($term eq 'day') {
 $expires_in = 3600*(23-$hour)+60*(59-$min)+(60-$sec);
 } else {
 param_error "cache: invalid expire_next value '$term' - must be 'hour' or 'day'\n";
 }
 } elsif (exists($options{expire_in})) {
 $expires_in = $options{expire_in};
 }
 my $value = $options{value};
 $cache->set($key, $value, $expires_in);
 return $value;

} elsif ($action eq 'expire') {
 my @keys = (ref($key) eq 'ARRAY') ? @$key : ($key);
 foreach my $key (@keys) {
 $cache->expire($key);
 }

} elsif ($action eq 'keys') {
 return $cache->get_keys;
 }
}
sub cache_self {
 my ($self, %options) = @_;

return if $self->{top_stack}->[STACK_IN_CALL_SELF]->{'CACHE_SELF'};

my (%store_options, %retrieve_options);
 my ($expires_in, $key, $cache);
 if ($self->data_cache_api eq '1.0') {
 foreach (qw(key expire_if busy_lock)) {
 $retrieve_options{$_} = $options{$_} if (exists($options{$_}));
 }
 foreach (qw(key expire_at expire_next expire_in)) {
 $store_options{$_} = $options{$_} if (exists($options{$_}));
 }
 } else {
 foreach (qw(expire_if busy_lock)) {
 $retrieve_options{$_} = delete($options{$_}) if (exists($options{$_}));
 }
 $expires_in = delete $options{expires_in} || delete $options{expire_in} || 'never';
 $key = delete $options{key} || '__mason_cache_self__';
 $cache = $self->cache(%options);
 }

my ($output, @retval, $error);

my $cached =
 ( $self->data_cache_api eq '1.0' ?
 $self->cache(%retrieve_options) :
 $cache->get($key, %retrieve_options)
 );

if ($cached) {
 ($output, my $retval) = @$cached;
 @retval = @$retval;
 } else {
 $self->call_self( \$output, \@retval, \$error, 'CACHE_SELF' );
 rethrow_exception $error
 unless ($self->_aborted_or_declined($error));

my $value = [$output, \@retval];
 if ($self->data_cache_api eq '1.0') {
 $self->cache(action=>'store', key=>$key, value=>$value, %store_options);
 } else {
 $cache->set($key, $value, $expires_in);
 }
 }
 $self->print($output);
 rethrow_exception $error;
 return (@retval, 1);
}
sub call_self
{
 my ($self, $output, $retval, $error, $tag) = @_;
 $tag ||= 'DEFAULT';
 my $top_stack = $self->{top_stack};
 $top_stack->[STACK_IN_CALL_SELF] ||= {};
 return if $top_stack->[STACK_IN_CALL_SELF]->{$tag};
 local $top_stack->[STACK_IN_CALL_SELF]->{$tag} = 1;
 my $wantarray =
 ( defined $retval ?
 ( UNIVERSAL::isa( $retval, 'ARRAY' ) ? 1 : 0 ) :
 undef
 );
 my $dummy;
 $output ||= \$dummy;
 $retval ||= \$dummy;
 local $top_stack->[STACK_BUFFER] = $output;
 my $comp = $top_stack->[STACK_COMP];
 my $args = $top_stack->[STACK_ARGS];
 my @result;
 eval {
 if ($wantarray) {
 @$retval = $comp->run(@$args);
 } elsif (defined $wantarray) {
 $$retval = $comp->run(@$args);
 } else {
 $comp->run(@$args);
 }
 };
 if ($@) {
 if ($error) {
 $$error = $@;
 } else {
 die $@;
 }
 }
 return 1;
}
sub call_dynamic {
 my ($m, $key, @args) = @_;
 my $comp = ($m->current_comp->is_subcomp) ? $m->current_comp->owner : $m->current_comp;
 if (!defined($comp->dynamic_subs_request) or $comp->dynamic_subs_request ne $m) {
 $comp->dynamic_subs_init;
 $comp->dynamic_subs_request($m);
 }

return $comp->run_dynamic_sub($key, @args);
}
sub call_next {
 my ($self,@extra_args) = @_;
 my $comp = $self->fetch_next
 or error "call_next: no next component to invoke";
 return $self->comp({base_comp=>$self->request_comp}, $comp, @{$self->current_args}, @extra_args);
}
sub caller
{
 my ($self) = @_;
 return $self->callers(1);
}
sub callers
{
 my ($self, $levels_back) = @_;
 if (defined($levels_back)) {
 my $frame = $self->_stack_frame($levels_back);
 return unless defined $frame;
 return $frame->[STACK_COMP];
 } else {
 my $depth = $self->depth;
 return map($_->[STACK_COMP], $self->_stack_frames);
 }
}
sub caller_args
{
 my ($self, $levels_back) = @_;
 param_error "caller_args expects stack level as argument" unless defined $levels_back;

my $frame = $self->_stack_frame($levels_back);
 return unless $frame;
 my $args = $frame->[STACK_ARGS];
 return wantarray ? @$args : { @$args };
}
sub comp_exists
{
 my ($self, $path) = @_;
 return $self->fetch_comp($path) ? 1 : 0;
}
sub decline
{
 my ($self) = @_;

$self->clear_buffer;
 my $subreq = $self->make_subrequest
 (comp => $self->{top_path},
 args => $self->{request_args},
 declined_comps => {$self->request_comp->comp_id, 1, %{$self->{declined_comps}}});
 my $retval = $subreq->exec;
 HTML::Mason::Exception::Decline->throw( error => 'Request->decline was called', declined_value => $retval );
}
sub depth
{
 return scalar @{ $_[0]->{stack} };
}
sub fetch_comp
{
 my ($self, $path, $current_comp, $error, $exists_only) = @_;

return undef unless defined($path);
 $current_comp ||= $self->{top_stack}->[STACK_COMP];

return $self->_fetch_comp($path, $current_comp, $error)
 unless $self->{use_internal_component_caches};

my $fetch_comp_cache = $current_comp->{fetch_comp_cache};
 unless (defined($fetch_comp_cache->{$path})) {
 if ($path =~ /^(?:SELF|REQUEST)/) {
 return $self->_fetch_comp($path, $current_comp, $error);
 } else {
 $fetch_comp_cache->{$path} =
 $self->_fetch_comp($path, $current_comp, $error);
 Scalar::Util::weaken($fetch_comp_cache->{$path}) if can_weaken;
 }
 }

return $fetch_comp_cache->{$path};
}
sub _fetch_comp
{
 my ($self, $path, $current_comp, $error) = @_;
 if ($path eq 'SELF') {
 return $self->base_comp;
 }
 if ($path eq 'PARENT') {
 my $c = $current_comp->parent;
 $$error = "PARENT designator used from component with no parent" if !$c && defined($error);
 return $c;
 }
 if ($path eq 'REQUEST') {
 return $self->request_comp;
 }
 if (index($path,':') != -1) {
 my $method_comp;
 my ($owner_path,$method_name) = split(':',$path,2);
 if (my $owner_comp = $self->fetch_comp($owner_path, $current_comp, $error)) {
 if ($owner_comp->_locate_inherited('methods',$method_name,\$method_comp)) {
 return $method_comp;
 } else {
 $$error = "no such method '$method_name' for component " . $owner_comp->title if defined($error);
 }
 } else {
 $$error ||= "could not find component for path '$owner_path'\n" if defined($error);
 }

return $method_comp;
 }
 if ($path !~ /\//) {
 if (my $subcomp = $current_comp->subcomps($path)) {
 return $subcomp;
 }
 if ($current_comp->is_subcomp and my $subcomp = $current_comp->owner->subcomps($path)) {
 return $subcomp;
 }
 }
 $path = absolute_comp_path($path, $current_comp->dir_path);
 my $comp = $self->interp->load($path);

return $comp;
}
sub _fetch_next_helper {
 my ($self) = @_;
 my $index = $self->{wrapper_index}->{$self->current_comp->comp_id};
 unless (defined($index)) {
 my @callers = $self->callers;
 shift(@callers);
 while (my $comp = shift(@callers) and !defined($index)) {
 $index = $self->{wrapper_index}->{$comp->comp_id};
 }
 }
 return $index;
}
sub fetch_next {
 my ($self) = @_;
 my $index = $self->_fetch_next_helper;
 error "fetch_next: cannot find next component in chain"
 unless defined($index);
 return $self->{wrapper_chain}->[$index+1];
}
sub fetch_next_all {
 my ($self) = @_;
 my $index = $self->_fetch_next_helper;
 error "fetch_next_all: cannot find next component in chain"
 unless defined($index);
 my @wc = @{$self->{wrapper_chain}};
 return @wc[($index+1)..$#wc];
}
sub file
{
 my ($self,$file) = @_;
 my $interp = $self->interp;
 unless ( File::Spec->file_name_is_absolute($file) ) {
 my $context_comp =
 ( $self->current_comp->is_subcomp ?
 $self->current_comp->owner :
 $self->current_comp );

if ($context_comp->is_file_based) {
 my $source_dir = $context_comp->source_dir;
 $file = File::Spec->catfile( $source_dir, $file );
 } else {
 $file = File::Spec->catfile( File::Spec->rootdir, $file );
 }
 }
 my $content = read_file($file,1);
 return $content;
}
sub print
{
 my $self = shift;
 my $bufref =
 ( defined $self->{top_stack}
 ? $self->{top_stack}->[STACK_BUFFER]
 : \$self->{request_buffer}
 );
 for ( @_ ) {
 $$bufref .= $_ if defined;
 }

$self->flush_buffer if $self->{autoflush};
}
*out = \&print;
sub comp {
 my $self = shift;
 my %mods;
 %mods = (%{shift()}, %mods) while ref($_[0]) eq 'HASH';
 my $path;
 my $comp = shift;
 if (!ref($comp)) {
 die "comp called without component - must pass a path or component object"
 unless defined($comp);
 $path = $comp;
 my $error;
 $comp = $self->fetch_comp($path, undef, \$error)
 or error($error || "could not find component for path '$path'\n");
 }
 my $depth = $self->depth;
 error "$depth levels deep in component stack (infinite recursive call?)\n"
 if $depth >= $self->{max_recurse};
 my $filter_buffer = '';
 my $top_buffer = defined($mods{store}) ? $mods{store} : $self->{top_stack}->[STACK_BUFFER];
 my $stack_buffer = $comp->{has_filter} ? \$filter_buffer : $top_buffer;
 my $flushable = exists $mods{flushable} ? $mods{flushable} : 1;
 push @{ $self->{stack} },
 [ $comp, 
 \@_, 
 $stack_buffer, 
 \%mods, 
 $path, 
 undef, 
 undef, 
 $flushable, 
 ];
 local $self->{top_stack} = $self->{stack}->[-1];
 if ($self->{has_plugins}) {
 my $context = bless
 [$self, $comp, \@_],
 'HTML::Mason::Plugin::Context::StartComponent';

foreach my $plugin_instance (@{$self->{plugin_instances}}) {
 $plugin_instance->start_component_hook( $context );
 }
 }
 my $wantarray = wantarray;
 my @result;

eval {
 {
 if ($wantarray) {
 @result = $comp->run(@_);
 } elsif (defined $wantarray) {
 $result[0] = $comp->run(@_);
 } else {
 $comp->run(@_);
 }
 }
 };
 my $error = $@;
 if ($comp->{has_filter}) {
 if (defined($comp->filter)) {
 $$top_buffer .= $comp->filter->($filter_buffer);
 }
 $self->{top_stack}->[STACK_BUFFER] = $top_buffer;
 }
 if ($self->{has_plugins}) {
 my $context = bless
 [$self, $comp, \@_, $wantarray, \@result, \$error],
 'HTML::Mason::Plugin::Context::EndComponent';

foreach my $plugin_instance (@{$self->{plugin_instances_reverse}}) {
 $plugin_instance->end_component_hook( $context );
 }
 }
 pop @{ $self->{stack} };
 rethrow_exception $error if $error;
 return $wantarray ? @result : $result[0];
}
sub scomp {
 my $self = shift;
 my $buf;
 $self->comp({store => \$buf, flushable => 0},@_);
 return $buf;
}
sub has_content {
 my $self = shift;
 return defined($self->{top_stack}->[STACK_MODS]->{content});
}
sub content {
 my $self = shift;
 my $content = $self->{top_stack}->[STACK_MODS]->{content};
 return undef unless defined($content);
 my $err;
 my $buffer;
 my $save_frame = pop @{ $self->{stack} };
 {
 local $self->{top_stack} = $self->{stack}[-1];
 local $self->{top_stack}->[STACK_BUFFER] = \$buffer;
 local $self->{top_stack}->[STACK_BUFFER_IS_FLUSHABLE] = 0;
 local $self->{top_stack}->[STACK_HIDDEN_BUFFER] = $save_frame->[STACK_BUFFER];
 eval { $content->(); };
 $err = $@;
 }

push @{ $self->{stack} }, $save_frame;

rethrow_exception $err;
 return $buffer;
}
sub notes {
 my $self = shift;
 return $self->{notes} unless @_;

my $key = shift;
 return $self->{notes}{$key} unless @_;

return $self->{notes}{$key} = shift;
}
sub clear_buffer
{
 my $self = shift;

foreach my $frame (@{$self->{stack}}) {
 my $bufref = $frame->[STACK_BUFFER];
 $$bufref = '';
 $bufref = $frame->[STACK_HIDDEN_BUFFER];
 $$bufref = '' if $bufref;
 }
}
sub flush_buffer
{
 my $self = shift;

$self->out_method->($self->{request_buffer})
 if length $self->{request_buffer};
 $self->{request_buffer} = '';

if ( $self->{top_stack}->[STACK_BUFFER_IS_FLUSHABLE]
 && $self->{top_stack}->[STACK_BUFFER] )
 {
 my $comp = $self->{top_stack}->[STACK_COMP];
 if ( $comp->has_filter
 && defined $comp->filter )
 {
 $self->out_method->
 ( $comp->filter->( ${ $self->{top_stack}->[STACK_BUFFER] } ) );
 }
 else
 {
 $self->out_method->( ${ $self->{top_stack}->[STACK_BUFFER] } );
 }
 ${$self->{top_stack}->[STACK_BUFFER]} = '';
 }
}
sub request_args
{
 my ($self) = @_;
 if (wantarray) {
 return @{$self->{request_args}};
 } else {
 return { @{$self->{request_args}} };
 }
}
*top_args = \&request_args;
*top_comp = \&request_comp;
sub time
{
 my ($self) = @_;
 my $time = $self->interp->current_time;
 $time = time() if $time eq 'real';
 return $time;
}
sub debug_hook
{
 1;
}
sub _stack_frame {
 my ($self, $levels) = @_;
 my $depth = $self->depth;
 my $index;
 if ($levels < 0) {
 $index = (-1 * $levels) - 1;
 } else {
 $index = $depth-1 - $levels;
 }
 return if $index < 0 or $index >= $depth;
 return $self->{stack}->[$index];
}
sub _stack_frames {
 my ($self) = @_;

my $depth = $self->depth;
 return reverse map { $self->{stack}->[$_] } (0..$depth-1);
}
sub current_comp { return $_[0]->{top_stack}->[STACK_COMP] }
sub current_args { return $_[0]->{top_stack}->[STACK_ARGS] }
sub base_comp {
 my ($self) = @_;

return unless $self->{top_stack};

unless ( defined $self->{top_stack}->[STACK_BASE_COMP] ) {
 $self->_compute_base_comp_for_frame( $self->depth - 1 );
 }
 return $self->{top_stack}->[STACK_BASE_COMP];
}
sub _compute_base_comp_for_frame {
 my ($self, $frame_num) = @_;
 die "Invalid frame number: $frame_num" if $frame_num < 0;

my $frame = $self->{stack}->[$frame_num];

unless (defined($frame->[STACK_BASE_COMP])) {
 my $mods = $frame->[STACK_MODS];
 my $path = $frame->[STACK_PATH];
 my $comp = $frame->[STACK_COMP];

my $base_comp;
 if (exists($mods->{base_comp})) {
 $base_comp = $mods->{base_comp};
 } elsif (!$path ||
 $path =~ m/^(?:SELF|PARENT|REQUEST)(?:\:..*)?$/ ||
 ($comp->is_subcomp && !$comp->is_method)) {
 $base_comp = $self->_compute_base_comp_for_frame($frame_num-1);
 } elsif ($path =~ m/(.*):/) {
 my $calling_comp = $self->{stack}->[$frame_num-1]->[STACK_COMP];
 $base_comp = $self->fetch_comp($1, $calling_comp);
 } else {
 $base_comp = $comp;
 }
 $frame->[STACK_BASE_COMP] = $base_comp;
 }
 return $frame->[STACK_BASE_COMP];
}
package Tie::Handle::Mason;
sub TIEHANDLE
{
 my $class = shift;

return bless {}, $class;
}
sub PRINT
{
 my $self = shift;

my $old = select STDOUT;
 $HTML::Mason::Commands::m->print(@_);

select $old;
}
sub PRINTF
{
 my $self = shift;
 $self->PRINT(sprintf(shift, @_));
}
1;
}
BEGIN { $INC{q{HTML/Mason/ComponentSource.pm}} = 1;
package HTML::Mason::ComponentSource;
use strict;
use warnings;
use File::Basename;
use File::Spec;
use HTML::Mason::Exceptions( abbr => [qw(param_error error)] );
use Params::Validate qw(:all);
Params::Validate::validation_options( on_fail => sub { param_error join '', @_ } );
use HTML::Mason::MethodMaker
 ( read_only => [ qw(comp_id friendly_name last_modified comp_path comp_class extra) ],
 );
my %defaults = ( comp_class => 'HTML::Mason::Component' );
sub new
{
 my $class = shift;

return bless { %defaults, @_ }, $class
}
sub comp_source_ref
{
 my $self = shift;

my $source = eval { $self->{source_callback}->() };

rethrow_exception $@;

unless ( defined $source )
 {
 error "source callback returned no source for $self->{friendly_name} component";
 }

my $sourceref = ref($source) ? $source : \$source;
 return $sourceref;
}
sub comp_source { ${shift()->comp_source_ref} }
sub object_code
{
 my $self = shift;
 my %p = validate( @_, { compiler => { isa => 'HTML::Mason::Compiler' } } );

return $p{compiler}->compile( comp_source => $self->comp_source,
 name => $self->friendly_name,
 comp_path => $self->comp_path,
 comp_class => $self->comp_class,
 );
}
1;
}
BEGIN { $INC{q{HTML/Mason/Resolver.pm}} = 1;
package HTML::Mason::Resolver;
use strict;
use warnings;
use HTML::Mason::Exceptions( abbr => ['param_error', 'virtual_error'] );
use Params::Validate qw(:all);
Params::Validate::validation_options( on_fail => sub { param_error join '', @_ } );
use HTML::Mason::ComponentSource;
use Class::Container;
use base qw(Class::Container);
sub get_info {
 shift->_virtual;
}
sub glob_path {
 shift->_virtual;
}
sub _virtual
{
 my $self = shift;

my $sub = (caller(1))[3];
 $sub =~ s/.*::(.*?)$/$1/;
 virtual_error "$sub is a virtual method and must be overridden in " . ref($self);
}
1;
}
BEGIN { $INC{q{HTML/Mason/Resolver/File.pm}} = 1;
package HTML::Mason::Resolver::File;
use strict;
use warnings;
use Cwd;
use File::Glob;
use File::Spec;
use HTML::Mason::Tools qw(read_file_ref paths_eq);
use Params::Validate qw(:all);
use HTML::Mason::ComponentSource;
use HTML::Mason::Resolver;
use base qw(HTML::Mason::Resolver);
use HTML::Mason::Exceptions (abbr => ['param_error']);
sub get_info {
 my ($self, $path, $comp_root_key, $comp_root_path) = @_;
 my $srcfile = File::Spec->canonpath( File::Spec->catfile( $comp_root_path, $path ) );
 return unless -f $srcfile;
 my $modified = (stat _)[9];
 my $base = $comp_root_key eq 'MAIN' ? '' : "/$comp_root_key";
 $comp_root_key = undef if $comp_root_key eq 'MAIN';

return
 HTML::Mason::ComponentSource->new
 ( friendly_name => $srcfile,
 comp_id => "$base$path",
 last_modified => $modified,
 comp_path => $path,
 comp_class => 'HTML::Mason::Component::FileBased',
 extra => { comp_root => $comp_root_key },
 source_callback => sub { read_file_ref($srcfile) },
 );
}
sub glob_path {
 my ($self, $pattern, $comp_root_path) = @_;

my @files = File::Glob::bsd_glob($comp_root_path . $pattern);
 my $root_length = length $comp_root_path;
 my @paths;
 foreach my $file (@files) {
 next unless -f $file;
 if (substr($file, 0, $root_length) eq $comp_root_path) {
 push(@paths, substr($file, $root_length));
 }
 }
 return @paths;
}
sub apache_request_to_comp_path {
 my ($self, $r, @comp_root_array) = @_;

my $file = $r->filename;
 $file .= $r->path_info unless -f $file;
 $file = File::Spec->canonpath($file);

foreach my $root (map $_->[1], @comp_root_array) {
 if (paths_eq($root, substr($file, 0, length($root)))) {
 my $path = substr($file, length $root);
 $path = length $path ? join '/', File::Spec->splitdir($path) : '/';
 chop $path if $path ne '/' && substr($path, -1) eq '/';

return $path;
 }
 }
 return undef;
}
1;
}
BEGIN { $INC{q{HTML/Mason/Interp.pm}} = 1;
package HTML::Mason::Interp;
use strict;
use warnings;
use File::Basename;
use File::Path;
use File::Spec;
use File::Temp;
use HTML::Mason;
use HTML::Mason::Escapes;
use HTML::Mason::Request;
use HTML::Mason::Resolver::File;
use HTML::Mason::Tools qw(read_file taint_is_on load_pkg);
use HTML::Mason::Exceptions( abbr => [qw(param_error system_error wrong_compiler_error compilation_error error)] );
use Params::Validate qw(:all);
Params::Validate::validation_options( on_fail => sub { param_error join '', @_ } );
use Class::Container;
use base qw(Class::Container);
BEGIN
{
 __PACKAGE__->valid_params
 (
 autohandler_name =>
 { parse => 'string', default => 'autohandler', type => SCALAR,
 descr => "The filename to use for Mason's 'autohandler' capability" },

buffer_preallocate_size =>
 { parse => 'string', default => 0, type => SCALAR,
 descr => "Number of bytes to preallocate in request buffer" },

code_cache_max_size =>
 { parse => 'string', default => 'unlimited', type => SCALAR,
 descr => "The maximum number of components in the code cache" },

comp_root =>
 { parse => 'list',
 type => SCALAR|ARRAYREF,
 default => File::Spec->rel2abs( Cwd::cwd ),
 descr => "A string or array of arrays indicating the search path for component calls" },

compiler =>
 { isa => 'HTML::Mason::Compiler',
 descr => "A Compiler object for compiling components" },

data_dir =>
 { parse => 'string', optional => 1, type => SCALAR,
 descr => "A directory for storing cache files and other state information" },

dynamic_comp_root =>
 { parse => 'boolean', default => 0, type => BOOLEAN,
 descr => "Indicates whether the comp_root may be changed between requests" },

escape_flags =>
 { parse => 'hash_list', optional => 1, type => HASHREF,
 descr => "A list of escape flags to set (as if calling the set_escape() method" },

object_file_extension =>
 { parse => 'string', type => SCALAR, default => '.obj',
 descr => "Extension to add to the end of object files" },
 ignore_warnings_expr =>
 { parse => 'string', type => SCALAR|OBJECT, default => qr/Subroutine .* redefined/i,
 descr => "A regular expression describing Perl warning messages to ignore" },

preloads =>
 { parse => 'list', optional => 1, type => ARRAYREF,
 descr => "A list of components to load immediately when creating the Interpreter" },

resolver =>
 { isa => 'HTML::Mason::Resolver',
 descr => "A Resolver object for fetching components from storage" },

static_source =>
 { parse => 'boolean', default => 0, type => BOOLEAN,
 descr => "When true, we only compile source files once" },

static_source_touch_file =>
 { parse => 'string', optional => 1, type => SCALAR,
 descr => "A file that, when touched, causes Mason to clear its component caches" },

use_object_files =>
 { parse => 'boolean', default => 1, type => BOOLEAN,
 descr => "Whether to cache component objects on disk" },
 );

__PACKAGE__->contained_objects
 (
 resolver => { class => 'HTML::Mason::Resolver::File',
 descr => "This class is expected to return component information based on a component path" },
 compiler => { class => 'HTML::Mason::Compiler::ToObject',
 descr => "This class is used to translate component source into code" },
 request => { class => 'HTML::Mason::Request',
 delayed => 1,
 descr => "Objects returned by make_request are members of this class" },
 );
}
use HTML::Mason::MethodMaker
 ( read_only => [ qw(autohandler_name buffer_preallocate_size code_cache code_cache_min_size code_cache_max_size compiler data_dir dynamic_comp_root object_file_extension preallocated_output_buffer preloads resolver source_cache static_source static_source_touch_file use_internal_component_caches use_object_files) ],

read_write => [ map { [ $_ => __PACKAGE__->validation_spec->{$_} ] }
 qw(ignore_warnings_expr)
 ],

read_write_contained => { request =>
 [ [ autoflush => { type => BOOLEAN } ],
 [ data_cache_api => { type => SCALAR } ],
 [ data_cache_defaults => { type => HASHREF } ],
 [ dhandler_name => { type => SCALAR } ],
 [ error_format => { type => SCALAR } ],
 [ error_mode => { type => SCALAR } ],
 [ max_recurse => { type => SCALAR } ],
 [ out_method => { type => SCALARREF | CODEREF } ],
 [ plugins => { type => ARRAYREF } ],
 ]
 },
 );
sub new
{
 my $class = shift;
 my $self = $class->SUPER::new(@_);

$self->_initialize;
 return $self;
}
sub _initialize
{
 my ($self) = shift;
 $self->{code_cache} = {};
 $self->{source_cache} = {};
 $self->{files_written} = [];
 $self->{static_source_touch_file_lastmod} = 0;

$self->_assign_comp_root($self->{comp_root});
 $self->_check_data_dir;
 $self->_create_data_subdirs;
 $self->_initialize_escapes;
 $self->{preallocated_output_buffer} = ' ' x $self->buffer_preallocate_size;

$self->_set_code_cache_attributes;
 $self->{use_internal_component_caches} =
 ($self->{static_source} &&
 $self->{unlimited_code_cache} &&
 !$self->{dynamic_comp_root});

$self->_preload_components;
}
sub _check_data_dir
{
 my $self = shift;

return unless $self->{data_dir};

$self->{data_dir} = File::Spec->canonpath( $self->{data_dir} );
 param_error "data_dir '$self->{data_dir}' must be an absolute directory"
 unless File::Spec->file_name_is_absolute( $self->{data_dir} );
}
sub _create_data_subdirs
{
 my $self = shift;

if ($self->data_dir) {
 $self->_make_object_dir;
 $self->_make_cache_dir;
 } else {
 $self->{use_object_files} = 0;
 }
}
sub _initialize_escapes
{
 my $self = shift;
 foreach ( [ h => \&HTML::Mason::Escapes::html_entities_escape ],
 [ u => \&HTML::Mason::Escapes::url_escape ],
 )
 {
 $self->set_escape(@$_);
 }

if ( my $e = delete $self->{escape_flags} )
 {
 while ( my ($flag, $code) = each %$e )
 {
 $self->set_escape( $flag => $code );
 }
 }
}
sub _set_code_cache_attributes
{
 my $self = shift;

$self->{unlimited_code_cache} = ($self->{code_cache_max_size} eq 'unlimited');
 unless ($self->{unlimited_code_cache}) {
 $self->{code_cache_min_size} = $self->{code_cache_max_size} * 0.75;
 }
}
sub _preload_components
{
 my $self = shift;

return unless $self->preloads;

foreach my $pattern (@{$self->preloads}) {
 error "preload pattern '$pattern' must be an absolute path"
 unless File::Spec->file_name_is_absolute($pattern);
 my %path_hash;
 foreach my $pair ($self->comp_root_array) {
 my $root = $pair->[1];
 foreach my $path ($self->resolver->glob_path($pattern, $root)) {
 $path_hash{$path}++;
 }
 }
 my @paths = keys(%path_hash);
 warn "Didn't find any components for preload pattern '$pattern'"
 unless @paths;
 foreach (@paths)
 {
 $self->load($_)
 or error "Cannot load component $_, found via pattern $pattern";
 }
 }
}
sub object_dir { my $self = shift; return $self->data_dir ? File::Spec->catdir( $self->data_dir, 'obj' ) : ''; }
sub object_create_marker_file { my $self = shift; return $self->object_dir ? File::Spec->catfile($self->object_dir, '.__obj_create_marker') : ''; }
sub cache_dir { my $self = shift; return $self->data_dir ? File::Spec->catdir( $self->data_dir, 'cache' ) : ''; }
sub _make_data_subdir
{
 my ($self, $dir) = @_;

unless (-d $dir) {
 my @newdirs = eval { mkpath( $dir, 0, 0775 ) };
 if ($@) {
 my $user = getpwuid($<);
 my $group = getgrgid($();
 my $data_dir = $self->data_dir;
 error "Cannot create directory '$dir' ($@) for user '$user', group '$group'. " .
 "Perhaps you need to create or set permissions on your data_dir ('$data_dir'). ";
 }
 $self->push_files_written(@newdirs);
 }
}
sub _make_object_dir
{
 my ($self) = @_;

my $object_dir = $self->object_dir;
 $self->_make_data_subdir($object_dir);
 my $object_create_marker_file = $self->object_create_marker_file;
 unless (-f $object_create_marker_file) {
 open my $fh, ">$object_create_marker_file"
 or system_error "Could not create '$object_create_marker_file': $!";
 $self->push_files_written($object_create_marker_file);
 }
}
sub _make_cache_dir
{
 my ($self) = @_;

my $cache_dir = $self->cache_dir;
 $self->_make_data_subdir($cache_dir);
}
sub exec {
 my $self = shift;
 my $comp = shift;
 $self->make_request(comp=>$comp, args=>\@_)->exec;
}
sub make_request {
 my $self = shift;

return $self->create_delayed_object( 'request', interp => $self, @_ );
}
sub comp_exists {
 my ($self, $path) = @_;
 return $self->resolve_comp_path_to_source($path);
}
sub load {
 my ($self, $path) = @_;
 my ($maxfilemod, $objfile, $objfilemod);
 my $code_cache = $self->{code_cache};
 my $resolver = $self->{resolver};
 unless (substr($path, 0, 1) eq '/') {
 error "Component path given to Interp->load must be absolute (was given $path)";
 }
 my $source = $self->resolve_comp_path_to_source($path);
 return unless defined $source;
 my $comp_id = $source->comp_id;
 my $srcmod = $source->last_modified;
 if ( exists $code_cache->{$comp_id} &&
 ( $self->static_source || $code_cache->{$comp_id}->{lastmod} >= $srcmod )
 ) {
 return $code_cache->{$comp_id}->{comp};
 }

if ($self->{use_object_files}) {
 $objfile = $self->comp_id_to_objfile($comp_id);

my @stat = stat $objfile;
 if ( @stat && ! -f _ ) {
 error "The object file '$objfile' exists but it is not a file!";
 }

if ($self->static_source) {
 $objfilemod = @stat ? $srcmod : 0;
 } else {
 $objfilemod = @stat ? $stat[9] : 0;
 }
 }

my $comp;
 if ($objfile) {
 if ($objfilemod < $srcmod) {
 $self->compiler->compile_to_file( file => $objfile, source => $source);
 }
 $comp = eval { $self->eval_object_code( object_file => $objfile ) };

if (!UNIVERSAL::isa($comp, 'HTML::Mason::Component')) {
 if (!defined($@) || $@ !~ /failed in require/) {
 $self->compiler->compile_to_file( file => $objfile, source => $source);
 $comp = eval { $self->eval_object_code( object_file => $objfile ) };
 }

if (!UNIVERSAL::isa($comp, 'HTML::Mason::Component')) {
 my $error = $@ ? $@ : "Could not get HTML::Mason::Component object from object file '$objfile'";
 $self->_compilation_error( $source->friendly_name, $error );
 }
 }
 } else {
 my $object_code = $source->object_code( compiler => $self->compiler );
 $comp = eval { $self->eval_object_code( object_code => $object_code ) };
 $self->_compilation_error( $source->friendly_name, $@ ) if $@;
 }
 $comp->assign_runtime_properties($self, $source);
 $self->delete_from_code_cache($comp_id);
 $code_cache->{$comp_id} = { lastmod => $srcmod, comp => $comp };

return $comp;
}
sub delete_from_code_cache {
 my ($self, $comp_id) = @_;
 return unless defined $self->{code_cache}{$comp_id}{comp};

delete $self->{code_cache}{$comp_id};
 return;
}
sub comp_id_to_objfile {
 my ($self, $comp_id) = @_;

return File::Spec->catfile
 ( $self->object_dir,
 $self->compiler->object_id,
 ( split /\//, $comp_id ),
 ) . $self->object_file_extension;
}
sub flush_code_cache {
 my $self = shift;
 if ($self->use_internal_component_caches) {
 foreach my $entry (values %{$self->{code_cache}}) {
 my $comp = $entry->{comp};
 $comp->flush_internal_caches;
 }
 }
 $self->{code_cache} = {};
 $self->{source_cache} = {};
}
sub purge_code_cache {
 my ($self) = @_;

return if $self->{unlimited_code_cache};
 my $current_size = scalar(keys(%{$self->{code_cache}}));
 if ($current_size > $self->code_cache_max_size) {
 my $code_cache = $self->{code_cache};
 my $min_size = $self->code_cache_min_size;
 my $decay_factor = 0.75;

my @elems;
 while (my ($path,$href) = each(%{$code_cache})) {
 push(@elems,[$path,$href->{comp}->mfu_count,$href->{comp}]);
 }
 @elems = sort { $a->[1] <=> $b->[1] } @elems;
 while (($current_size > $min_size) and @elems) {
 $self->delete_from_code_cache(shift(@elems)->[0]);
 $current_size--;
 }
 foreach my $elem (@elems) {
 $elem->[2]->mfu_count( $elem->[2]->mfu_count * $decay_factor );
 }
 }
}
sub remove_object_files
{
 my $self = shift;

my $object_dir = $self->object_dir;
 if (-d $object_dir) {
 my $temp_dir = File::Temp::tempdir(DIR => $self->data_dir);
 rename($object_dir, File::Spec->catdir( $temp_dir, 'target' ) )
 or die "could not rename '$object_dir' to '$temp_dir': $@";
 $self->_make_object_dir;
 rmtree($temp_dir);
 } else {
 $self->_make_object_dir;
 }
}
sub check_static_source_touch_file
{
 my $self = shift;

if (my $touch_file = $self->static_source_touch_file) {
 return unless -f $touch_file;
 my $touch_file_lastmod = (stat($touch_file))[9];
 if ($touch_file_lastmod > $self->{static_source_touch_file_lastmod}) {
 if ($self->use_object_files) {
 my $object_create_marker_file = $self->object_create_marker_file;
 if (!-e $object_create_marker_file ||
 (stat($object_create_marker_file))[9] < $touch_file_lastmod) {
 $self->remove_object_files;
 }
 }
 $self->flush_code_cache;
 $self->{static_source_touch_file_lastmod} = $touch_file_lastmod;
 }
 }
}
sub make_component {
 my $self = shift;

my %p = validate(@_, { comp_source => { type => SCALAR, optional => 1 },
 comp_file => { type => SCALAR, optional => 1 },
 name => { type => SCALAR, optional => 1 } });

$p{comp_source} = read_file(delete $p{comp_file}) if exists $p{comp_file};
 param_error "Must specify either 'comp_source' or 'comp_file' parameter to 'make_component()'"
 unless defined $p{comp_source};

$p{name} ||= '<anonymous component>';

my $source = HTML::Mason::ComponentSource->new( friendly_name => $p{name},
 comp_path => $p{name},
 comp_id => undef,
 last_modified => time,
 comp_class => 'HTML::Mason::Component',
 source_callback => sub { $p{comp_source} },
 );

my $object_code = $source->object_code( compiler => $self->compiler);

my $comp = eval { $self->eval_object_code( object_code => $object_code ) };
 $self->_compilation_error( $p{name}, $@ ) if $@;

$comp->assign_runtime_properties($self, $source);

return $comp;
}
sub set_global
{
 my ($self, $decl, @values) = @_;
 param_error "Interp->set_global: expects a variable name and one or more values"
 unless @values;
 my ($prefix, $name) = ($decl =~ s/^([\$@%])//) ? ($1, $decl) : ('$', $decl);

my $varname = sprintf("%s::%s",$self->compiler->in_package,$name);
 no strict 'refs';
 if ($prefix eq '$') {
 $$varname = $values[0];
 } elsif ($prefix eq '@') {
 @$varname = @values;
 } else {
 %$varname = @values;
 }
}
sub comp_root
{
 my $self = shift;

if (my $new_comp_root = shift) {
 die "cannot assign new comp_root unless dynamic_comp_root parameter is set"
 unless $self->dynamic_comp_root;
 $self->_assign_comp_root($new_comp_root);
 }
 if (@{$self->{comp_root}} == 1 and $self->{comp_root}[0][0] eq 'MAIN') {
 return $self->{comp_root}[0][1];
 } else {
 return $self->{comp_root};
 }
}
sub comp_root_array
{
 return @{ $_[0]->{comp_root} };
}
sub _assign_comp_root
{
 my ($self, $new_comp_root) = @_;
 if (!ref($new_comp_root)) {
 $new_comp_root = [[ MAIN => $new_comp_root ]];
 } elsif (ref($new_comp_root) ne 'ARRAY') {
 die "Component root $new_comp_root must be a scalar or array reference";
 }
 my $comp_root_key_map = $self->{comp_root_key_map} ||= {};
 foreach my $pair (@$new_comp_root) {
 param_error "Multiple-path component root must consist of a list of two-element lists"
 if ref($pair) ne 'ARRAY';
 param_error "Component root key '$pair->[0]' cannot contain slash"
 if $pair->[0] =~ /\//;
 $pair->[1] = File::Spec->canonpath( $pair->[1] );
 param_error "comp_root path '$pair->[1]' is not an absolute directory"
 unless File::Spec->file_name_is_absolute( $pair->[1] );

my ($key, $path) = @$pair;
 if (my $orig_path = $comp_root_key_map->{$key}) {
 if ($path ne $orig_path) {
 die "comp_root key '$key' was originally associated with '$path', cannot change to '$orig_path'";
 }
 } else {
 $comp_root_key_map->{$key} = $path;
 }
 }
 $self->{comp_root} = $new_comp_root;
}
sub resolve_comp_path_to_source
{
 my ($self, $path) = @_;

my $source;
 if ($self->{static_source}) {
 my $source_cache = $self->{source_cache};
 foreach my $pair (@{$self->{comp_root}}) {
 my $source_cache_for_root = $source_cache->{$pair->[0]} ||= {};
 unless (exists($source_cache_for_root->{$path})) {
 $source_cache_for_root->{$path}
 = $self->{resolver}->get_info($path, @$pair);
 }
 last if $source = $source_cache_for_root->{$path};
 }
 } else {
 my $resolver = $self->{resolver};
 foreach my $pair ($self->comp_root_array) {
 last if $source = $resolver->get_info($path, @$pair);
 }
 }
 return $source;
}
sub files_written
{
 my $self = shift;
 return @{$self->{files_written}};
}
sub push_files_written
{
 my $self = shift;
 my $fref = $self->{'files_written'};
 push(@$fref,@_);
}
sub find_comp_upwards
{
 my ($self, $startpath, $name) = @_;
 $startpath =~ s{/+$}{};
 do {
 my $comp = $self->load("$startpath/$name");
 return $comp if $comp;
 } while $startpath =~ s{/+[^/]*$}{};

return; 
}
sub eval_object_code
{
 my ($self, %p) = @_;
 my $ignore_expr = $self->ignore_warnings_expr;
 my ($comp, $err);
 my $warnstr = '';

{
 local $^W = $ignore_expr eq '.' ? 0 : 1;
 local $SIG{__WARN__} =
 ( $ignore_expr ?
 ( $ignore_expr eq '.' ?
 sub { } :
 sub { $warnstr .= $_[0] if $_[0] !~ /$ignore_expr/ }
 ) :
 sub { $warnstr .= $_[0] } );

$comp = $self->_do_or_eval(\%p);
 }

$err = $warnstr . $@;
 if ($err) {
 $err =~ s/has too many errors\..+/has too many errors./s;
 compilation_error $err;
 } else {
 return $comp;
 }
}
sub _do_or_eval
{
 my ($self, $p) = @_;

if ($p->{object_file}) {
 return do $p->{object_file};
 } else {
 (${$p->{object_code}}) = ${$p->{object_code}} =~ /^(.*)/s if taint_is_on;

return eval ${$p->{object_code}};
 }
}
sub _compilation_error {
 my ($self, $filename, $err) = @_;

HTML::Mason::Exception::Compilation->throw(error=>$err, filename=>$filename);
}
sub object_file {
 my ($self, $comp) = @_;
 return $comp->persistent ?
 $self->comp_id_to_objfile($comp->comp_id) :
 undef;
}
sub use_autohandlers
{
 my $self = shift;
 return defined $self->{autohandler_name} and length $self->{autohandler_name};
}
sub status_as_html {
 my ($self, %p) = @_;

my $comp_source = <<'EOF';
<h3>Interpreter properties:</h3>
<blockquote>
 <h4>Startup options:</h4>
 <tt>
<table width="100%">
<%perl>
foreach my $property (sort keys %$interp) {
    my $val = $interp->{$property};

    my $default = ( defined $val && defined $valid{$property}{default} && $val eq $valid{$property}{default} ) || ( ! defined $val && exists $valid{$property}{default} && ! defined $valid{$property}{default} );

    my $display = $val;
    if (ref $val) {
        $display = '<font color="darkred">';
        # only object can ->can, others die
        my $is_object = eval { $val->can('anything'); 1 };
        if ($is_object) {
            $display .= ref $val . ' object';
        } else {
            if (UNIVERSAL::isa($val, 'ARRAY')) {
                $display .= 'ARRAY reference - [ ';
                $display .= join ', ', @$val;
                $display .= '] ';
            } elsif (UNIVERSAL::isa($val, 'HASH')) {
                $display .= 'HASH reference - { ';
                my @pairs;
                while (my ($k, $v) = each %$val) {
                   push @pairs, "$k => $v";
                }
                $display .= join ', ', @pairs;
                $display .= ' }';
            } else {
                $display = ref $val . ' reference';
            }
        }
        $display .= '</font>';
    }

    defined $display && $display =~ s,([\x00-\x1F]),'<font color="purple">control-' . chr( ord('A') + ord($1) - 1 ) . '</font>',eg; # does this work for non-ASCII?
</%perl>
 <tr valign="top" cellspacing="10">
  <td>
    <% $property | h %>
  </td>
  <td>
   <% defined $display ? $display : '<i>undef</i>' %>
   <% $default ? '<font color=green>(default)</font>' : '' %>
  </td>
 </tr>
% }
</table>
  </tt>

 <h4>Components in memory cache:</h4>
 <tt>
% my $cache;
% if ($cache = $interp->code_cache and %$cache) {
%   foreach my $key (sort keys %$cache) {
      <% $key |h%> (modified <% scalar localtime $cache->{$key}->{lastmod} %>)
      <br>
%   }
% } else {
    <I>None</I>
% }
  </tt>
</blockquote>

<%args>
 $interp   # The interpreter we'll elucidate
 %valid    # Default values for interp member data
</%args>
EOF

my $comp = $self->make_component(comp_source => $comp_source);
 my $out;

my $args = [interp => $self, valid => $self->validation_spec];
 $self->make_request(comp=>$comp, args=>$args, out_method=>\$out, %p)->exec;

return $out;
}
sub set_escape
{
 my $self = shift;
 my %p = @_;

while ( my ($name, $sub) = each %p )
 {
 my $flag_regex = $self->compiler->lexer->escape_flag_regex;

param_error "Invalid escape name ($name)"
 if $name !~ /^$flag_regex$/ || $name =~ /^n$/;

my $coderef;
 if ( ref $sub )
 {
 $coderef = $sub;
 }
 else
 {
 if ( $sub =~ /^\w+$/ )
 {
 no strict 'refs';
 unless ( defined &{"HTML::Mason::Escapes::$sub"} )
 {
 param_error "Invalid escape: $sub (no matching subroutine in HTML::Mason::Escapes";
 }

$coderef = \&{"HTML::Mason::Escapes::$sub"};
 }
 else
 {
 $coderef = eval $sub;
 param_error "Invalid escape: $sub ($@)" if $@;
 }
 }

$self->{escapes}{$name} = $coderef;
 }
}
sub remove_escape
{
 my $self = shift;

delete $self->{escapes}{ shift() };
}
sub apply_escapes
{
 my $self = shift;
 my $text = shift;

foreach my $flag (@_)
 {
 param_error "Invalid escape flag: $flag"
 unless exists $self->{escapes}{$flag};

$self->{escapes}{$flag}->(\$text);
 }

return $text;
}
1;
}
BEGIN { $INC{q{HTML/Mason.pm}} = 1;
package HTML::Mason;
use 5.006;
$HTML::Mason::VERSION = '1.42';
use HTML::Mason::Interp;
sub version
{
 return $HTML::Mason::VERSION;
}
1;
}
BEGIN { $INC{q{HTML/Mason/Component.pm}} = 1;
package HTML::Mason::Component;
use strict;
use warnings;
use File::Spec;
use HTML::Mason::Exceptions( abbr => [qw(param_error)] );
use HTML::Mason::Tools qw(absolute_comp_path can_weaken);
use Params::Validate qw(:all);
Params::Validate::validation_options( on_fail => sub { param_error join '', @_ } );
use HTML::Mason::Exceptions( abbr => ['error'] );
use HTML::Mason::MethodMaker
 ( read_only => [ qw(code comp_id compiler_id declared_args inherit_path inherit_start_path has_filter load_time) ],

read_write => [ [ dynamic_subs_request => { isa => 'HTML::Mason::Request' } ],
 [ mfu_count => { type => SCALAR } ],
 [ filter => { type => CODEREF } ],
 ]
 );
my %defaults = ( attr => {},
 declared_args => {},
 dynamic_subs_init => sub {},
 flags => {},
 methods => {},
 mfu_count => 0,
 subcomps => {},
 );
sub new
{
 my $class = shift;
 my $self = bless { %defaults, @_ }, $class;
 while (my ($name,$c) = each(%{$self->{subcomps}})) {
 $c->assign_subcomponent_properties($self,$name,0);
 Scalar::Util::weaken($c->{owner}) if can_weaken;
 }
 while (my ($name,$c) = each(%{$self->{methods}})) {
 $c->assign_subcomponent_properties($self,$name,1);
 Scalar::Util::weaken($c->{owner}) if can_weaken;
 }

return $self;
}
my $comp_count = 0;
sub assign_runtime_properties {
 my ($self, $interp, $source) = @_;
 $self->interp($interp);
 $self->{comp_id} = defined $source->comp_id ? $source->comp_id : "[anon ". ++$comp_count . "]";

$self->{path} = $source->comp_path;

$self->_determine_inheritance;

foreach my $c (values(%{$self->{subcomps}}), values(%{$self->{methods}})) {
 $c->assign_runtime_properties($interp, $source);
 }
 if ($interp->use_internal_component_caches) {
 $self->{fetch_comp_cache} = {};
 }
}
sub flush_internal_caches
{
 my ($self) = @_;

$self->{fetch_comp_cache} = {};
 delete($self->{parent_cache});
}
sub _determine_inheritance {
 my $self = shift;

my $interp = $self->interp;
 if (exists($self->{flags}->{inherit})) {
 if (defined($self->{flags}->{inherit})) {
 $self->{inherit_path} = absolute_comp_path($self->{flags}->{inherit}, $self->dir_path);
 }
 } elsif ( $interp->use_autohandlers ) {
 if ($self->name eq $interp->autohandler_name) {
 unless ($self->dir_path eq '/') {
 ($self->{inherit_start_path}) = $self->dir_path =~ m,^(.*/)?.*,s
 }
 } else {
 $self->{inherit_start_path} = $self->dir_path;
 }
 }
}
sub run {
 my $self = shift;

$self->{mfu_count}++;

$self->{code}->(@_);
}
sub dynamic_subs_init {
 my $self = shift;

error "cannot call a method or subcomponent from a <%shared> block"
 if $self->{in_dynamic_subs_init};

local $self->{in_dynamic_subs_init} = 1;

$self->{dynamic_subs_hash} = $self->{dynamic_subs_init}->();
 error "could not process <%shared> section (does it contain a return()?)"
 unless ref($self->{dynamic_subs_hash}) eq 'HASH';
}
sub run_dynamic_sub {
 my ($self, $key, @args) = @_;

error "call_dynamic: assert error - could not find code for key $key in component " . $self->title
 unless exists $self->{dynamic_subs_hash}->{$key};

return $self->{dynamic_subs_hash}->{$key}->(@args);
}
sub assign_subcomponent_properties {}
sub persistent { 0 }
sub is_subcomp { 0 }
sub is_method { 0 }
sub is_file_based { 0 }
sub title { return $_[0]->{comp_id} }
sub name { return $_[0]->{comp_id} }
sub path { return undef }
sub dir_path { return undef }
sub subcomps {
 my ($self,$key) = @_;
 if (defined($key)) {
 return $self->{subcomps}->{$key};
 } else {
 return $self->{subcomps};
 }
}
sub methods {
 my ($self,$key) = @_;
 if (defined($key)) {
 return $self->{methods}->{$key};
 } else {
 return $self->{methods};
 }
}
sub attributes { $_[0]->{attr} }
sub attr {
 my ($self,$name) = @_;
 my $value;
 if ($self->_locate_inherited('attr',$name,\$value)) {
 return $value;
 } else {
 error "no attribute '$name' for component " . $self->title;
 }
}
sub attr_if_exists {
 my ($self,$name) = @_;
 my $value;
 if ($self->_locate_inherited('attr',$name,\$value)) {
 return $value;
 } else {
 return undef;
 }
}
sub attr_exists {
 my ($self,$name) = @_;
 return $self->_locate_inherited('attr',$name);
}
sub call_method {
 my ($self,$name,@args) = @_;
 my $method;
 if ($self->_locate_inherited('methods',$name,\$method)) {
 HTML::Mason::Request->instance->comp({base_comp=>$self},$method,@args);
 } else {
 error "no method '$name' for component " . $self->title;
 }
}
sub scall_method {
 my ($self,$name,@args) = @_;
 my $method;
 if ($self->_locate_inherited('methods',$name,\$method)) {
 HTML::Mason::Request->instance->scomp({base_comp=>$self},$method,@args);
 } else {
 error "no method '$name' for component " . $self->title;
 }
}
sub method_exists {
 my ($self,$name) = @_;
 return $self->_locate_inherited('methods',$name);
}
sub _locate_inherited {
 my ($self,$field,$key,$ref) = @_;
 my $count = 0;
 for (my $comp = $self; $comp; $comp = $comp->parent) {
 if (exists($comp->{$field}->{$key})) {
 $$ref = $comp->{$field}->{$key} if $ref;
 return 1;
 }
 error "inheritance chain length > 32 (infinite inheritance loop?)"
 if ++$count > 32;
 }
 return 0;
}
sub flag {
 my ($self,$name) = @_;
 my %flag_defaults =
 (
 );
 if (exists($self->{flags}->{$name})) {
 return $self->{flags}->{$name};
 } elsif (exists($flag_defaults{$name})) {
 return $flag_defaults{$name};
 } else {
 error "invalid flag: $name";
 }
}
sub parent {
 my ($self) = @_;
 return $self->{parent_cache} if exists($self->{parent_cache});

my $interp = $self->interp;
 my $parent;
 if ($self->inherit_path) {
 $parent = $interp->load($self->inherit_path)
 or error(sprintf("cannot find inherit path '%s' for component '%s'",
 $self->inherit_path, $self->title));
 } elsif ($self->inherit_start_path) {
 $parent = $interp->find_comp_upwards($self->inherit_start_path, $interp->autohandler_name);
 }
 if ($interp->use_internal_component_caches) {
 $self->{parent_cache} = $parent;
 }

return $parent;
}
sub interp {
 my $self = shift;

if (@_) {
 validate_pos( @_, { isa => 'HTML::Mason::Interp' } );

$self->{interp} = $_[0];

Scalar::Util::weaken( $self->{interp} ) if can_weaken;
 } elsif ( ! defined $self->{interp} ) {
 die "The Interp object that this object contains has gone out of scope.\n";
 }

return $self->{interp};
}
sub object_file {
 my $self = shift;
 return $self->interp->object_file($self);
}
sub create_time {
 my $self = shift;
 return $self->load_time(@_);
}
1;
}
BEGIN { $INC{q{HTML/Mason/Component/FileBased.pm}} = 1;
package HTML::Mason::Component::FileBased;
use strict;
use warnings;
use File::Basename;
use File::Spec;
use HTML::Mason::Component;
use base qw(HTML::Mason::Component);
use HTML::Mason::Exceptions( abbr => ['error'] );
use HTML::Mason::MethodMaker ( read_only => [ qw(path source_file name dir_path) ] );
sub is_file_based { 1 }
sub persistent { 1 }
sub source_dir {
 my $dir = dirname($_[0]->source_file);
 return File::Spec->canonpath($dir);
}
sub title {
 my ($self) = @_;
 return $self->path . ($self->{source_root_key} ? " [".lc($self->{source_root_key})."]" : "");
}
sub assign_runtime_properties {
 my ($self, $interp, $source) = @_;

$self->{source_file} = $source->friendly_name;
 $self->{source_root_key} = $source->extra->{comp_root};
 @{$self}{ 'dir_path', 'name'} = $source->comp_path =~ m,^(.*/)?(.*),s;
 $self->{dir_path} =~ s,/$,, unless $self->{dir_path} eq '/';

$self->SUPER::assign_runtime_properties($interp, $source);
}
1;
}
BEGIN { $INC{q{HTML/Mason/Component/Subcomponent.pm}} = 1;
package HTML::Mason::Component::Subcomponent;
use strict;
use warnings;
use HTML::Mason::Component;
use vars qw(@ISA);
@ISA = qw(HTML::Mason::Component);
use HTML::Mason::MethodMaker ( read_only => [ qw(comp_id is_method name owner path) ] );
sub assign_subcomponent_properties {
 my $self = shift;
 ($self->{owner}, $self->{name}, $self->{is_method}) = @_;
}
sub assign_runtime_properties {
 my ($self, $interp, $source) = @_;
 $self->SUPER::assign_runtime_properties($interp, $source);
 $self->{comp_id} = sprintf("[%s '%s' of %s]", $self->{is_method} ? 'method' : 'subcomponent',
 $self->name, $self->owner->comp_id);
 $self->{path} = $self->owner->path . ":" . $self->name;
}
sub cache_file { return $_[0]->owner->cache_file }
sub load_time { return $_[0]->owner->load_time }
sub compiler_id { return $_[0]->owner->compilation_params }
sub dir_path { return $_[0]->owner->dir_path }
sub is_subcomp { 1 }
sub object_file { return $_[0]->owner->object_file }
sub parent { return $_[0]->owner->parent }
sub persistent { return $_[0]->owner->persistent }
sub title { return $_[0]->owner->title . ":" . $_[0]->name }
1;
}
BEGIN { $INC{q{HTML/Mason/Lexer.pm}} = 1;
package HTML::Mason::Lexer;
use strict;
use warnings;
use HTML::Mason::Exceptions( abbr => [qw(param_error syntax_error error)] );
use HTML::Mason::Tools qw(taint_is_on);
use Params::Validate qw(:all);
Params::Validate::validation_options( on_fail => sub { param_error join '', @_ } );
use Class::Container;
use base qw(Class::Container);
my %blocks = ( args => 'variable_list_block',
 attr => 'key_val_block',
 flags => 'key_val_block',
 cleanup => 'raw_block',
 doc => 'doc_block',
 filter => 'raw_block',
 init => 'raw_block',
 once => 'raw_block',
 perl => 'raw_block',
 shared => 'raw_block',
 text => 'text_block',
 );
sub block_names
{
 return keys %blocks;
}
sub block_body_method
{
 return $blocks{ $_[1] };
}
{
 my $blocks_re;

my $re = join '|', __PACKAGE__->block_names;
 $blocks_re = qr/$re/i;

sub blocks_regex
 {
 return $blocks_re;
 }
}
sub lex
{
 my $self = shift;
 my %p = validate(@_,
 {comp_source => SCALAR|SCALARREF,
 name => SCALAR,
 compiler => {isa => 'HTML::Mason::Compiler'}}
 );
 $p{comp_source} = ${$p{comp_source}} if ref $p{comp_source};
 local $self->{current} = \%p;
 my $current = $self->{current}; 
 $current->{comp_source} =~ s/\r\n?/\n/g;
 $current->{lines} = 1;
 $current->{in_def} = $current->{in_method} = 0;
 $current->{ending} = qr/\G\z/;
 ($current->{comp_source}) = (delete $current->{comp_source}) =~ /(.*)/s
 if taint_is_on;

eval
 {
 $current->{compiler}->start_component;
 $self->start;
 };
 my $err = $@;
 eval
 {
 $current->{compiler}->end_component;
 };
 $err ||= $@;

rethrow_exception $err;
}
sub start
{
 my $self = shift;

my $end;
 while (1)
 {
 last if $end = $self->match_end;

$self->match_block && next;

$self->match_named_block && next;

$self->match_substitute && next;

$self->match_comp_call && next;

$self->match_perl_line && next;

$self->match_comp_content_call && next;

$self->match_comp_content_call_end && next;

$self->match_text && next;

if ( ( $self->{current}{in_def} || $self->{current}{in_method} ) &&
 $self->{current}{comp_source} =~ /\G\z/ )
 {
 my $type = $self->{current}{in_def} ? 'def' : 'method';
 $self->throw_syntax_error("Missing closing </%$type> tag");
 }

last if $self->{current}{comp_source} =~ /\G\z/;
 $self->throw_syntax_error("Infinite parsing loop encountered - Lexer bug?");
 }

if ( $self->{current}{in_def} || $self->{current}{in_method} )
 {
 my $type = $self->{current}{in_def} ? 'def' : 'method';
 unless ( $end =~ m,</%\Q$type\E>\n?,i )
 {
 my $block_name = $self->{current}{"in_$type"};
 $self->throw_syntax_error("No </%$type> tag for <%$type $block_name> block");
 }
 }
}
sub match_block
{
 my $self = shift;

my $blocks_re = $self->blocks_regex;

if ( $self->{current}{comp_source} =~ /\G<%($blocks_re)>/igcs )
 {
 my $type = lc $1;
 $self->{current}{compiler}->start_block( block_type => $type );

my $method = $self->block_body_method($type);
 $self->$method( {block_type => $type} );

return 1;
 }
}
sub generic_block
{
 my ($self, $method, $p) = @_;

$p->{allow_text} = 1;
 my ($block, $nl) = $self->match_block_end( $p );

$self->{current}{compiler}->$method( block_type => $p->{block_type},
 block => $block );

$self->{current}{lines} += $block =~ tr/\n//;
 $self->{current}{lines}++ if $nl;

$self->{current}{compiler}->end_block( block_type => $p->{block_type} );
}
sub text_block
{
 my $self = shift;
 $self->generic_block('text_block', @_);
}
sub raw_block
{
 my $self = shift;
 $self->generic_block('raw_block', @_);
}
sub doc_block
{
 my $self = shift;
 $self->generic_block('doc_block', @_);
}
sub variable_list_block
{
 my ($self, $p) = @_;

my $ending = qr/ \n | <\/%\Q$p->{block_type}\E> /ix;
 while ( $self->{current}{comp_source} =~ m,
                       \G               # last pos matched
                       (?:
                        [ \t]*
                        ( [\$\@\%] )    # variable type
                        ( [^\W\d]\w* )  # only allows valid Perl variable names
                        [ \t]*
                        # if we have a default arg we'll suck up
                        # any comment it has as part of the default
                        # otherwise explcitly search for a comment
                        (?:
                         (?:              # this entire entire piece is optional
                           =>
                          ( [^\n]+? )     # default value
                         )
                         |
                         (?:              # an optional comment
                          [ \t]*
                          \#
                          [^\n]*
                         )
                        )?
                        (?= $ending )
                        |
                        [ \t]*          # a comment line
                        \#
                        [^\n]*
                        (?= $ending )
                        |
                        [ \t]*          # just space
                       )
                       (\n |          # newline or
                          (?= <\/%\Q$p->{block_type}\E> ) )   # end of block (don't consume it)
                      ,ixgc
 )
 {
 if ( defined $1 && defined $2 && length $1 && length $2 )
 {
 $self->{current}{compiler}->variable_declaration( block_type => $p->{block_type},
 type => $1,
 name => $2,
 default => $3,
 );
 }

$self->{current}{lines}++ if $4;
 }

$p->{allow_text} = 0;
 my $nl = $self->match_block_end( $p );
 $self->{current}{lines}++ if $nl;

$self->{current}{compiler}->end_block( block_type => $p->{block_type} );
}
sub key_val_block
{
 my ($self, $p) = @_;

my $ending = qr, (?: \n |           # newline or
                         (?= </%\Q$p->{block_type}\E> ) )   # end of block (don't consume it)
                   ,ix;

while ( $self->{current}{comp_source} =~ /
                      \G
                      [ \t]*
                      ([\w_]+)          # identifier
                      [ \t]*=>[ \t]*    # separator
                      (\S[^\n]*?)       # value ( must start with a non-space char)
                      $ending
                      |
                      \G\n              # a plain empty line
                      |
                      \G
                      [ \t]*            # an optional comment
                      \#
                      [^\n]*
                      $ending
                      |
                      \G[ \t]+?
                      $ending
                     /xgc )
 {
 if ( defined $1 && defined $2 && length $1 && length $2 )
 {
 $self->{current}{compiler}->key_value_pair( block_type => $p->{block_type},
 key => $1,
 value => $2
 );
 }

$self->{current}{lines}++;
 }

$p->{allow_text} = 0;
 my $nl = $self->match_block_end( $p );
 $self->{current}{lines}++ if $nl;

$self->{current}{compiler}->end_block( block_type => $p->{block_type} );
}
sub match_block_end
{
 my ($self, $p) = @_;

my $re = $p->{allow_text} ? qr,\G(.*?)</%\Q$p->{block_type}\E>(\n?),is
 : qr,\G\s*</%\Q$p->{block_type}\E>(\n?),is;
 if ( $self->{current}{comp_source} =~ /$re/gc )
 {
 return $p->{allow_text} ? ($1, $2) : $1;
 }
 else
 {
 $self->throw_syntax_error("Invalid <%$p->{block_type}> section line");
 }
}
sub match_named_block
{
 my ($self, $p) = @_;

if ( $self->{current}{comp_source} =~ /\G<%(def|method)(?:\s+([^\n]+?))?\s*>/igcs )
 {
 my ($type, $name) = (lc $1, $2);

$self->throw_syntax_error("$type block without a name")
 unless defined $name && length $name;

$self->{current}{compiler}->start_named_block( block_type => $type,
 name => $name );
 local $self->{current}{ending} = qr,\G</%\Q$type\E>\n?,i;

local $self->{current}{"in_$type"} = $name;

$self->start;

$self->{current}{compiler}->end_named_block( block_type => $type );

return 1;
 }
}
my $flag = qr/[[:alpha:]_]\w*/;
sub escape_flag_regex { $flag }
sub match_substitute
{

my $self = shift;

return 0 unless $self->{current}{comp_source} =~ /\G<%/gcs;

if ( $self->{current}{comp_source} =~
 m{
           \G
           (.+?)                # Substitution body ($1)
           (
            \s*
            (?<!\|)             # Not preceded by a '|'
            \|                  # A '|'
            \s*
            (                   # (Start $3)
             $flag              # A flag
             (?:\s*,\s*$flag)*  # More flags, with comma separators
            )
            \s*
           )?
           %>                   # Closing tag
          }xcigs )
 {
 $self->{current}{lines} += tr/\n// foreach grep defined, ($1, $2);

$self->{current}{compiler}->substitution( substitution => $1,
 escape => $3 );
 return 1;
 }
 else
 {
 $self->throw_syntax_error("'<%' without matching '%>'");
 }
}
sub match_comp_call
{
 my $self = shift;

if ( $self->{current}{comp_source} =~ /\G<&(?!\|)/gcs )
 {
 if ( $self->{current}{comp_source} =~ /\G(.*?)&>/gcs )
 {
 my $call = $1;
 $self->{current}{compiler}->component_call( call => $call );
 $self->{current}{lines} += $call =~ tr/\n//;

return 1;
 }
 else
 {
 $self->throw_syntax_error("'<&' without matching '&>'");
 }
 }
}
sub match_comp_content_call
{
 my $self = shift;

if ( $self->{current}{comp_source} =~ /\G<&\|/gcs )
 {
 if ( $self->{current}{comp_source} =~ /\G(.*?)&>/gcs )
 {
 my $call = $1;
 $self->{current}{compiler}->component_content_call( call => $call );
 $self->{current}{lines} += $call =~ tr/\n//;

return 1;
 }
 else
 {
 $self->throw_syntax_error("'<&|' without matching '&>'");
 }
 }
}
sub match_comp_content_call_end
{
 my $self = shift;

if ( $self->{current}{comp_source} =~ m,\G</&(.*?)>,gcs )
 {
 my $call = $1 || '';
 $self->{current}{compiler}->component_content_call_end( call_end => $call );
 $self->{current}{lines} += $call =~ tr/\n//;

return 1;
 }
}
sub match_perl_line
{
 my $self = shift;

if ( $self->{current}{comp_source} =~ /\G(?<=^)%([^\n]*)(?:\n|\z)/gcm )
 {
 $self->{current}{compiler}->perl_line( line => $1 );
 $self->{current}{lines}++;

return 1;
 }
}
sub match_text
{
 my $self = shift;
 my $c = $self->{current};
 if ( $c->{comp_source} =~ m{
                                \G
                                (.*?)         # anything, followed by:
                                (
                                 (?<=\n)(?=%) # an eval line - consume the \n
                                 |
                                 (?=</?[%&])  # a substitution or block or call start or end
                                              # - don't consume
                                 |
                                 \\\n         # an escaped newline  - throw away
                                 |
                                 \z           # end of string
                                )
                               }xcgs )
 {
 $c->{compiler}->text( text => $1 ) if length $1;
 $c->{lines} += tr/\n// foreach grep defined, ($1, $2);

return 1;
 }

return 0;
}
sub match_end
{
 my $self = shift;
 if ( $self->{current}{comp_source} =~ /($self->{current}{ending})/gcs )
 {
 $self->{current}{lines} += $1 =~ tr/\n//;
 return defined $1 && length $1 ? $1 : 1;
 }
 return 0;
}
sub _next_line
{
 my $self = shift;
 my $pos = shift;

$pos = ( defined $pos ?
 $pos :
 ( substr( $self->{current}{comp_source}, pos($self->{current}{comp_source}), 1 ) eq "\n" ?
 pos($self->{current}{comp_source}) + 1 :
 pos($self->{current}{comp_source}) ) );

my $to_eol = ( index( $self->{current}{comp_source}, "\n", $pos ) != -1 ?
 ( index( $self->{current}{comp_source}, "\n" , $pos ) ) - $pos :
 length $self->{current}{comp_source} );
 return substr( $self->{current}{comp_source}, $pos, $to_eol );
}
sub line_number
{
 my $self = shift;

return $self->{current}{lines};
}
sub name
{
 my $self = shift;

return $self->{current}{name};
}
sub throw_syntax_error
{
 my ($self, $error) = @_;

HTML::Mason::Exception::Syntax->throw( error => $error,
 comp_name => $self->name,
 source_line => $self->_next_line,
 line_number => $self->line_number );
}
1;
}
BEGIN { $INC{q{HTML/Mason/Compiler.pm}} = 1;
package HTML::Mason::Compiler;
use strict;
use warnings;
use Data::Dumper;
use HTML::Mason::Component::FileBased;
use HTML::Mason::Component::Subcomponent;
use HTML::Mason::Exceptions( abbr => [qw(param_error compiler_error syntax_error)] );
use HTML::Mason::Lexer;
use HTML::Mason::Tools qw(checksum);
use Params::Validate qw(:all);
Params::Validate::validation_options( on_fail => sub { param_error join '', @_ } );
use Class::Container;
use base qw(Class::Container);
BEGIN
{
 __PACKAGE__->valid_params
 (
 allow_globals =>
 { parse => 'list', type => ARRAYREF, default => [],
 descr => "An array of names of Perl variables that are allowed globally within components" },

default_escape_flags =>
 { parse => 'string', type => SCALAR|ARRAYREF, default => [],
 descr => "Escape flags that will apply by default to all Mason tag output" },

enable_autoflush =>
 { parse => 'boolean', type => SCALAR, default => 1,
 descr => "Whether to include support for autoflush when compiling components" },

lexer =>
 { isa => 'HTML::Mason::Lexer',
 descr => "A Lexer object that will scan component text during compilation" },

preprocess =>
 { parse => 'code', type => CODEREF, optional => 1,
 descr => "A subroutine through which all component text will be sent during compilation" },

postprocess_perl =>
 { parse => 'code', type => CODEREF, optional => 1,
 descr => "A subroutine through which all Perl code will be sent during compilation" },

postprocess_text =>
 { parse => 'code', type => CODEREF, optional => 1,
 descr => "A subroutine through which all plain text will be sent during compilation" },

use_source_line_numbers =>
 { parse => 'boolean', type => SCALAR, default => 1,
 descr => "Whether to use source line numbers in errors and debugger" },
 );

__PACKAGE__->contained_objects
 ( lexer => { class => 'HTML::Mason::Lexer',
 descr => "This class generates compiler events based on the components source" },
 );
 if (defined($DB::sub)) {
 *IN_PERL_DB = sub () { 1 };
 } else {
 *IN_PERL_DB = sub () { 0 };
 }
}
use HTML::Mason::MethodMaker
 ( read_only => [qw(enable_autoflush lexer object_id preprocess postprocess_perl postprocess_text use_source_line_numbers)
 ],
 );
my $old_escape_re = qr/^[hnu]+$/;
sub new
{
 my $class = shift;
 my $self = $class->SUPER::new(@_);

$self->default_escape_flags( $self->{default_escape_flags} )
 if defined $self->{default_escape_flags};
 $self->allow_globals( @{$self->{allow_globals}} );
 $self->compute_object_id;

return $self;
}
sub compute_object_id
{
 my $self = shift;
 my $spec = $self->validation_spec;
 my @id_keys =
 ( grep { ! exists $spec->{$_}{isa} && ! exists $spec->{$_}{can} }
 grep { $_ ne 'container' } keys %$spec );

my @vals = ('HTML::Mason::VERSION', $HTML::Mason::VERSION);
 foreach my $k ( @id_keys ) {
 push @vals, $k, $self->{$k};
 }
 my $dumped_vals = Data::Dumper->new(\@vals)->Indent(0)->Dump;
 $self->{object_id} = checksum($dumped_vals);
}
my %top_level_only_block = map { $_ => 1 } qw(cleanup once shared);
my %valid_comp_flag = map { $_ => 1 } qw(inherit);
sub add_allowed_globals
{
 my $self = shift;
 my @globals = @_;

if ( my @bad = grep { ! /^[\$@%]/ } @globals )
 {
 param_error "add_allowed_globals: bad parameters '@bad', must begin with one of \$, \@, %\n";
 }

$self->{allow_globals} = [ keys %{ { map { $_ => 1 } @globals, @{ $self->{allow_globals} } } } ];
 return @{ $self->{allow_globals} };
}
sub allow_globals
{
 my $self = shift;

if (@_)
 {
 $self->{allow_globals} = [];
 return if @_ == 1 and not defined $_[0]; 
 $self->add_allowed_globals(@_);
 }

return @{ $self->{allow_globals} };
}
sub default_escape_flags
{
 my $self = shift;

return $self->{default_escape_flags} unless @_;

my $flags = shift;

unless ( defined $flags )
 {
 $self->{default_escape_flags} = [];
 return;
 }
 unless ( ref $flags )
 {
 if ( $flags =~ /^[hu]+$/ )
 {
 $self->{default_escape_flags} = [ split //, $flags ];
 }
 else
 {
 $self->{default_escape_flags} = [ $flags ];
 }
 }

return $self->{default_escape_flags};
}
sub compile
{
 my $self = shift;
 my %p = validate( @_, { comp_source => { type => SCALAR|SCALARREF },
 name => { type => SCALAR },
 comp_path => { type => SCALAR },
 fh => { type => HANDLE, optional => 1 },
 } );
 my $src = ref($p{comp_source}) ? $p{comp_source} : \$p{comp_source};
 local $self->{current_compile} = {};
 local $self->{main_compile} = $self->{current_compile};
 local $self->{paused_compiles} = [];

local $self->{comp_path} = $p{comp_path};
 if ($self->preprocess)
 {
 eval { $self->preprocess->( $src ) };
 compiler_error "Error during custom preprocess step: $@" if $@;
 }

$self->lexer->lex( comp_source => $src, name => $p{name}, compiler => $self );

return $self->compiled_component( exists($p{fh}) ? (fh => $p{fh}) : () );
}
sub start_component
{
 my $self = shift;
 my $c = $self->{current_compile};

$c->{in_main} = 1;

$c->{in_block} = undef;

$self->_init_comp_data($c);
}
sub _init_comp_data
{
 my $self = shift;
 my $data = shift;

$data->{body} = '';
 $data->{last_body_code_type} = '';

foreach ( qw(def method) )
 {
 $data->{$_} = {};
 }

$data->{args} = [];
 $data->{flags} = {};
 $data->{attr} = {};

$data->{comp_with_content_stack} = [];

foreach ( qw(cleanup filter init once shared) )
 {
 $data->{blocks}{$_} = [];
 }
}
sub end_component
{
 my $self = shift;
 my $c = $self->{current_compile};

$self->lexer->throw_syntax_error("Not enough component-with-content ending tags found")
 if @{ $c->{comp_with_content_stack} };
}
sub start_block
{
 my $self = shift;
 my $c = $self->{current_compile};
 my %p = @_;

$self->lexer->throw_syntax_error("Cannot define a $p{block_type} section inside a method or subcomponent")
 if $top_level_only_block{ $p{block_type} } && ! $c->{in_main};

$self->lexer->throw_syntax_error("Cannot nest a $p{block_type} inside a $c->{in_block} block")
 if $c->{in_block};

$c->{in_block} = $p{block_type};
}
sub raw_block
{

my $self = shift;
 my $c = $self->{current_compile};
 my %p = @_;

eval { $self->postprocess_perl->( \$p{block} ) if $self->postprocess_perl };
 compiler_error $@ if $@;

my $method = "$p{block_type}_block";
 return $self->$method(%p) if $self->can($method);

my $comment = '';
 if ( $self->lexer->line_number )
 {
 my $line = $self->lexer->line_number;
 my $file = $self->lexer->name;
 $comment = "#line $line $file\n" if $self->use_source_line_numbers;
 }

push @{ $self->{current_compile}{blocks}{ $p{block_type} } }, "$comment$p{block}";
}
sub doc_block
{
}
sub perl_block
{
 my $self = shift;
 my %p = @_;

$self->_add_body_code( $p{block} );

$self->{current_compile}{last_body_code_type} = 'perl_block';
}
sub text
{
 my ($self, %p) = @_;
 my $tref = ref($p{text}) ? $p{text} : \$p{text}; 

eval { $self->postprocess_text->($tref) } if $self->postprocess_text;
 compiler_error $@ if $@;

$$tref =~ s,([\'\\]),\\$1,g;

if ($self->enable_autoflush) {
 $self->_add_body_code("\$m->print( '", $$tref, "' );\n");
 } else {
 $self->_add_body_code("\$\$_outbuf .= '", $$tref, "';\n");
 }

$self->{current_compile}{last_body_code_type} = 'text';
}
sub text_block
{
 my $self = shift;
 my %p = @_;
 $self->text(text => \$p{block});
}
sub end_block
{
 my $self = shift;
 my $c = $self->{current_compile};
 my %p = @_;

$self->lexer->throw_syntax_error("End of $p{block_type} encountered while in $c->{in_block} block")
 unless $c->{in_block} eq $p{block_type};

$c->{in_block} = undef;
}
sub variable_declaration
{
 my $self = shift;
 my %p = @_;

$self->lexer->throw_syntax_error("variable_declaration called inside a $p{block_type} block")
 unless $p{block_type} eq 'args';

my $arg = "$p{type}$p{name}";

$self->lexer->throw_syntax_error("$arg already defined")
 if grep { "$_->{type}$_->{name}" eq $arg } @{ $self->{current_compile}{args} };

push @{ $self->{current_compile}{args} }, { type => $p{type},
 name => $p{name},
 default => $p{default},
 line => $self->lexer->line_number,
 file => $self->lexer->name,
 };
}
sub key_value_pair
{
 my $self = shift;
 my %p = @_;

compiler_error "key_value_pair called inside a $p{block_type} block"
 unless $p{block_type} eq 'flags' || $p{block_type} eq 'attr';

my $type = $p{block_type} eq 'flags' ? 'flag' : 'attribute';
 $self->lexer->throw_syntax_error("$p{key} $type already defined")
 if exists $self->{current_compile}{ $p{block_type} }{ $p{key} };

$self->{current_compile}{ $p{block_type} }{ $p{key} } = $p{value}
}
sub start_named_block
{
 my $self = shift;
 my $c = $self->{current_compile};
 my %p = @_;
 $self->lexer->throw_syntax_error
 ("Cannot define a $p{block_type} block inside a method or subcomponent")
 unless $c->{in_main};
 $self->lexer->throw_syntax_error("Invalid $p{block_type} name: $p{name}")
 if $p{name} =~ /[^.\w-]/;
 $self->lexer->throw_syntax_error
 (sprintf("Duplicate definition of %s '%s'",
 $p{block_type} eq 'def' ? 'subcomponent' : 'method', $p{name}))
 if exists $c->{$p{block_type}}{ $p{name} };
 my $other_type = $p{block_type} eq 'def' ? 'method' : 'def';
 $self->lexer->throw_syntax_error
 ("Cannot define a method and subcomponent with the same name ($p{name})")
 if exists $c->{$other_type}{ $p{name} };

$c->{in_main}--;

$c->{ $p{block_type} }{ $p{name} } = {};
 $self->_init_comp_data( $c->{ $p{block_type} }{ $p{name} } );
 push @{$self->{paused_compiles}}, $c;
 $self->{current_compile} = $c->{ $p{block_type} }{ $p{name} };
 $self->{current_compile}->{in_named_block} = {block_type => $p{block_type}, name => $p{name}};
}
sub end_named_block
{
 my $self = shift;

delete $self->{current_compile}->{in_named_block};
 $self->{current_compile} = pop @{$self->{paused_compiles}};
 $self->{current_compile}{in_main}++;
}
sub substitution
{
 my $self = shift;
 my %p = @_;

my $text = $p{substitution};
 my @lines = split(/\n/, $text);
 unless (grep { /^\s*[^\s\#]/ } @lines) {
 $self->{current_compile}{last_body_code_type} = 'substitution';
 return;
 }

if ( ( exists $p{escape} && defined $p{escape} ) ||
 @{ $self->{default_escape_flags} }
 )
 {
 my @flags;
 if ( defined $p{escape} )
 {
 $p{escape} =~ s/\s+$//;

if ( $p{escape} =~ /$old_escape_re/ )
 {
 @flags = split //, $p{escape};
 }
 else
 {
 @flags = split /\s*,\s*/, $p{escape};
 }
 }

unshift @flags, @{ $self->default_escape_flags }
 unless grep { $_ eq 'n' } @flags;

my %seen;
 my $flags =
 ( join ', ',
 map { $seen{$_}++ ? () : "'$_'" }
 grep { $_ ne 'n' } @flags
 );

$text = "\$m->interp->apply_escapes( (join '', ($text)), $flags )" if $flags;
 }

my $code;
 if ($self->enable_autoflush) {
 $code = "\$m->print( $text );\n";
 } else {
 $code = "for ( $text ) { \$\$_outbuf .= \$_ if defined }\n";
 }

eval { $self->postprocess_perl->(\$code) } if $self->postprocess_perl;
 compiler_error $@ if $@;

$self->_add_body_code($code);

$self->{current_compile}{last_body_code_type} = 'substitution';
}
sub component_call
{
 my $self = shift;
 my %p = @_;

my ($prespace, $call, $postspace) = ($p{call} =~ /(\s*)(.*)(\s*)/s);
 if ( $call =~ m,^[\w/.],)
 {
 my $comma = index($call, ',');
 $comma = length $call if $comma == -1;
 (my $comp = substr($call, 0, $comma)) =~ s/\s+$//;
 $call = "'$comp'" . substr($call, $comma);
 }
 my $code = "\$m->comp( $prespace $call $postspace \n); ";
 eval { $self->postprocess_perl->(\$code) } if $self->postprocess_perl;
 compiler_error $@ if $@;

$self->_add_body_code($code);

$self->{current_compile}{last_body_code_type} = 'component_call';
}
sub component_content_call
{
 my $self = shift;
 my $c = $self->{current_compile};
 my %p = @_;

my $call = $p{call};
 for ($call) { s/^\s+//; s/\s+$//; }
 push @{ $c->{comp_with_content_stack} }, $call;

my $code = "\$m->comp( { content => sub {\n";
 $code .= $self->_set_buffer;

eval { $self->postprocess_perl->(\$code) } if $self->postprocess_perl;
 compiler_error $@ if $@;

$self->_add_body_code($code);

$c->{last_body_code_type} = 'component_content_call';
}
sub component_content_call_end
{
 my $self = shift;
 my $c = $self->{current_compile};
 my %p = @_;

$self->lexer->throw_syntax_error("Found component with content ending tag but no beginning tag")
 unless @{ $c->{comp_with_content_stack} };

my $call = pop @{ $c->{comp_with_content_stack} };
 my $call_end = $p{call_end};
 for ($call_end) { s/^\s+//; s/\s+$//; }

my $comp = undef;
 if ( $call =~ m,^[\w/.],)
 {
 my $comma = index($call, ',');
 $comma = length $call if $comma == -1;
 ($comp = substr($call, 0, $comma)) =~ s/\s+$//;
 $call = "'$comp'" . substr($call, $comma);
 }
 if ($call_end) {
 if ($call_end !~ m,^[\w/.],) {
 $self->lexer->throw_syntax_error("Cannot use an expression inside component with content ending tag; use a bare component name or </&> instead");
 }
 if (!defined($comp)) {
 $self->lexer->throw_syntax_error("Cannot match an expression as a component name; use </&> instead");
 }
 if ($call_end ne $comp) {
 $self->lexer->throw_syntax_error("Component name in ending tag ($call_end) does not match component name in beginning tag ($comp)");
 }
 }

my $code = "} }, $call\n );\n";

eval { $self->postprocess_perl->(\$code) } if $self->postprocess_perl;
 compiler_error $@ if $@;

$self->_add_body_code($code);

$c->{last_body_code_type} = 'component_content_call_end';
}
sub perl_line
{
 my $self = shift;
 my %p = @_;

my $code = "$p{line}\n";

eval { $self->postprocess_perl->(\$code) } if $self->postprocess_perl;
 compiler_error $@ if $@;

$self->_add_body_code($code);

$self->{current_compile}{last_body_code_type} = 'perl_line';
}
sub _add_body_code
{
 my $self = shift;
 if ( $self->lexer->line_number &&
 $self->{current_compile}{last_body_code_type} ne 'perl_line' )
 {
 my $line = $self->lexer->line_number;
 my $file = $self->lexer->name;
 $self->{current_compile}{body} .= "#line $line $file\n" if $self->use_source_line_numbers;
 }

$self->{current_compile}{body} .= $_ foreach @_;
}
sub dump
{
 my $self = shift;
 my $c = $self->{current_compile};

warn "Main component\n";

$self->_dump_data( $c );

foreach ( keys %{ $c->{def} } )
 {
 warn "  Subcomponent $_\n";
 $self->_dump_data( $c->{def}{$_}, '  ' );
 }

foreach ( keys %{ $c->{method} } )
 {
 warn "  Methods $_\n";
 $self->_dump_data( $c->{method}{$_}, '  ');
 }
}
sub _dump_data
{
 my $self = shift;
 my $data = shift;
 my $indent = shift || '';

if ( @{ $data->{args} } )
 {
 warn "$indent  args\n";
 foreach ( @{ $data->{args} } )
 {
 warn "$indent    $_->{type}$_->{name}";
 warn " => $_->{default}" if defined $_->{default};
 warn "\n";
 }
 }

warn "\n$indent  body\n";
 warn $data->{body}, "\n";
}
sub _blocks
{
 my $self = shift;

return @{ $self->{current_compile}{blocks}{ shift() } };
}
sub HTML::Mason::Parser::new
{
 die "The Parser module is no longer a part of HTML::Mason.  Please see ".
 "the Lexer and Compiler modules, its replacements.\n";
}
1;
}
BEGIN { $INC{q{HTML/Mason/Compiler/ToObject.pm}} = 1;
package HTML::Mason::Compiler::ToObject;
use strict;
use warnings;
use Params::Validate qw(BOOLEAN SCALAR validate);
use HTML::Mason::Tools qw(taint_is_on);
use HTML::Mason::Compiler;
use base qw(HTML::Mason::Compiler);
use HTML::Mason::Exceptions( abbr => [qw(wrong_compiler_error system_error)] );
use File::Path qw(mkpath rmtree);
use File::Basename qw(dirname);
BEGIN
{
 __PACKAGE__->valid_params
 (
 comp_class =>
 { parse => 'string', type => SCALAR, default => 'HTML::Mason::Component',
 descr => "The class into which component objects will be blessed" },

subcomp_class =>
 { parse => 'string', type => SCALAR, default => 'HTML::Mason::Component::Subcomponent',
 descr => "The class into which subcomponent objects will be blessed" },

in_package =>
 { parse => 'string', type => SCALAR, default => 'HTML::Mason::Commands',
 descr => "The package in which component execution will take place" },

preamble =>
 { parse => 'string', type => SCALAR, default => '',
 descr => "A chunk of Perl code to add to the beginning of each compiled component" },

postamble =>
 { parse => 'string', type => SCALAR, default => '',
 descr => "A chunk of Perl code to add to the end of each compiled component" },

use_strict =>
 { parse => 'boolean', type => SCALAR, default => 1,
 descr => "Whether to turn on Perl's 'strict' pragma in components" },

define_args_hash =>
 { parse => 'string', type => SCALAR, default => 'auto',
 regex => qr/^(?:always|auto|never)$/,
 descr => "Whether or not to create the %ARGS hash" },

named_component_subs =>
 { parse => 'boolean', type => BOOLEAN, default => 0,
 descr => "Whether to use named subroutines for component code" },
 );
}
use HTML::Mason::MethodMaker
 ( read_only => [
 qw(comp_class define_args_hash in_package named_component_subs postamble preamble subcomp_class use_strict)
 ],
 );
sub compile
{
 my $self = shift;
 my %p = @_;

local $self->{comp_class} = delete $p{comp_class} if exists $p{comp_class};
 return $self->SUPER::compile( %p );
}
sub compile_to_file
{
 my $self = shift;

my %p = validate( @_, { file => { type => SCALAR },
 source => { isa => 'HTML::Mason::ComponentSource' } },
 );

my ($file, $source) = @p{qw(file source)};
 my @newfiles = ($file);

if (defined $file && !-f $file) {
 my ($dirname) = dirname($file);
 if (!-d $dirname) {
 unlink($dirname) if (-e _);
 push @newfiles, mkpath($dirname, 0, 0775);
 system_error "Couldn't create directory $dirname: $!"
 unless -d $dirname;
 }
 rmtree($file) if (-d $file);
 }

($file) = $file =~ /^(.*)/s if taint_is_on; 

open my $fh, "> $file"
 or system_error "Couldn't create object file $file: $!";

$self->compile( comp_source => $source->comp_source_ref,
 name => $source->friendly_name,
 comp_class => $source->comp_class,
 comp_path => $source->comp_path,
 fh => $fh );

close $fh
 or system_error "Couldn't close object file $file: $!";

return \@newfiles;
}
sub _output_chunk
{
 my ($self, $fh, $string) = (shift, shift, shift);
 if ($fh)
 {
 print $fh (ref $_ ? $$_ : $_) foreach grep defined, @_;
 }
 else
 {
 $$string .= (ref $_ ? $$_ : $_) foreach @_;
 }
}
sub compiled_component
{
 my ($self, %p) = @_;
 my $c = $self->{current_compile};
 my $obj_text = '';

local $c->{compiled_def} = $self->_compile_subcomponents if %{ $c->{def} };
 local $c->{compiled_method} = $self->_compile_methods if %{ $c->{method} };
 my $header = $self->_make_main_header;
 $self->_output_chunk($p{fh}, \$obj_text, $header);

my $params = $self->_component_params;

$params->{load_time} = time;

$params->{subcomps} = '\%_def' if %{ $c->{def} };
 $params->{methods} = '\%_method' if %{ $c->{method} };

if ( $self->_blocks('shared') )
 {
 my %subs;
 while ( my ($name, $pref) = each %{ $c->{compiled_def} } )
 {
 my $key = "subcomponent_$name";
 $subs{$key} = $pref->{code};
 $pref->{code} = "sub {\nHTML::Mason::Request->instance->call_dynamic('$key',\@_)\n}";
 }
 while (my ($name, $pref) = each %{ $c->{compiled_method} } )
 {
 my $key = "method_$name";
 $subs{$key} = $pref->{code};
 $pref->{code} = "sub {\nHTML::Mason::Request->instance->call_dynamic( '$key', \@_ )\n}";
 }
 $subs{main} = $params->{code};
 $params->{code} = "sub {\nHTML::Mason::Request->instance->call_dynamic( 'main', \@_ )\n}";

my $named_subs = '';
 my %named_subs = $self->_named_subs_hash;
 while ( my ( $name, $body ) = each %named_subs )
 {
 $named_subs .= '*' . $name . " = sub {\n" . $body . "\n};\n\n";
 }

$params->{dynamic_subs_init} =
 join '', ( "sub {\n",
 $self->_set_request,
 $self->_blocks('shared'),
 $named_subs,
 "return {\n",
 map( "'$_' => $subs{$_},\n", sort keys %subs ),
 "\n}\n}"
 );
 }
 else
 {
 my %named_subs = $self->_named_subs_hash;
 while ( my ( $name, $body ) = each %named_subs )
 {
 $self->_output_chunk( $p{fh}, \$obj_text,
 "sub $name {\n" . $body . "\n}\n"
 );
 }
 }

$self->_output_chunk($p{fh}, \$obj_text, $self->_subcomponents_footer);
 $self->_output_chunk($p{fh}, \$obj_text, $self->_methods_footer);

$self->_output_chunk($p{fh}, \$obj_text,
 $self->_constructor( $self->comp_class,
 $params ),
 ';',
 );

return \$obj_text;
}
sub _named_subs_hash
{
 my $self = shift;

return unless $self->named_component_subs;

my %subs;
 $subs{ $self->_sub_name } = $self->_body;

while ( my ( $name, $params ) =
 each %{ $self->{current_compile}{compiled_def} } )
 {
 $subs{ $self->_sub_name( 'def', $name ) } = $params->{body};
 }

while ( my ( $name, $params ) =
 each %{ $self->{current_compile}{compiled_method} } )
 {
 $subs{ $self->_sub_name( 'method', $name ) } = $params->{body};
 }

return %subs;
}
sub _sub_name
{
 my $self = shift;

return join '_', $self->_escape_sub_name_part( $self->{comp_path}, @_ );
}
sub _escape_sub_name_part
{
 my $self = shift;

return map { my $part = $_;
 $part =~ s/([^\w_])/'_' . sprintf( '%x', ord $1 )/ge;
 $part; } @_;
}
sub _compile_subcomponents
{
 my $self = shift;

return $self->_compile_subcomponents_or_methods('def');
}
sub _compile_methods
{
 my $self = shift;

return $self->_compile_subcomponents_or_methods('method');
}
sub _compile_subcomponents_or_methods
{
 my $self = shift;
 my $type = shift;

my %compiled;
 foreach ( keys %{ $self->{current_compile}{$type} } )
 {
 local $self->{current_compile} = $self->{current_compile}{$type}{$_};
 local $self->{current_compile}->{in_named_block} = {type => $type, name => $_};
 $compiled{$_} = $self->_component_params;
 }

return \%compiled;
}
sub _make_main_header
{
 my $self = shift;

my $pkg = $self->in_package;

return join '', ( "package $pkg;\n",
 $self->use_strict ? "use strict;\n" : "no strict;\n",
 sprintf( "use vars qw(\%s);\n",
 join ' ', '$m', $self->allow_globals ),
 $self->_blocks('once'),
 );
}
sub _subcomponents_footer
{
 my $self = shift;

return $self->_subcomponent_or_method_footer('def');
}
sub _methods_footer
{
 my $self = shift;

return $self->_subcomponent_or_method_footer('method');
}
sub _subcomponent_or_method_footer
{
 my $self = shift;
 my $c = $self->{current_compile};
 my $type = shift;

return '' unless %{ $c->{$type} };

return join('',
 "my %_$type =\n(\n",
 map( {("'$_' => " ,
 $self->_constructor( $self->{subcomp_class},
 $c->{"compiled_$type"}{$_} ) ,
 ",\n")} keys %{ $c->{"compiled_$type"} } ) ,
 "\n);\n"
 );
}
sub _constructor
{
 my ($self, $class, $params) = @_;

return ("${class}->new(\n",
 map( {("'$_' => ", $params->{$_}, ",\n")}
 sort grep { $_ ne 'body' } keys %$params ),
 "\n)\n",
 );
}
sub _component_params
{
 my $self = shift;

my %params;

if ( $self->named_component_subs )
 {
 $params{code} =
 '\\&' .
 $self->_sub_name
 ( grep { defined }
 @{ $self->{current_compile}{in_named_block} }
 { 'type', 'name' } );
 $params{body} = $self->_body;
 }
 else
 {
 $params{code} = join '', "sub {\n", $self->_body, "}";
 }

$params{flags} = join '', "{\n", $self->_flags, "\n}"
 if keys %{ $self->{current_compile}{flags} };

$params{attr} = join '', "{\n", $self->_attr, "\n}"
 if keys %{ $self->{current_compile}{attr} };

$params{declared_args} = join '', "{\n", $self->_declared_args, "\n}"
 if @{ $self->{current_compile}{args} };

$params{has_filter} = 1 if $self->_blocks('filter');

return \%params;
}
sub _body
{
 my $self = shift;

return join '', ( $self->preamble,
 $self->_set_request,
 $self->_set_buffer,
 $self->_arg_declarations,
 $self->_filter,
 "\$m->debug_hook( \$m->current_comp->path ) if ( HTML::Mason::Compiler::IN_PERL_DB() );\n\n",
 $self->_blocks('init'),
 $self->{current_compile}{body},

$self->_blocks('cleanup'),
 $self->postamble,
 ";return;\n",
 );
}
sub _set_request
{
 my $self = shift;

return if $self->in_package eq 'HTML::Mason::Commands';

return 'local $' . $self->in_package . '::m = $HTML::Mason::Commands::m;' . "\n";
}
sub _set_buffer
{
 my $self = shift;

if ($self->enable_autoflush) {
 return '';
 } else {
 return 'my $_outbuf = $m->{top_stack}->[HTML::Mason::Request::STACK_BUFFER];' . "\n";
 }
}
my %coercion_funcs = ( '@' => 'HTML::Mason::Tools::coerce_to_array',
 '%' => 'HTML::Mason::Tools::coerce_to_hash',
 );
sub _arg_declarations
{
 my $self = shift;

my $init;
 my @args_hash;
 my $pos;
 my @req_check;
 my @decl;
 my @assign;

my $define_args_hash = $self->_define_args_hash;

unless ( @{ $self->{current_compile}{args} } )
 {
 return unless $define_args_hash;

return ( "my \%ARGS;\n",
 "{ local \$^W; \%ARGS = \@_ unless (\@_ % 2); }\n"
 );
 }

$init = <<'EOF';
HTML::Mason::Exception::Params->throw
    ( error =>
      "Odd number of parameters passed to component expecting name/value pairs"
    ) if @_ % 2;
EOF

if ( $define_args_hash )
 {
 @args_hash = "my \%ARGS = \@_;\n";
 }
 $pos = <<'EOF';
{
    my %pos;
    for ( my $x = 0; $x < @_; $x += 2 )
    {
        $pos{ $_[$x] } = $x + 1;
    }
EOF

my @required =
 ( map { $_->{name} }
 grep { ! defined $_->{default} }
 @{ $self->{current_compile}{args} }
 );

if (@required)
 {
 local $" = ' ';
 @req_check = <<"EOF";

    foreach my \$arg ( qw( @required ) )
    {
        HTML::Mason::Exception::Params->throw
            ( error => "no value sent for required parameter '\$arg'" )
                unless exists \$pos{\$arg};
    }
EOF
 }

foreach ( @{ $self->{current_compile}{args} } )
 {
 my $var_name = "$_->{type}$_->{name}";
 push @decl, $var_name;

my $arg_in_array = "\$_[ \$pos{'$_->{name}'} ]";

my $coerce;
 if ( $coercion_funcs{ $_->{type} } )
 {
 $coerce = $coercion_funcs{ $_->{type} } . "( $arg_in_array, '$var_name')";
 }
 else
 {
 $coerce = $arg_in_array;
 }

push @assign, "#line $_->{line} $_->{file}\n"
 if defined $_->{line} && defined $_->{file} && $self->use_source_line_numbers;

if ( defined $_->{default} )
 {
 my $default_val = $_->{default};
 $default_val .= "\n" if defined $_->{default} && $_->{default} =~ /\#/;

push @assign, <<"EOF";
     $var_name = exists \$pos{'$_->{name}'} ? $coerce : $default_val;
EOF
 }
 else
 {
 push @assign,
 "    $var_name = $coerce;\n";
 }
 }

my $decl = 'my ( ';
 $decl .= join ', ', @decl;
 $decl .= " );\n";
 return $init, @args_hash, $decl, $pos, @req_check, @assign, "}\n";
}
sub _define_args_hash
{
 my $self = shift;

return 1 if $self->define_args_hash eq 'always';
 return 0 if $self->define_args_hash eq 'never';

foreach ( $self->preamble,
 $self->_blocks('filter'),
 $self->_blocks('init'),
 $self->{current_compile}{body},
 $self->_blocks('cleanup'),
 $self->postamble,
 grep { defined } map { $_->{default} } @{ $self->{current_compile}{args} }
 )
 {
 return 1 if /ARGS/;
 }
}
sub _filter
{
 my $self = shift;

my @filter;
 @filter = $self->_blocks('filter')
 or return;

return ( join '',
 "\$m->current_comp->filter( sub { local \$_ = shift;\n",
 ( join ";\n", @filter ),
 ";\n",
 "return \$_;\n",
 "} );\n",
 );
}
sub _flags
{
 my $self = shift;

return $self->_flags_or_attr('flags');
}
sub _attr
{
 my $self = shift;

return $self->_flags_or_attr('attr');
}
sub _flags_or_attr
{
 my $self = shift;
 my $type = shift;

return join "\n,", ( map { "$_ => $self->{current_compile}{$type}{$_}" }
 keys %{ $self->{current_compile}{$type} } );
}
sub _declared_args
{
 my $self = shift;

my @args;

foreach my $arg ( sort {"$a->{type}$a->{name}" cmp "$b->{type}$b->{name}" }
 @{ $self->{current_compile}{args} } )
 {
 my $def = defined $arg->{default} ? "$arg->{default}" : 'undef';
 $def =~ s,([\\']),\\$1,g;
 $def = "'$def'" unless $def eq 'undef';

push @args, "  '$arg->{type}$arg->{name}' => { default => $def }";
 }

return join ",\n", @args;
}
1;
}
BEGIN { $INC{q{HTML/Mason/Plugin.pm}} = 1;
package HTML::Mason::Plugin;
use strict;
use warnings;
sub new {
 my $class = shift;
 bless { @_ }, $class;
}
sub start_request_hook {
}
sub end_request_hook {
}
sub start_component_hook {
}
sub end_component_hook {
}
1;
}
BEGIN { $INC{q{URI/Escape.pm}} = 1;
package URI::Escape;
use strict;
use vars qw(@ISA @EXPORT @EXPORT_OK $VERSION);
use vars qw(%escapes);
require Exporter;
@ISA = qw(Exporter);
@EXPORT = qw(uri_escape uri_unescape uri_escape_utf8);
@EXPORT_OK = qw(%escapes);
$VERSION = "3.29";
use Carp ();
for (0..255) {
 $escapes{chr($_)} = sprintf("%%%02X", $_);
}
my %subst; 
sub uri_escape
{
 my($text, $patn) = @_;
 return undef unless defined $text;
 if (defined $patn){
 unless (exists $subst{$patn}) {
 (my $tmp = $patn) =~ s,/,\\/,g;
 eval "\$subst{\$patn} = sub {\$_[0] =~ s/([$tmp])/\$escapes{\$1} || _fail_hi(\$1)/ge; }";
 Carp::croak("uri_escape: $@") if $@;
 }
 &{$subst{$patn}}($text);
 } else {
 $text =~ s/([^A-Za-z0-9\-_.!~*'()])/$escapes{$1} || _fail_hi($1)/ge;
 }
 $text;
}
sub _fail_hi {
 my $chr = shift;
 Carp::croak(sprintf "Can't escape \\x{%04X}, try uri_escape_utf8() instead", ord($chr));
}
sub uri_escape_utf8
{
 my $text = shift;
 if ($] < 5.008) {
 $text =~ s/([^\0-\x7F])/do {my $o = ord($1); sprintf("%c%c", 0xc0 | ($o >> 6), 0x80 | ($o & 0x3f)) }/ge;
 }
 else {
 utf8::encode($text);
 }

return uri_escape($text, @_);
}
sub uri_unescape
{
 my $str = shift;
 if (@_ && wantarray) {
 my @str = ($str, @_); 
 foreach (@str) {
 s/%([0-9A-Fa-f]{2})/chr(hex($1))/eg;
 }
 return @str;
 }
 $str =~ s/%([0-9A-Fa-f]{2})/chr(hex($1))/eg if defined $str;
 $str;
}
sub escape_char {
 return join '', @URI::Escape::escapes{$_[0] =~ /(\C)/g};
}
1;
}
BEGIN { $INC{q{URI.pm}} = 1;
package URI;
use strict;
use vars qw($VERSION);
$VERSION = "1.51";
use vars qw($ABS_REMOTE_LEADING_DOTS $ABS_ALLOW_RELATIVE_SCHEME $DEFAULT_QUERY_FORM_DELIMITER);
my %implements; 
use vars qw($reserved $mark $unreserved $uric $scheme_re);
$reserved = q(;/?:@&=+$,[]);
$mark = q(-_.!~*'()); 
$unreserved = "A-Za-z0-9\Q$mark\E";
$uric = quotemeta($reserved) . $unreserved . "%";
$scheme_re = '[a-zA-Z][a-zA-Z0-9.+\-]*';
use Carp ();
use URI::Escape ();
use overload ('""' => sub { ${$_[0]} },
 '==' => sub { _obj_eq(@_) },
 '!=' => sub { !_obj_eq(@_) },
 fallback => 1,
 );
sub _obj_eq {
 return overload::StrVal($_[0]) eq overload::StrVal($_[1]);
}
sub new
{
 my($class, $uri, $scheme) = @_;

$uri = defined ($uri) ? "$uri" : ""; 
 $uri =~ s/^<(?:URL:)?(.*)>$/$1/; 
 $uri =~ s/^"(.*)"$/$1/;
 $uri =~ s/^\s+//;
 $uri =~ s/\s+$//;

my $impclass;
 if ($uri =~ m/^($scheme_re):/so) {
 $scheme = $1;
 }
 else {
 if (($impclass = ref($scheme))) {
 $scheme = $scheme->scheme;
 }
 elsif ($scheme && $scheme =~ m/^($scheme_re)(?::|$)/o) {
 $scheme = $1;
 }
 }
 $impclass ||= implementor($scheme) ||
 do {
 require URI::_foreign;
 $impclass = 'URI::_foreign';
 };

return $impclass->_init($uri, $scheme);
}
sub new_abs
{
 my($class, $uri, $base) = @_;
 $uri = $class->new($uri, $base);
 $uri->abs($base);
}
sub _init
{
 my $class = shift;
 my($str, $scheme) = @_;
 $str = $class->_uric_escape($str);
 $str = "$scheme:$str" unless $str =~ /^$scheme_re:/o ||
 $class->_no_scheme_ok;
 my $self = bless \$str, $class;
 $self;
}
sub _uric_escape
{
 my($class, $str) = @_;
 $str =~ s*([^$uric\#])* URI::Escape::escape_char($1) *ego;
 return $str;
}
sub implementor
{
 my($scheme, $impclass) = @_;
 if (!$scheme || $scheme !~ /\A$scheme_re\z/o) {
 require URI::_generic;
 return "URI::_generic";
 }

$scheme = lc($scheme);

if ($impclass) {
 my $old = $implements{$scheme};
 $impclass->_init_implementor($scheme);
 $implements{$scheme} = $impclass;
 return $old;
 }

my $ic = $implements{$scheme};
 return $ic if $ic;
 $ic = "URI::$scheme"; 
 $ic =~ s/\+/_P/g;
 $ic =~ s/\./_O/g;
 $ic =~ s/\-/_/g;

no strict 'refs';
 unless (@{"${ic}::ISA"}) {
 eval "require $ic";
 die $@ if $@ && $@ !~ /Can\'t locate.*in \@INC/;
 return unless @{"${ic}::ISA"};
 }

$ic->_init_implementor($scheme);
 $implements{$scheme} = $ic;
 $ic;
}
sub _init_implementor
{
 my($class, $scheme) = @_;
}
sub clone
{
 my $self = shift;
 my $other = $$self;
 bless \$other, ref $self;
}
sub _no_scheme_ok { 0 }
sub _scheme
{
 my $self = shift;

unless (@_) {
 return unless $$self =~ /^($scheme_re):/o;
 return $1;
 }

my $old;
 my $new = shift;
 if (defined($new) && length($new)) {
 Carp::croak("Bad scheme '$new'") unless $new =~ /^$scheme_re$/o;
 $old = $1 if $$self =~ s/^($scheme_re)://o;
 my $newself = URI->new("$new:$$self");
 $$self = $$newself;
 bless $self, ref($newself);
 }
 else {
 if ($self->_no_scheme_ok) {
 $old = $1 if $$self =~ s/^($scheme_re)://o;
 Carp::carp("Oops, opaque part now look like scheme")
 if $^W && $$self =~ m/^$scheme_re:/o
 }
 else {
 $old = $1 if $$self =~ m/^($scheme_re):/o;
 }
 }

return $old;
}
sub scheme
{
 my $scheme = shift->_scheme(@_);
 return unless defined $scheme;
 lc($scheme);
}
sub opaque
{
 my $self = shift;

unless (@_) {
 $$self =~ /^(?:$scheme_re:)?([^\#]*)/o or die;
 return $1;
 }

$$self =~ /^($scheme_re:)?    # optional scheme
	        ([^\#]*)          # opaque
                (\#.*)?           # optional fragment
              $/sx or die;

my $old_scheme = $1;
 my $old_opaque = $2;
 my $old_frag = $3;

my $new_opaque = shift;
 $new_opaque = "" unless defined $new_opaque;
 $new_opaque =~ s/([^$uric])/ URI::Escape::escape_char($1)/ego;

$$self = defined($old_scheme) ? $old_scheme : "";
 $$self .= $new_opaque;
 $$self .= $old_frag if defined $old_frag;

$old_opaque;
}
*path = \&opaque; 
sub fragment
{
 my $self = shift;
 unless (@_) {
 return unless $$self =~ /\#(.*)/s;
 return $1;
 }

my $old;
 $old = $1 if $$self =~ s/\#(.*)//s;

my $new_frag = shift;
 if (defined $new_frag) {
 $new_frag =~ s/([^$uric])/ URI::Escape::escape_char($1) /ego;
 $$self .= "#$new_frag";
 }
 $old;
}
sub as_string
{
 my $self = shift;
 $$self;
}
sub as_iri
{
 my $self = shift;
 my $str = $$self;
 if ($str =~ /\bxn--/ && $self->can("ihost")) {
 my $ihost = $self->ihost;
 if ($ihost) {
 my $u = $self->clone;
 $u->host("%%host%%");
 $str = $u->as_string;
 $str =~ s/%%host%%/$ihost/;
 }
 }
 if ($str =~ s/%([89A-F][0-9A-F])/chr(hex($1))/eg) {

require Encode;
 my $enc = Encode::find_encoding("UTF-8");
 my $u = "";
 while (length $str) {
 $u .= $enc->decode($str, Encode::FB_QUIET());
 if (length $str) {
 $u .= URI::Escape::escape_char(substr($str, 0, 1, ""));
 }
 }
 $str = $u;
 }
 return $str;
}
sub canonical
{

my $self = shift;
 my $scheme = $self->_scheme || "";
 my $uc_scheme = $scheme =~ /[A-Z]/;
 my $esc = $$self =~ /%[a-fA-F0-9]{2}/;
 return $self unless $uc_scheme || $esc;

my $other = $self->clone;
 if ($uc_scheme) {
 $other->_scheme(lc $scheme);
 }
 if ($esc) {
 $$other =~ s{%([0-9a-fA-F]{2})}
	            { my $a = chr(hex($1));
                      $a =~ /^[$unreserved]\z/o ? $a : "%\U$1"
                    }ge;
 }
 return $other;
}
sub eq {
 my($self, $other) = @_;
 $self = URI->new($self, $other) unless ref $self;
 $other = URI->new($other, $self) unless ref $other;
 ref($self) eq ref($other) && 
 $self->canonical->as_string eq $other->canonical->as_string;
}
sub abs { $_[0]; }
sub rel { $_[0]; }
sub STORABLE_freeze {
 my($self, $cloning) = @_;
 return $$self;
}
sub STORABLE_thaw {
 my($self, $cloning, $str) = @_;
 $$self = $str;
}
1;
}
BEGIN { $INC{q{URI/_query.pm}} = 1;
package URI::_query;
use strict;
use URI ();
use URI::Escape qw(uri_unescape);
sub query
{
 my $self = shift;
 $$self =~ m,^([^?\#]*)(?:\?([^\#]*))?(.*)$,s or die;

if (@_) {
 my $q = shift;
 $$self = $1;
 if (defined $q) {
 $q =~ s/([^$URI::uric])/ URI::Escape::escape_char($1)/ego;
 $$self .= "?$q";
 }
 $$self .= $3;
 }
 $2;
}
sub query_form {
 my $self = shift;
 my $old = $self->query;
 if (@_) {
 my $delim;
 my $r = $_[0];
 if (ref($r) eq "ARRAY") {
 $delim = $_[1];
 @_ = @$r;
 }
 elsif (ref($r) eq "HASH") {
 $delim = $_[1];
 @_ = %$r;
 }
 $delim = pop if @_ % 2;

my @query;
 while (my($key,$vals) = splice(@_, 0, 2)) {
 $key = '' unless defined $key;
 $key =~ s/([;\/?:@&=+,\$\[\]%])/ URI::Escape::escape_char($1)/eg;
 $key =~ s/ /+/g;
 $vals = [ref($vals) eq "ARRAY" ? @$vals : $vals];
 for my $val (@$vals) {
 $val = '' unless defined $val;
 $val =~ s/([;\/?:@&=+,\$\[\]%])/ URI::Escape::escape_char($1)/eg;
 $val =~ s/ /+/g;
 push(@query, "$key=$val");
 }
 }
 if (@query) {
 unless ($delim) {
 $delim = $1 if $old && $old =~ /([&;])/;
 $delim ||= $URI::DEFAULT_QUERY_FORM_DELIMITER || "&";
 }
 $self->query(join($delim, @query));
 }
 else {
 $self->query(undef);
 }
 }
 return if !defined($old) || !length($old) || !defined(wantarray);
 return unless $old =~ /=/; 
 map { s/\+/ /g; uri_unescape($_) }
 map { /=/ ? split(/=/, $_, 2) : ($_ => '')} split(/[&;]/, $old);
}
sub query_keywords
{
 my $self = shift;
 my $old = $self->query;
 if (@_) {
 my @copy = @_;
 @copy = @{$copy[0]} if @copy == 1 && ref($copy[0]) eq "ARRAY";
 for (@copy) { s/([;\/?:@&=+,\$\[\]%])/ URI::Escape::escape_char($1)/eg; }
 $self->query(@copy ? join('+', @copy) : undef);
 }
 return if !defined($old) || !defined(wantarray);
 return if $old =~ /=/; 
 map { uri_unescape($_) } split(/\+/, $old, -1);
}
*equery = \&query;
1;
}
BEGIN { $INC{q{URI/_generic.pm}} = 1;
package URI::_generic;
require URI;
require URI::_query;
@ISA=qw(URI URI::_query);
use strict;
use URI::Escape qw(uri_unescape);
use Carp ();
my $ACHAR = $URI::uric; $ACHAR =~ s,\\[/?],,g;
my $PCHAR = $URI::uric; $PCHAR =~ s,\\[?],,g;
sub _no_scheme_ok { 1 }
sub authority
{
 my $self = shift;
 $$self =~ m,^((?:$URI::scheme_re:)?)(?://([^/?\#]*))?(.*)$,os or die;

if (@_) {
 my $auth = shift;
 $$self = $1;
 my $rest = $3;
 if (defined $auth) {
 $auth =~ s/([^$ACHAR])/ URI::Escape::escape_char($1)/ego;
 $$self .= "//$auth";
 }
 _check_path($rest, $$self);
 $$self .= $rest;
 }
 $2;
}
sub path
{
 my $self = shift;
 $$self =~ m,^((?:[^:/?\#]+:)?(?://[^/?\#]*)?)([^?\#]*)(.*)$,s or die;

if (@_) {
 $$self = $1;
 my $rest = $3;
 my $new_path = shift;
 $new_path = "" unless defined $new_path;
 $new_path =~ s/([^$PCHAR])/ URI::Escape::escape_char($1)/ego;
 _check_path($new_path, $$self);
 $$self .= $new_path . $rest;
 }
 $2;
}
sub path_query
{
 my $self = shift;
 $$self =~ m,^((?:[^:/?\#]+:)?(?://[^/?\#]*)?)([^\#]*)(.*)$,s or die;

if (@_) {
 $$self = $1;
 my $rest = $3;
 my $new_path = shift;
 $new_path = "" unless defined $new_path;
 $new_path =~ s/([^$URI::uric])/ URI::Escape::escape_char($1)/ego;
 _check_path($new_path, $$self);
 $$self .= $new_path . $rest;
 }
 $2;
}
sub _check_path
{
 my($path, $pre) = @_;
 my $prefix;
 if ($pre =~ m,/,) { 
 $prefix = "/" if length($path) && $path !~ m,^[/?\#],;
 }
 else {
 if ($path =~ m,^//,) {
 Carp::carp("Path starting with double slash is confusing")
 if $^W;
 }
 elsif (!length($pre) && $path =~ m,^[^:/?\#]+:,) {
 Carp::carp("Path might look like scheme, './' prepended")
 if $^W;
 $prefix = "./";
 }
 }
 substr($_[0], 0, 0) = $prefix if defined $prefix;
}
sub path_segments
{
 my $self = shift;
 my $path = $self->path;
 if (@_) {
 my @arg = @_; 
 for (@arg) {
 if (ref($_)) {
 my @seg = @$_;
 $seg[0] =~ s/%/%25/g;
 for (@seg) { s/;/%3B/g; }
 $_ = join(";", @seg);
 }
 else {
 s/%/%25/g; s/;/%3B/g;
 }
 s,/,%2F,g;
 }
 $self->path(join("/", @arg));
 }
 return $path unless wantarray;
 map {/;/ ? $self->_split_segment($_)
 : uri_unescape($_) }
 split('/', $path, -1);
}
sub _split_segment
{
 my $self = shift;
 require URI::_segment;
 URI::_segment->new(@_);
}
sub abs
{
 my $self = shift;
 my $base = shift || Carp::croak("Missing base argument");

if (my $scheme = $self->scheme) {
 return $self unless $URI::ABS_ALLOW_RELATIVE_SCHEME;
 $base = URI->new($base) unless ref $base;
 return $self unless $scheme eq $base->scheme;
 }

$base = URI->new($base) unless ref $base;
 my $abs = $self->clone;
 $abs->scheme($base->scheme);
 return $abs if $$self =~ m,^(?:$URI::scheme_re:)?//,o;
 $abs->authority($base->authority);

my $path = $self->path;
 return $abs if $path =~ m,^/,;

if (!length($path)) {
 my $abs = $base->clone;
 my $query = $self->query;
 $abs->query($query) if defined $query;
 $abs->fragment($self->fragment);
 return $abs;
 }

my $p = $base->path;
 $p =~ s,[^/]+$,,;
 $p .= $path;
 my @p = split('/', $p, -1);
 shift(@p) if @p && !length($p[0]);
 my $i = 1;
 while ($i < @p) {
 if ($p[$i-1] eq ".") {
 splice(@p, $i-1, 1);
 $i-- if $i > 1;
 }
 elsif ($p[$i] eq ".." && $p[$i-1] ne "..") {
 splice(@p, $i-1, 2);
 if ($i > 1) {
 $i--;
 push(@p, "") if $i == @p;
 }
 }
 else {
 $i++;
 }
 }
 $p[-1] = "" if @p && $p[-1] eq "."; 
 if ($URI::ABS_REMOTE_LEADING_DOTS) {
 shift @p while @p && $p[0] =~ /^\.\.?$/;
 }
 $abs->path("/" . join("/", @p));
 $abs;
}
sub rel {
 my $self = shift;
 my $base = shift || Carp::croak("Missing base argument");
 my $rel = $self->clone;
 $base = URI->new($base) unless ref $base;
 my $scheme = $rel->scheme;
 my $auth = $rel->canonical->authority;
 my $path = $rel->path;

if (!defined($scheme) && !defined($auth)) {
 return $rel;
 }
 my $bscheme = $base->scheme;
 my $bauth = $base->canonical->authority;
 my $bpath = $base->path;

for ($bscheme, $bauth, $auth) {
 $_ = '' unless defined
 }

unless ($scheme eq $bscheme && $auth eq $bauth) {
 return $rel;
 }

for ($path, $bpath) { $_ = "/$_" unless m,^/,; }
 $rel->scheme(undef);
 $rel->authority(undef);
 my $li = 1;
 while (1) {
 my $i = index($path, '/', $li);
 last if $i < 0 ||
 $i != index($bpath, '/', $li) ||
 substr($path,$li,$i-$li) ne substr($bpath,$li,$i-$li);
 $li=$i+1;
 }
 substr($path, 0,$li) = '';
 substr($bpath,0,$li) = '';

if ($path eq $bpath &&
 defined($rel->fragment) &&
 !defined($rel->query)) {
 $rel->path("");
 }
 else {
 $path = ('../' x $bpath =~ tr|/|/|) . $path;
 $path = "./" if $path eq "";
 $rel->path($path);
 }

$rel;
}
1;
}
BEGIN { $INC{q{URI/_server.pm}} = 1;
package URI::_server;
require URI::_generic;
@ISA=qw(URI::_generic);
use strict;
use URI::Escape qw(uri_unescape);
sub _uric_escape {
 my($class, $str) = @_;
 if ($str =~ m,^((?:$URI::scheme_re:)?)//([^/?\#]*)(.*)$,os) {
 my($scheme, $host, $rest) = ($1, $2, $3);
 my $ui = $host =~ s/(.*@)// ? $1 : "";
 my $port = $host =~ s/(:\d+)\z// ? $1 : "";
 if (_host_escape($host)) {
 $str = "$scheme//$ui$host$port$rest";
 }
 }
 return $class->SUPER::_uric_escape($str);
}
sub _host_escape {
 return unless $_[0] =~ /[^URI::uric]/;
 require URI::_idna;
 $_[0] = URI::_idna::encode($_[0]);
 return 1;
}
sub as_iri {
 my $self = shift;
 my $str = $self->SUPER::as_iri;
 if ($str =~ /\bxn--/) {
 if ($str =~ m,^((?:$URI::scheme_re:)?)//([^/?\#]*)(.*)$,os) {
 my($scheme, $host, $rest) = ($1, $2, $3);
 my $ui = $host =~ s/(.*@)// ? $1 : "";
 my $port = $host =~ s/(:\d+)\z// ? $1 : "";
 require URI::_idna;
 $host = URI::_idna::encode($host);
 $str = "$scheme//$ui$host$port$rest";
 }
 }
 return $str;
}
sub userinfo
{
 my $self = shift;
 my $old = $self->authority;

if (@_) {
 my $new = $old;
 $new = "" unless defined $new;
 $new =~ s/.*@//; 
 my $ui = shift;
 if (defined $ui) {
 $ui =~ s/@/%40/g; 
 $new = "$ui\@$new";
 }
 $self->authority($new);
 }
 return undef if !defined($old) || $old !~ /(.*)@/;
 return $1;
}
sub host
{
 my $self = shift;
 my $old = $self->authority;
 if (@_) {
 my $tmp = $old;
 $tmp = "" unless defined $tmp;
 my $ui = ($tmp =~ /(.*@)/) ? $1 : "";
 my $port = ($tmp =~ /(:\d+)$/) ? $1 : "";
 my $new = shift;
 $new = "" unless defined $new;
 if (length $new) {
 $new =~ s/[@]/%40/g; 
 if ($new =~ /^[^:]*:\d*\z/ || $new =~ /]:\d*\z/) {
 $new =~ s/(:\d*)\z// || die "Assert";
 $port = $1;
 }
 $new = "[$new]" if $new =~ /:/ && $new !~ /^\[/; 
 _host_escape($new);
 }
 $self->authority("$ui$new$port");
 }
 return undef unless defined $old;
 $old =~ s/.*@//;
 $old =~ s/:\d+$//; 
 $old =~ s{^\[(.*)\]$}{$1}; 
 return uri_unescape($old);
}
sub ihost
{
 my $self = shift;
 my $old = $self->host(@_);
 if ($old =~ /(^|\.)xn--/) {
 require URI::_idna;
 $old = URI::_idna::decode($old);
 }
 return $old;
}
sub _port
{
 my $self = shift;
 my $old = $self->authority;
 if (@_) {
 my $new = $old;
 $new =~ s/:\d*$//;
 my $port = shift;
 $new .= ":$port" if defined $port;
 $self->authority($new);
 }
 return $1 if defined($old) && $old =~ /:(\d*)$/;
 return;
}
sub port
{
 my $self = shift;
 my $port = $self->_port(@_);
 $port = $self->default_port if !defined($port) || $port eq "";
 $port;
}
sub host_port
{
 my $self = shift;
 my $old = $self->authority;
 $self->host(shift) if @_;
 return undef unless defined $old;
 $old =~ s/.*@//; 
 $old =~ s/:$//; 
 $old .= ":" . $self->port unless $old =~ /:\d+$/;
 $old;
}
sub default_port { undef }
sub canonical
{
 my $self = shift;
 my $other = $self->SUPER::canonical;
 my $host = $other->host || "";
 my $port = $other->_port;
 my $uc_host = $host =~ /[A-Z]/;
 my $def_port = defined($port) && ($port eq "" ||
 $port == $self->default_port);
 if ($uc_host || $def_port) {
 $other = $other->clone if $other == $self;
 $other->host(lc $host) if $uc_host;
 $other->port(undef) if $def_port;
 }
 $other;
}
1;
}
BEGIN { $INC{q{URI/_punycode.pm}} = 1;
package URI::_punycode;
use strict;
our $VERSION = 0.02;
require Exporter;
our @ISA = qw(Exporter);
our @EXPORT = qw(encode_punycode decode_punycode);
use integer;
our $DEBUG = 0;
use constant BASE => 36;
use constant TMIN => 1;
use constant TMAX => 26;
use constant SKEW => 38;
use constant DAMP => 700;
use constant INITIAL_BIAS => 72;
use constant INITIAL_N => 128;
my $Delimiter = chr 0x2D;
my $BasicRE = qr/[\x00-\x7f]/;
sub _croak { require Carp; Carp::croak(@_); }
sub digit_value {
 my $code = shift;
 return ord($code) - ord("A") if $code =~ /[A-Z]/;
 return ord($code) - ord("a") if $code =~ /[a-z]/;
 return ord($code) - ord("0") + 26 if $code =~ /[0-9]/;
 return;
}
sub code_point {
 my $digit = shift;
 return $digit + ord('a') if 0 <= $digit && $digit <= 25;
 return $digit + ord('0') - 26 if 26 <= $digit && $digit <= 36;
 die 'NOT COME HERE';
}
sub adapt {
 my($delta, $numpoints, $firsttime) = @_;
 $delta = $firsttime ? $delta / DAMP : $delta / 2;
 $delta += $delta / $numpoints;
 my $k = 0;
 while ($delta > ((BASE - TMIN) * TMAX) / 2) {
 $delta /= BASE - TMIN;
 $k += BASE;
 }
 return $k + (((BASE - TMIN + 1) * $delta) / ($delta + SKEW));
}
sub decode_punycode {
 my $code = shift;

my $n = INITIAL_N;
 my $i = 0;
 my $bias = INITIAL_BIAS;
 my @output;

if ($code =~ s/(.*)$Delimiter//o) {
 push @output, map ord, split //, $1;
 return _croak('non-basic code point') unless $1 =~ /^$BasicRE*$/o;
 }

while ($code) {
 my $oldi = $i;
 my $w = 1;
 LOOP:
 for (my $k = BASE; 1; $k += BASE) {
 my $cp = substr($code, 0, 1, '');
 my $digit = digit_value($cp);
 defined $digit or return _croak("invalid punycode input");
 $i += $digit * $w;
 my $t = ($k <= $bias) ? TMIN
 : ($k >= $bias + TMAX) ? TMAX : $k - $bias;
 last LOOP if $digit < $t;
 $w *= (BASE - $t);
 }
 $bias = adapt($i - $oldi, @output + 1, $oldi == 0);
 warn "bias becomes $bias" if $DEBUG;
 $n += $i / (@output + 1);
 $i = $i % (@output + 1);
 splice(@output, $i, 0, $n);
 warn join " ", map sprintf('%04x', $_), @output if $DEBUG;
 $i++;
 }
 return join '', map chr, @output;
}
sub encode_punycode {
 my $input = shift;
 my @input = map substr($input, $_, 1), 0..length($input)-1;

my $n = INITIAL_N;
 my $delta = 0;
 my $bias = INITIAL_BIAS;

my @output;
 my @basic = grep /$BasicRE/, @input;
 my $h = my $b = @basic;
 push @output, @basic;
 push @output, $Delimiter if $b && $h < @input;
 warn "basic codepoints: (@output)" if $DEBUG;

while ($h < @input) {
 my $m = min(grep { $_ >= $n } map ord, @input);
 warn sprintf "next code point to insert is %04x", $m if $DEBUG;
 $delta += ($m - $n) * ($h + 1);
 $n = $m;
 for my $i (@input) {
 my $c = ord($i);
 $delta++ if $c < $n;
 if ($c == $n) {
 my $q = $delta;
 LOOP:
 for (my $k = BASE; 1; $k += BASE) {
 my $t = ($k <= $bias) ? TMIN :
 ($k >= $bias + TMAX) ? TMAX : $k - $bias;
 last LOOP if $q < $t;
 my $cp = code_point($t + (($q - $t) % (BASE - $t)));
 push @output, chr($cp);
 $q = ($q - $t) / (BASE - $t);
 }
 push @output, chr(code_point($q));
 $bias = adapt($delta, $h + 1, $h == $b);
 warn "bias becomes $bias" if $DEBUG;
 $delta = 0;
 $h++;
 }
 }
 $delta++;
 $n++;
 }
 return join '', @output;
}
sub min {
 my $min = shift;
 for (@_) { $min = $_ if $_ <= $min }
 return $min;
}
1;
}
BEGIN { $INC{q{URI/_idna.pm}} = 1;
package URI::_idna;
use strict;
use URI::_punycode qw(encode_punycode decode_punycode);
use Carp qw(croak);
my $ASCII = qr/^[\x00-\x7F]*\z/;
sub encode {
 my $idomain = shift;
 my @labels = split(/\./, $idomain, -1);
 my @last_empty;
 push(@last_empty, pop @labels) if @labels > 1 && $labels[-1] eq "";
 for (@labels) {
 $_ = ToASCII($_);
 }
 return join(".", @labels, @last_empty);
}
sub decode {
 my $domain = shift;
 return join(".", map ToUnicode($_), split(/\./, $domain, -1))
}
sub nameprep { 
 my $label = shift;
 $label = lc($label);
 return $label;
}
sub check_size {
 my $label = shift;
 croak "Label empty" if $label eq "";
 croak "Label too long" if length($label) > 63;
 return $label;
}
sub ToASCII {
 my $label = shift;
 return check_size($label) if $label =~ $ASCII;
 $label = nameprep($label);
 return check_size($label) if $label =~ $ASCII;
 if ($label =~ /^xn--/) {
 croak "Label starts with ACE prefix";
 }
 $label = encode_punycode($label);
 $label = "xn--$label";
 return check_size($label);
}
sub ToUnicode {
 my $label = shift;
 $label = nameprep($label) unless $label =~ $ASCII;
 return $label unless $label =~ /^xn--/;
 my $label1 = decode_punycode(substr($label, 4));
 my $label2 = ToASCII($label);
 if (lc($label) ne $label2) {
 croak "IDNA does not round-trip: '$label' vs '$label2'";
 }
 return $label1;
}
1;
}
BEGIN { $INC{q{URI/http.pm}} = 1;
package URI::http;
require URI::_server;
@ISA=qw(URI::_server);
use strict;
sub default_port { 80 }
sub canonical
{
 my $self = shift;
 my $other = $self->SUPER::canonical;

my $slash_path = defined($other->authority) &&
 !length($other->path) && !defined($other->query);

if ($slash_path) {
 $other = $other->clone if $other == $self;
 $other->path("/");
 }
 $other;
}
1;
}
BEGIN { $INC{q{HTTP/Date.pm}} = 1;
package HTTP::Date;
$VERSION = "5.831";
require 5.004;
require Exporter;
@ISA = qw(Exporter);
@EXPORT = qw(time2str str2time);
@EXPORT_OK = qw(parse_date time2iso time2isoz);
use strict;
require Time::Local;
use vars qw(@DoW @MoY %MoY);
@DoW = qw(Sun Mon Tue Wed Thu Fri Sat);
@MoY = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
@MoY{@MoY} = (1..12);
my %GMT_ZONE = (GMT => 1, UTC => 1, UT => 1, Z => 1);
sub time2str (;$)
{
 my $time = shift;
 $time = time unless defined $time;
 my ($sec, $min, $hour, $mday, $mon, $year, $wday) = gmtime($time);
 sprintf("%s, %02d %s %04d %02d:%02d:%02d GMT",
 $DoW[$wday],
 $mday, $MoY[$mon], $year+1900,
 $hour, $min, $sec);
}
sub str2time ($;$)
{
 my $str = shift;
 return undef unless defined $str;
 if ($str =~ /^[SMTWF][a-z][a-z], (\d\d) ([JFMAJSOND][a-z][a-z]) (\d\d\d\d) (\d\d):(\d\d):(\d\d) GMT$/) {
 return eval {
 my $t = Time::Local::timegm($6, $5, $4, $1, $MoY{$2}-1, $3);
 $t < 0 ? undef : $t;
 };
 }

my @d = parse_date($str);
 return undef unless @d;
 $d[1]--; 

my $tz = pop(@d);
 unless (defined $tz) {
 unless (defined($tz = shift)) {
 return eval { my $frac = $d[-1]; $frac -= ($d[-1] = int($frac));
 my $t = Time::Local::timelocal(reverse @d) + $frac;
 $t < 0 ? undef : $t;
 };
 }
 }

my $offset = 0;
 if ($GMT_ZONE{uc $tz}) {
 }
 elsif ($tz =~ /^([-+])?(\d\d?):?(\d\d)?$/) {
 $offset = 3600 * $2;
 $offset += 60 * $3 if $3;
 $offset *= -1 if $1 && $1 eq '-';
 }
 else {
 eval { require Time::Zone } || return undef;
 $offset = Time::Zone::tz_offset($tz);
 return undef unless defined $offset;
 }

return eval { my $frac = $d[-1]; $frac -= ($d[-1] = int($frac));
 my $t = Time::Local::timegm(reverse @d) + $frac;
 $t < 0 ? undef : $t - $offset;
 };
}
sub parse_date ($)
{
 local($_) = shift;
 return unless defined;
 s/^\s+//; 
 s/^(?:Sun|Mon|Tue|Wed|Thu|Fri|Sat)[a-z]*,?\s*//i; 

my($day, $mon, $yr, $hr, $min, $sec, $tz, $ampm);
 (($day,$mon,$yr,$hr,$min,$sec,$tz) =
 /^
	 (\d\d?)               # day
	    (?:\s+|[-\/])
	 (\w+)                 # month
	    (?:\s+|[-\/])
	 (\d+)                 # year
	 (?:
	       (?:\s+|:)       # separator before clock
	    (\d\d?):(\d\d)     # hour:min
	    (?::(\d\d))?       # optional seconds
	 )?                    # optional clock
	    \s*
	 ([-+]?\d{2,4}|(?![APap][Mm]\b)[A-Za-z]+)? # timezone
	    \s*
	 (?:\(\w+\))?	       # ASCII representation of timezone in parens.
	    \s*$
	/x)

||
 (($mon, $day, $hr, $min, $sec, $tz, $yr) =
 /^
	 (\w{1,3})             # month
	    \s+
	 (\d\d?)               # day
	    \s+
	 (\d\d?):(\d\d)        # hour:min
	 (?::(\d\d))?          # optional seconds
	    \s+
	 (?:([A-Za-z]+)\s+)?   # optional timezone
	 (\d+)                 # year
	    \s*$               # allow trailing whitespace
	/x)

||
 (($mon, $day, $yr, $hr, $min, $sec) =
 /^
	 (\w{3})               # month
	    \s+
	 (\d\d?)               # day
	    \s+
	 (?:
	    (\d\d\d\d) |       # year
	    (\d{1,2}):(\d{2})  # hour:min
            (?::(\d\d))?       # optional seconds
	 )
	 \s*$
       /x)

||
 (($yr, $mon, $day, $hr, $min, $sec, $tz) =
 /^
	  (\d{4})              # year
	     [-\/]?
	  (\d\d?)              # numerical month
	     [-\/]?
	  (\d\d?)              # day
	 (?:
	       (?:\s+|[-:Tt])  # separator before clock
	    (\d\d?):?(\d\d)    # hour:min
	    (?::?(\d\d(?:\.\d*)?))?  # optional seconds (and fractional)
	 )?                    # optional clock
	    \s*
	 ([-+]?\d\d?:?(:?\d\d)?
	  |Z|z)?               # timezone  (Z is "zero meridian", i.e. GMT)
	    \s*$
	/x)

||
 (($mon, $day, $yr, $hr, $min, $ampm) =
 /^
          (\d{2})                # numerical month
             -
          (\d{2})                # day
             -
          (\d{2})                # year
             \s+
          (\d\d?):(\d\d)([APap][Mm])  # hour:min AM or PM
             \s*$
        /x)

||
 return; 
 $mon = $MoY{$mon} ||
 $MoY{"\u\L$mon"} ||
 ($mon =~ /^\d\d?$/ && $mon >= 1 && $mon <= 12 && int($mon)) ||
 return;
 unless (defined $yr) {
 my $cur_mon;
 ($cur_mon, $yr) = (localtime)[4, 5];
 $yr += 1900;
 $cur_mon++;
 $yr-- if $mon > $cur_mon;
 }
 elsif (length($yr) < 3) {
 my $cur_yr = (localtime)[5] + 1900;
 my $m = $cur_yr % 100;
 my $tmp = $yr;
 $yr += $cur_yr - $m;
 $m -= $tmp;
 $yr += ($m > 0) ? 100 : -100
 if abs($m) > 50;
 }
 $hr = 0 unless defined($hr);
 $min = 0 unless defined($min);
 $sec = 0 unless defined($sec);
 if ($ampm) {
 $ampm = uc $ampm;
 $hr = 0 if $hr == 12 && $ampm eq 'AM';
 $hr += 12 if $ampm eq 'PM' && $hr != 12;
 }

return($yr, $mon, $day, $hr, $min, $sec, $tz)
 if wantarray;

if (defined $tz) {
 $tz = "Z" if $tz =~ /^(GMT|UTC?|[-+]?0+)$/;
 }
 else {
 $tz = "";
 }
 return sprintf("%04d-%02d-%02d %02d:%02d:%02d%s",
 $yr, $mon, $day, $hr, $min, $sec, $tz);
}
sub time2iso (;$)
{
 my $time = shift;
 $time = time unless defined $time;
 my($sec,$min,$hour,$mday,$mon,$year) = localtime($time);
 sprintf("%04d-%02d-%02d %02d:%02d:%02d",
 $year+1900, $mon+1, $mday, $hour, $min, $sec);
}
sub time2isoz (;$)
{
 my $time = shift;
 $time = time unless defined $time;
 my($sec,$min,$hour,$mday,$mon,$year) = gmtime($time);
 sprintf("%04d-%02d-%02d %02d:%02d:%02dZ",
 $year+1900, $mon+1, $mday, $hour, $min, $sec);
}
1;
}
BEGIN { $INC{q{HTTP/Status.pm}} = 1;
package HTTP::Status;
use strict;
require 5.002; 
use vars qw(@ISA @EXPORT @EXPORT_OK %EXPORT_TAGS $VERSION);
require Exporter;
@ISA = qw(Exporter);
@EXPORT = qw(is_info is_success is_redirect is_error status_message);
@EXPORT_OK = qw(is_client_error is_server_error);
$VERSION = "5.817";
my %StatusCode = (
 100 => 'Continue',
 101 => 'Switching Protocols',
 102 => 'Processing', 
 200 => 'OK',
 201 => 'Created',
 202 => 'Accepted',
 203 => 'Non-Authoritative Information',
 204 => 'No Content',
 205 => 'Reset Content',
 206 => 'Partial Content',
 207 => 'Multi-Status', 
 300 => 'Multiple Choices',
 301 => 'Moved Permanently',
 302 => 'Found',
 303 => 'See Other',
 304 => 'Not Modified',
 305 => 'Use Proxy',
 307 => 'Temporary Redirect',
 400 => 'Bad Request',
 401 => 'Unauthorized',
 402 => 'Payment Required',
 403 => 'Forbidden',
 404 => 'Not Found',
 405 => 'Method Not Allowed',
 406 => 'Not Acceptable',
 407 => 'Proxy Authentication Required',
 408 => 'Request Timeout',
 409 => 'Conflict',
 410 => 'Gone',
 411 => 'Length Required',
 412 => 'Precondition Failed',
 413 => 'Request Entity Too Large',
 414 => 'Request-URI Too Large',
 415 => 'Unsupported Media Type',
 416 => 'Request Range Not Satisfiable',
 417 => 'Expectation Failed',
 422 => 'Unprocessable Entity', 
 423 => 'Locked', 
 424 => 'Failed Dependency', 
 425 => 'No code', 
 426 => 'Upgrade Required', 
 449 => 'Retry with', 
 500 => 'Internal Server Error',
 501 => 'Not Implemented',
 502 => 'Bad Gateway',
 503 => 'Service Unavailable',
 504 => 'Gateway Timeout',
 505 => 'HTTP Version Not Supported',
 506 => 'Variant Also Negotiates', 
 507 => 'Insufficient Storage', 
 509 => 'Bandwidth Limit Exceeded', 
 510 => 'Not Extended', 
);
my $mnemonicCode = '';
my ($code, $message);
while (($code, $message) = each %StatusCode) {
 $message =~ tr/a-z \-/A-Z__/;
 $mnemonicCode .= "sub HTTP_$message () { $code }\n";
 $mnemonicCode .= "*RC_$message = \\&HTTP_$message;\n"; 
 $mnemonicCode .= "push(\@EXPORT_OK, 'HTTP_$message');\n";
 $mnemonicCode .= "push(\@EXPORT, 'RC_$message');\n";
}
eval $mnemonicCode; 
die if $@;
*RC_MOVED_TEMPORARILY = \&RC_FOUND; 
push(@EXPORT, "RC_MOVED_TEMPORARILY");
%EXPORT_TAGS = (
 constants => [grep /^HTTP_/, @EXPORT_OK],
 is => [grep /^is_/, @EXPORT, @EXPORT_OK],
);
sub status_message ($) { $StatusCode{$_[0]}; }
sub is_info ($) { $_[0] >= 100 && $_[0] < 200; }
sub is_success ($) { $_[0] >= 200 && $_[0] < 300; }
sub is_redirect ($) { $_[0] >= 300 && $_[0] < 400; }
sub is_error ($) { $_[0] >= 400 && $_[0] < 600; }
sub is_client_error ($) { $_[0] >= 400 && $_[0] < 500; }
sub is_server_error ($) { $_[0] >= 500 && $_[0] < 600; }
1;
}
BEGIN { $INC{q{HTTP/Headers.pm}} = 1;
package HTTP::Headers;
use strict;
use Carp ();
use vars qw($VERSION $TRANSLATE_UNDERSCORE);
$VERSION = "5.827";
$TRANSLATE_UNDERSCORE = 1 unless defined $TRANSLATE_UNDERSCORE;
my @general_headers = qw(Cache-Control Connection Date Pragma Trailer Transfer-Encoding Upgrade Via Warning);
my @request_headers = qw(Accept Accept-Charset Accept-Encoding Accept-Language Authorization Expect From Host If-Match If-Modified-Since If-None-Match If-Range If-Unmodified-Since Max-Forwards Proxy-Authorization Range Referer TE User-Agent);
my @response_headers = qw(Accept-Ranges Age ETag Location Proxy-Authenticate Retry-After Server Vary WWW-Authenticate);
my @entity_headers = qw(Allow Content-Encoding Content-Language Content-Length Content-Location Content-MD5 Content-Range Content-Type Expires Last-Modified);
my %entity_header = map { lc($_) => 1 } @entity_headers;
my @header_order = (
 @general_headers,
 @request_headers,
 @response_headers,
 @entity_headers,
);
my %header_order;
my %standard_case;
{
 my $i = 0;
 for (@header_order) {
 my $lc = lc $_;
 $header_order{$lc} = ++$i;
 $standard_case{$lc} = $_;
 }
}
sub new
{
 my($class) = shift;
 my $self = bless {}, $class;
 $self->header(@_) if @_; 
 $self;
}
sub header
{
 my $self = shift;
 Carp::croak('Usage: $h->header($field, ...)') unless @_;
 my(@old);
 my %seen;
 while (@_) {
 my $field = shift;
 my $op = @_ ? ($seen{lc($field)}++ ? 'PUSH' : 'SET') : 'GET';
 @old = $self->_header($field, shift, $op);
 }
 return @old if wantarray;
 return $old[0] if @old <= 1;
 join(", ", @old);
}
sub clear
{
 my $self = shift;
 %$self = ();
}
sub push_header
{
 my $self = shift;
 return $self->_header(@_, 'PUSH_H') if @_ == 2;
 while (@_) {
 $self->_header(splice(@_, 0, 2), 'PUSH_H');
 }
}
sub init_header
{
 Carp::croak('Usage: $h->init_header($field, $val)') if @_ != 3;
 shift->_header(@_, 'INIT');
}
sub remove_header
{
 my($self, @fields) = @_;
 my $field;
 my @values;
 foreach $field (@fields) {
 $field =~ tr/_/-/ if $field !~ /^:/ && $TRANSLATE_UNDERSCORE;
 my $v = delete $self->{lc $field};
 push(@values, ref($v) eq 'ARRAY' ? @$v : $v) if defined $v;
 }
 return @values;
}
sub remove_content_headers
{
 my $self = shift;
 unless (defined(wantarray)) {
 delete @$self{grep $entity_header{$_} || /^content-/, keys %$self};
 return;
 }

my $c = ref($self)->new;
 for my $f (grep $entity_header{$_} || /^content-/, keys %$self) {
 $c->{$f} = delete $self->{$f};
 }
 $c;
}
sub _header
{
 my($self, $field, $val, $op) = @_;

unless ($field =~ /^:/) {
 $field =~ tr/_/-/ if $TRANSLATE_UNDERSCORE;
 my $old = $field;
 $field = lc $field;
 unless(defined $standard_case{$field}) {
 $old =~ s/\b(\w)/\u$1/g;
 $standard_case{$field} = $old;
 }
 }

$op ||= defined($val) ? 'SET' : 'GET';
 if ($op eq 'PUSH_H') {
 if (exists $self->{$field}) {
 my $h = $self->{$field};
 if (ref($h) eq 'ARRAY') {
 push(@$h, ref($val) eq "ARRAY" ? @$val : $val);
 }
 else {
 $self->{$field} = [$h, ref($val) eq "ARRAY" ? @$val : $val]
 }
 return;
 }
 $self->{$field} = $val;
 return;
 }

my $h = $self->{$field};
 my @old = ref($h) eq 'ARRAY' ? @$h : (defined($h) ? ($h) : ());

unless ($op eq 'GET' || ($op eq 'INIT' && @old)) {
 if (defined($val)) {
 my @new = ($op eq 'PUSH') ? @old : ();
 if (ref($val) ne 'ARRAY') {
 push(@new, $val);
 }
 else {
 push(@new, @$val);
 }
 $self->{$field} = @new > 1 ? \@new : $new[0];
 }
 elsif ($op ne 'PUSH') {
 delete $self->{$field};
 }
 }
 @old;
}
sub _sorted_field_names
{
 my $self = shift;
 return sort {
 ($header_order{$a} || 999) <=> ($header_order{$b} || 999) ||
 $a cmp $b
 } keys %$self
}
sub header_field_names {
 my $self = shift;
 return map $standard_case{$_} || $_, $self->_sorted_field_names
 if wantarray;
 return keys %$self;
}
sub scan
{
 my($self, $sub) = @_;
 my $key;
 foreach $key ($self->_sorted_field_names) {
 next if $key =~ /^_/;
 my $vals = $self->{$key};
 if (ref($vals) eq 'ARRAY') {
 my $val;
 for $val (@$vals) {
 &$sub($standard_case{$key} || $key, $val);
 }
 }
 else {
 &$sub($standard_case{$key} || $key, $vals);
 }
 }
}
sub as_string
{
 my($self, $endl) = @_;
 $endl = "\n" unless defined $endl;

my @result = ();
 $self->scan(sub {
 my($field, $val) = @_;
 $field =~ s/^://;
 if ($val =~ /\n/) {
 $val =~ s/\s+$//; 
 $val =~ s/\n\n+/\n/g; 
 $val =~ s/\n([^\040\t])/\n $1/g; 
 $val =~ s/\n/$endl/g; 
 }
 push(@result, "$field: $val");
 });

join($endl, @result, '');
}
if (eval { require Storable; 1 }) {
 *clone = \&Storable::dclone;
} else {
 *clone = sub {
 my $self = shift;
 my $clone = new HTTP::Headers;
 $self->scan(sub { $clone->push_header(@_);} );
 $clone;
 };
}
sub _date_header
{
 require HTTP::Date;
 my($self, $header, $time) = @_;
 my($old) = $self->_header($header);
 if (defined $time) {
 $self->_header($header, HTTP::Date::time2str($time));
 }
 $old =~ s/;.*// if defined($old);
 HTTP::Date::str2time($old);
}
sub date { shift->_date_header('Date', @_); }
sub expires { shift->_date_header('Expires', @_); }
sub if_modified_since { shift->_date_header('If-Modified-Since', @_); }
sub if_unmodified_since { shift->_date_header('If-Unmodified-Since', @_); }
sub last_modified { shift->_date_header('Last-Modified', @_); }
sub client_date { shift->_date_header('Client-Date', @_); }
sub content_type {
 my $self = shift;
 my $ct = $self->{'content-type'};
 $self->{'content-type'} = shift if @_;
 $ct = $ct->[0] if ref($ct) eq 'ARRAY';
 return '' unless defined($ct) && length($ct);
 my @ct = split(/;\s*/, $ct, 2);
 for ($ct[0]) {
 s/\s+//g;
 $_ = lc($_);
 }
 wantarray ? @ct : $ct[0];
}
sub content_type_charset {
 my $self = shift;
 require HTTP::Headers::Util;
 my $h = $self->{'content-type'};
 $h = $h->[0] if ref($h);
 $h = "" unless defined $h;
 my @v = HTTP::Headers::Util::split_header_words($h);
 if (@v) {
 my($ct, undef, %ct_param) = @{$v[0]};
 my $charset = $ct_param{charset};
 if ($ct) {
 $ct = lc($ct);
 $ct =~ s/\s+//;
 }
 if ($charset) {
 $charset = uc($charset);
 $charset =~ s/^\s+//; $charset =~ s/\s+\z//;
 undef($charset) if $charset eq "";
 }
 return $ct, $charset if wantarray;
 return $charset;
 }
 return undef, undef if wantarray;
 return undef;
}
sub content_is_text {
 my $self = shift;
 return $self->content_type =~ m,^text/,;
}
sub content_is_html {
 my $self = shift;
 return $self->content_type eq 'text/html' || $self->content_is_xhtml;
}
sub content_is_xhtml {
 my $ct = shift->content_type;
 return $ct eq "application/xhtml+xml" ||
 $ct eq "application/vnd.wap.xhtml+xml";
}
sub content_is_xml {
 my $ct = shift->content_type;
 return 1 if $ct eq "text/xml";
 return 1 if $ct eq "application/xml";
 return 1 if $ct =~ /\+xml$/;
 return 0;
}
sub referer {
 my $self = shift;
 if (@_ && $_[0] =~ /#/) {
 my $uri = shift;
 if (ref($uri)) {
 $uri = $uri->clone;
 $uri->fragment(undef);
 }
 else {
 $uri =~ s/\#.*//;
 }
 unshift @_, $uri;
 }
 ($self->_header('Referer', @_))[0];
}
*referrer = \&referer; 
sub title { (shift->_header('Title', @_))[0] }
sub content_encoding { (shift->_header('Content-Encoding', @_))[0] }
sub content_language { (shift->_header('Content-Language', @_))[0] }
sub content_length { (shift->_header('Content-Length', @_))[0] }
sub user_agent { (shift->_header('User-Agent', @_))[0] }
sub server { (shift->_header('Server', @_))[0] }
sub from { (shift->_header('From', @_))[0] }
sub warning { (shift->_header('Warning', @_))[0] }
sub www_authenticate { (shift->_header('WWW-Authenticate', @_))[0] }
sub authorization { (shift->_header('Authorization', @_))[0] }
sub proxy_authenticate { (shift->_header('Proxy-Authenticate', @_))[0] }
sub proxy_authorization { (shift->_header('Proxy-Authorization', @_))[0] }
sub authorization_basic { shift->_basic_auth("Authorization", @_) }
sub proxy_authorization_basic { shift->_basic_auth("Proxy-Authorization", @_) }
sub _basic_auth {
 require MIME::Base64;
 my($self, $h, $user, $passwd) = @_;
 my($old) = $self->_header($h);
 if (defined $user) {
 Carp::croak("Basic authorization user name can't contain ':'")
 if $user =~ /:/;
 $passwd = '' unless defined $passwd;
 $self->_header($h => 'Basic ' .
 MIME::Base64::encode("$user:$passwd", ''));
 }
 if (defined $old && $old =~ s/^\s*Basic\s+//) {
 my $val = MIME::Base64::decode($old);
 return $val unless wantarray;
 return split(/:/, $val, 2);
 }
 return;
}
1;
}
BEGIN { $INC{q{HTTP/Message.pm}} = 1;
package HTTP::Message;
use strict;
use vars qw($VERSION $AUTOLOAD);
$VERSION = "5.834";
require HTTP::Headers;
require Carp;
my $CRLF = "\015\012"; 
$HTTP::URI_CLASS ||= $ENV{PERL_HTTP_URI_CLASS} || "URI";
eval "require $HTTP::URI_CLASS"; die $@ if $@;
*_utf8_downgrade = defined(&utf8::downgrade) ?
 sub {
 utf8::downgrade($_[0], 1) or
 Carp::croak("HTTP::Message content must be bytes")
 }
 :
 sub {
 };
sub new
{
 my($class, $header, $content) = @_;
 if (defined $header) {
 Carp::croak("Bad header argument") unless ref $header;
 if (ref($header) eq "ARRAY") {
 $header = HTTP::Headers->new(@$header);
 }
 else {
 $header = $header->clone;
 }
 }
 else {
 $header = HTTP::Headers->new;
 }
 if (defined $content) {
 _utf8_downgrade($content);
 }
 else {
 $content = '';
 }

bless {
 '_headers' => $header,
 '_content' => $content,
 }, $class;
}
sub parse
{
 my($class, $str) = @_;

my @hdr;
 while (1) {
 if ($str =~ s/^([^\s:]+)[ \t]*: ?(.*)\n?//) {
 push(@hdr, $1, $2);
 $hdr[-1] =~ s/\r\z//;
 }
 elsif (@hdr && $str =~ s/^([ \t].*)\n?//) {
 $hdr[-1] .= "\n$1";
 $hdr[-1] =~ s/\r\z//;
 }
 else {
 $str =~ s/^\r?\n//;
 last;
 }
 }
 local $HTTP::Headers::TRANSLATE_UNDERSCORE;
 new($class, \@hdr, $str);
}
sub clone
{
 my $self = shift;
 my $clone = HTTP::Message->new($self->headers,
 $self->content);
 $clone->protocol($self->protocol);
 $clone;
}
sub clear {
 my $self = shift;
 $self->{_headers}->clear;
 $self->content("");
 delete $self->{_parts};
 return;
}
sub protocol {
 shift->_elem('_protocol', @_);
}
sub headers {
 my $self = shift;
 $self->_content unless exists $self->{_content};

$self->{_headers};
}
sub headers_as_string {
 shift->headers->as_string(@_);
}
sub content {

my $self = $_[0];
 if (defined(wantarray)) {
 $self->_content unless exists $self->{_content};
 my $old = $self->{_content};
 $old = $$old if ref($old) eq "SCALAR";
 &_set_content if @_ > 1;
 return $old;
 }

if (@_ > 1) {
 &_set_content;
 }
 else {
 Carp::carp("Useless content call in void context") if $^W;
 }
}
sub _set_content {
 my $self = $_[0];
 _utf8_downgrade($_[1]);
 if (!ref($_[1]) && ref($self->{_content}) eq "SCALAR") {
 ${$self->{_content}} = $_[1];
 }
 else {
 die "Can't set content to be a scalar reference" if ref($_[1]) eq "SCALAR";
 $self->{_content} = $_[1];
 delete $self->{_content_ref};
 }
 delete $self->{_parts} unless $_[2];
}
sub add_content
{
 my $self = shift;
 $self->_content unless exists $self->{_content};
 my $chunkref = \$_[0];
 $chunkref = $$chunkref if ref($$chunkref); 

_utf8_downgrade($$chunkref);

my $ref = ref($self->{_content});
 if (!$ref) {
 $self->{_content} .= $$chunkref;
 }
 elsif ($ref eq "SCALAR") {
 ${$self->{_content}} .= $$chunkref;
 }
 else {
 Carp::croak("Can't append to $ref content");
 }
 delete $self->{_parts};
}
sub add_content_utf8 {
 my($self, $buf) = @_;
 utf8::upgrade($buf);
 utf8::encode($buf);
 $self->add_content($buf);
}
sub content_ref
{
 my $self = shift;
 $self->_content unless exists $self->{_content};
 delete $self->{_parts};
 my $old = \$self->{_content};
 my $old_cref = $self->{_content_ref};
 if (@_) {
 my $new = shift;
 Carp::croak("Setting content_ref to a non-ref") unless ref($new);
 delete $self->{_content}; 
 $self->{_content} = $new;
 $self->{_content_ref}++;
 }
 $old = $$old if $old_cref;
 return $old;
}
sub content_charset
{
 my $self = shift;
 if (my $charset = $self->content_type_charset) {
 return $charset;
 }
 my $cref = $self->decoded_content(ref => 1, charset => "none");
 local $_;
 for ($$cref) {
 return "UTF-8" if /^\xEF\xBB\xBF/;
 return "UTF-32-LE" if /^\xFF\xFE\x00\x00/;
 return "UTF-32-BE" if /^\x00\x00\xFE\xFF/;
 return "UTF-16-LE" if /^\xFF\xFE/;
 return "UTF-16-BE" if /^\xFE\xFF/;
 }

if ($self->content_is_xml) {
 for ($$cref) {
 return "UTF-32-BE" if /^\x00\x00\x00</;
 return "UTF-32-LE" if /^<\x00\x00\x00/;
 return "UTF-16-BE" if /^(?:\x00\s)*\x00</;
 return "UTF-16-LE" if /^(?:\s\x00)*<\x00/;
 if (/^\s*(<\?xml[^\x00]*?\?>)/) {
 if ($1 =~ /\sencoding\s*=\s*(["'])(.*?)\1/) {
 my $enc = $2;
 $enc =~ s/^\s+//; $enc =~ s/\s+\z//;
 return $enc if $enc;
 }
 }
 }
 return "UTF-8";
 }
 elsif ($self->content_is_html) {
 my $charset;
 require HTML::Parser;
 my $p = HTML::Parser->new(
 start_h => [sub {
 my($tag, $attr, $self) = @_;
 $charset = $attr->{charset};
 unless ($charset) {
 if (my $c = $attr->{content}) {
 require HTTP::Headers::Util;
 my @v = HTTP::Headers::Util::split_header_words($c);
 return unless @v;
 my($ct, undef, %ct_param) = @{$v[0]};
 $charset = $ct_param{charset};
 }
 return unless $charset;
 }
 if ($charset =~ /^utf-?16/i) {
 $charset = "UTF-8";
 }
 $self->eof;
 }, "tagname, attr, self"],
 report_tags => [qw(meta)],
 utf8_mode => 1,
 );
 $p->parse($$cref);
 return $charset if $charset;
 }
 if ($self->content_type =~ /^text\//) {
 for ($$cref) {
 if (length) {
 return "US-ASCII" unless /[\x80-\xFF]/;
 require Encode;
 eval {
 Encode::decode_utf8($_, Encode::FB_CROAK());
 };
 return "UTF-8" unless $@;
 return "ISO-8859-1";
 }
 }
 }

return undef;
}
sub decoded_content
{
 my($self, %opt) = @_;
 my $content_ref;
 my $content_ref_iscopy;

eval {
 $content_ref = $self->content_ref;
 die "Can't decode ref content" if ref($content_ref) ne "SCALAR";

if (my $h = $self->header("Content-Encoding")) {
 $h =~ s/^\s+//;
 $h =~ s/\s+$//;
 for my $ce (reverse split(/\s*,\s*/, lc($h))) {
 next unless $ce;
 next if $ce eq "identity";
 if ($ce eq "gzip" || $ce eq "x-gzip") {
 require IO::Uncompress::Gunzip;
 my $output;
 IO::Uncompress::Gunzip::gunzip($content_ref, \$output, Transparent => 0)
 or die "Can't gunzip content: $IO::Uncompress::Gunzip::GunzipError";
 $content_ref = \$output;
 $content_ref_iscopy++;
 }
 elsif ($ce eq "x-bzip2") {
 require IO::Uncompress::Bunzip2;
 my $output;
 IO::Uncompress::Bunzip2::bunzip2($content_ref, \$output, Transparent => 0)
 or die "Can't bunzip content: $IO::Uncompress::Bunzip2::Bunzip2Error";
 $content_ref = \$output;
 $content_ref_iscopy++;
 }
 elsif ($ce eq "deflate") {
 require IO::Uncompress::Inflate;
 my $output;
 my $status = IO::Uncompress::Inflate::inflate($content_ref, \$output, Transparent => 0);
 my $error = $IO::Uncompress::Inflate::InflateError;
 unless ($status) {
 $output = undef;
 require IO::Uncompress::RawInflate;
 unless (IO::Uncompress::RawInflate::rawinflate($content_ref, \$output)) {
 $self->push_header("Client-Warning" =>
 "Could not raw inflate content: $IO::Uncompress::RawInflate::RawInflateError");
 $output = undef;
 }
 }
 die "Can't inflate content: $error" unless defined $output;
 $content_ref = \$output;
 $content_ref_iscopy++;
 }
 elsif ($ce eq "compress" || $ce eq "x-compress") {
 die "Can't uncompress content";
 }
 elsif ($ce eq "base64") { 
 require MIME::Base64;
 $content_ref = \MIME::Base64::decode($$content_ref);
 $content_ref_iscopy++;
 }
 elsif ($ce eq "quoted-printable") { 
 require MIME::QuotedPrint;
 $content_ref = \MIME::QuotedPrint::decode($$content_ref);
 $content_ref_iscopy++;
 }
 else {
 die "Don't know how to decode Content-Encoding '$ce'";
 }
 }
 }

if ($self->content_is_text || $self->content_is_xml) {
 my $charset = lc(
 $opt{charset} ||
 $self->content_type_charset ||
 $opt{default_charset} ||
 $self->content_charset ||
 "ISO-8859-1"
 );
 unless ($charset =~ /^(?:none|us-ascii|iso-8859-1)\z/) {
 require Encode;
 if (do{my $v = $Encode::VERSION; $v =~ s/_//g; $v} < 2.0901 &&
 !$content_ref_iscopy)
 {
 my $copy = $$content_ref;
 $content_ref = \$copy;
 $content_ref_iscopy++;
 }
 $content_ref = \Encode::decode($charset, $$content_ref,
 ($opt{charset_strict} ? Encode::FB_CROAK() : 0) | Encode::LEAVE_SRC());
 die "Encode::decode() returned undef improperly" unless defined $$content_ref;
 }
 }
 };
 if ($@) {
 Carp::croak($@) if $opt{raise_error};
 return undef;
 }

return $opt{ref} ? $content_ref : $$content_ref;
}
sub decodable
{
 my $self = shift;
 my @enc;
 eval {
 require IO::Uncompress::Gunzip;
 push(@enc, "gzip", "x-gzip");
 };
 eval {
 require IO::Uncompress::Inflate;
 require IO::Uncompress::RawInflate;
 push(@enc, "deflate");
 };
 eval {
 require IO::Uncompress::Bunzip2;
 push(@enc, "x-bzip2");
 };
 return wantarray ? @enc : join(", ", @enc);
}
sub decode
{
 my $self = shift;
 return 1 unless $self->header("Content-Encoding");
 if (defined(my $content = $self->decoded_content(charset => "none"))) {
 $self->remove_header("Content-Encoding", "Content-Length", "Content-MD5");
 $self->content($content);
 return 1;
 }
 return 0;
}
sub encode
{
 my($self, @enc) = @_;

Carp::croak("Can't encode multipart/* messages") if $self->content_type =~ m,^multipart/,;
 Carp::croak("Can't encode message/* messages") if $self->content_type =~ m,^message/,;

return 1 unless @enc; 

my $content = $self->content;
 for my $encoding (@enc) {
 if ($encoding eq "identity") {
 }
 elsif ($encoding eq "base64") {
 require MIME::Base64;
 $content = MIME::Base64::encode($content);
 }
 elsif ($encoding eq "gzip" || $encoding eq "x-gzip") {
 require IO::Compress::Gzip;
 my $output;
 IO::Compress::Gzip::gzip(\$content, \$output, Minimal => 1)
 or die "Can't gzip content: $IO::Compress::Gzip::GzipError";
 $content = $output;
 }
 elsif ($encoding eq "deflate") {
 require IO::Compress::Deflate;
 my $output;
 IO::Compress::Deflate::deflate(\$content, \$output)
 or die "Can't deflate content: $IO::Compress::Deflate::DeflateError";
 $content = $output;
 }
 elsif ($encoding eq "x-bzip2") {
 require IO::Compress::Bzip2;
 my $output;
 IO::Compress::Bzip2::bzip2(\$content, \$output)
 or die "Can't bzip2 content: $IO::Compress::Bzip2::Bzip2Error";
 $content = $output;
 }
 elsif ($encoding eq "rot13") { 
 $content =~ tr/A-Za-z/N-ZA-Mn-za-m/;
 }
 else {
 return 0;
 }
 }
 my $h = $self->header("Content-Encoding");
 unshift(@enc, $h) if $h;
 $self->header("Content-Encoding", join(", ", @enc));
 $self->remove_header("Content-Length", "Content-MD5");
 $self->content($content);
 return 1;
}
sub as_string
{
 my($self, $eol) = @_;
 $eol = "\n" unless defined $eol;
 my $content = $self->content;

return join("", $self->{'_headers'}->as_string($eol),
 $eol,
 $content,
 (@_ == 1 && length($content) &&
 $content !~ /\n\z/) ? "\n" : "",
 );
}
sub dump
{
 my($self, %opt) = @_;
 my $content = $self->content;
 my $chopped = 0;
 if (!ref($content)) {
 my $maxlen = $opt{maxlength};
 $maxlen = 512 unless defined($maxlen);
 if ($maxlen && length($content) > $maxlen * 1.1 + 3) {
 $chopped = length($content) - $maxlen;
 $content = substr($content, 0, $maxlen) . "...";
 }

$content =~ s/\\/\\\\/g;
 $content =~ s/\t/\\t/g;
 $content =~ s/\r/\\r/g;
 $content =~ s/([\0-\11\13-\037])(?!\d)/sprintf('\\%o',ord($1))/eg;

$content =~ s/([\0-\11\13-\037\177-\377])/sprintf('\\x%02X',ord($1))/eg;
 $content =~ s/([^\12\040-\176])/sprintf('\\x{%X}',ord($1))/eg;
 $content =~ s/( +)\n/("\\40" x length($1)) . "\n"/eg;
 $content =~ s/(\n+)\n/("\\n" x length($1)) . "\n"/eg;
 $content =~ s/\n\z/\\n/;

my $no_content = "(no content)";
 if ($content eq $no_content) {
 $content =~ s/^(.)/sprintf('\\x%02X',ord($1))/eg;
 }
 elsif ($content eq "") {
 $content = "(no content)";
 }
 }

my @dump;
 push(@dump, $opt{preheader}) if $opt{preheader};
 push(@dump, $self->{_headers}->as_string, $content);
 push(@dump, "(+ $chopped more bytes not shown)") if $chopped;

my $dump = join("\n", @dump, "");
 $dump =~ s/^/$opt{prefix}/gm if $opt{prefix};

print $dump unless defined wantarray;
 return $dump;
}
sub parts {
 my $self = shift;
 if (defined(wantarray) && (!exists $self->{_parts} || ref($self->{_content}) eq "SCALAR")) {
 $self->_parts;
 }
 my $old = $self->{_parts};
 if (@_) {
 my @parts = map { ref($_) eq 'ARRAY' ? @$_ : $_ } @_;
 my $ct = $self->content_type || "";
 if ($ct =~ m,^message/,) {
 Carp::croak("Only one part allowed for $ct content")
 if @parts > 1;
 }
 elsif ($ct !~ m,^multipart/,) {
 $self->remove_content_headers;
 $self->content_type("multipart/mixed");
 }
 $self->{_parts} = \@parts;
 _stale_content($self);
 }
 return @$old if wantarray;
 return $old->[0];
}
sub add_part {
 my $self = shift;
 if (($self->content_type || "") !~ m,^multipart/,) {
 my $p = HTTP::Message->new($self->remove_content_headers,
 $self->content(""));
 $self->content_type("multipart/mixed");
 $self->{_parts} = [];
 if ($p->headers->header_field_names || $p->content ne "") {
 push(@{$self->{_parts}}, $p);
 }
 }
 elsif (!exists $self->{_parts} || ref($self->{_content}) eq "SCALAR") {
 $self->_parts;
 }

push(@{$self->{_parts}}, @_);
 _stale_content($self);
 return;
}
sub _stale_content {
 my $self = shift;
 if (ref($self->{_content}) eq "SCALAR") {
 $self->_content;
 }
 else {
 delete $self->{_content};
 delete $self->{_content_ref};
 }
}
sub AUTOLOAD
{
 my $method = substr($AUTOLOAD, rindex($AUTOLOAD, '::')+2);
 no strict 'refs';
 *$method = sub { shift->headers->$method(@_) };
 goto &$method;
}
sub DESTROY {} 
sub _elem
{
 my $self = shift;
 my $elem = shift;
 my $old = $self->{$elem};
 $self->{$elem} = $_[0] if @_;
 return $old;
}
sub _parts {
 my $self = shift;
 my $ct = $self->content_type;
 if ($ct =~ m,^multipart/,) {
 require HTTP::Headers::Util;
 my @h = HTTP::Headers::Util::split_header_words($self->header("Content-Type"));
 die "Assert" unless @h;
 my %h = @{$h[0]};
 if (defined(my $b = $h{boundary})) {
 my $str = $self->content;
 $str =~ s/\r?\n--\Q$b\E--\r?\n.*//s;
 if ($str =~ s/(^|.*?\r?\n)--\Q$b\E\r?\n//s) {
 $self->{_parts} = [map HTTP::Message->parse($_),
 split(/\r?\n--\Q$b\E\r?\n/, $str)]
 }
 }
 }
 elsif ($ct eq "message/http") {
 require HTTP::Request;
 require HTTP::Response;
 my $content = $self->content;
 my $class = ($content =~ m,^(HTTP/.*)\n,) ?
 "HTTP::Response" : "HTTP::Request";
 $self->{_parts} = [$class->parse($content)];
 }
 elsif ($ct =~ m,^message/,) {
 $self->{_parts} = [ HTTP::Message->parse($self->content) ];
 }

$self->{_parts} ||= [];
}
sub _content {
 my $self = shift;
 my $ct = $self->{_headers}->header("Content-Type") || "multipart/mixed";
 if ($ct =~ m,^\s*message/,i) {
 _set_content($self, $self->{_parts}[0]->as_string($CRLF), 1);
 return;
 }

require HTTP::Headers::Util;
 my @v = HTTP::Headers::Util::split_header_words($ct);
 Carp::carp("Multiple Content-Type headers") if @v > 1;
 @v = @{$v[0]};

my $boundary;
 my $boundary_index;
 for (my @tmp = @v; @tmp;) {
 my($k, $v) = splice(@tmp, 0, 2);
 if ($k eq "boundary") {
 $boundary = $v;
 $boundary_index = @v - @tmp - 1;
 last;
 }
 }

my @parts = map $_->as_string($CRLF), @{$self->{_parts}};

my $bno = 0;
 $boundary = _boundary() unless defined $boundary;
 CHECK_BOUNDARY:
 {
 for (@parts) {
 if (index($_, $boundary) >= 0) {
 $boundary = _boundary(++$bno);
 redo CHECK_BOUNDARY;
 }
 }
 }

if ($boundary_index) {
 $v[$boundary_index] = $boundary;
 }
 else {
 push(@v, boundary => $boundary);
 }

$ct = HTTP::Headers::Util::join_header_words(@v);
 $self->{_headers}->header("Content-Type", $ct);

_set_content($self, "--$boundary$CRLF" .
 join("$CRLF--$boundary$CRLF", @parts) .
 "$CRLF--$boundary--$CRLF",
 1);
}
sub _boundary
{
 my $size = shift || return "xYzZY";
 require MIME::Base64;
 my $b = MIME::Base64::encode(join("", map chr(rand(256)), 1..$size*3), "");
 $b =~ s/[\W]/X/g; 
 $b;
}
1;
}
BEGIN { $INC{q{HTTP/Request.pm}} = 1;
package HTTP::Request;
require HTTP::Message;
@ISA = qw(HTTP::Message);
$VERSION = "5.827";
use strict;
sub new
{
 my($class, $method, $uri, $header, $content) = @_;
 my $self = $class->SUPER::new($header, $content);
 $self->method($method);
 $self->uri($uri);
 $self;
}
sub parse
{
 my($class, $str) = @_;
 my $request_line;
 if ($str =~ s/^(.*)\n//) {
 $request_line = $1;
 }
 else {
 $request_line = $str;
 $str = "";
 }

my $self = $class->SUPER::parse($str);
 my($method, $uri, $protocol) = split(' ', $request_line);
 $self->method($method) if defined($method);
 $self->uri($uri) if defined($uri);
 $self->protocol($protocol) if $protocol;
 $self;
}
sub clone
{
 my $self = shift;
 my $clone = bless $self->SUPER::clone, ref($self);
 $clone->method($self->method);
 $clone->uri($self->uri);
 $clone;
}
sub method
{
 shift->_elem('_method', @_);
}
sub uri
{
 my $self = shift;
 my $old = $self->{'_uri'};
 if (@_) {
 my $uri = shift;
 if (!defined $uri) {
 }
 elsif (ref $uri) {
 Carp::croak("A URI can't be a " . ref($uri) . " reference")
 if ref($uri) eq 'HASH' or ref($uri) eq 'ARRAY';
 Carp::croak("Can't use a " . ref($uri) . " object as a URI")
 unless $uri->can('scheme');
 $uri = $uri->clone;
 unless ($HTTP::URI_CLASS eq "URI") {
 eval { local $SIG{__DIE__}; $uri = $uri->abs; };
 die $@ if $@ && $@ !~ /Missing base argument/;
 }
 }
 else {
 $uri = $HTTP::URI_CLASS->new($uri);
 }
 $self->{'_uri'} = $uri;
 delete $self->{'_uri_canonical'};
 }
 $old;
}
*url = \&uri; 
sub uri_canonical
{
 my $self = shift;
 return $self->{'_uri_canonical'} ||= $self->{'_uri'}->canonical;
}
sub accept_decodable
{
 my $self = shift;
 $self->header("Accept-Encoding", scalar($self->decodable));
}
sub as_string
{
 my $self = shift;
 my($eol) = @_;
 $eol = "\n" unless defined $eol;

my $req_line = $self->method || "-";
 my $uri = $self->uri;
 $uri = (defined $uri) ? $uri->as_string : "-";
 $req_line .= " $uri";
 my $proto = $self->protocol;
 $req_line .= " $proto" if $proto;

return join($eol, $req_line, $self->SUPER::as_string(@_));
}
sub dump
{
 my $self = shift;
 my @pre = ($self->method || "-", $self->uri || "-");
 if (my $prot = $self->protocol) {
 push(@pre, $prot);
 }

return $self->SUPER::dump(
 preheader => join(" ", @pre),
 @_,
 );
}
1;
}
BEGIN { $INC{q{HTTP/Response.pm}} = 1;
package HTTP::Response;
require HTTP::Message;
@ISA = qw(HTTP::Message);
$VERSION = "5.824";
use strict;
use HTTP::Status ();
sub new
{
 my($class, $rc, $msg, $header, $content) = @_;
 my $self = $class->SUPER::new($header, $content);
 $self->code($rc);
 $self->message($msg);
 $self;
}
sub parse
{
 my($class, $str) = @_;
 my $status_line;
 if ($str =~ s/^(.*)\n//) {
 $status_line = $1;
 }
 else {
 $status_line = $str;
 $str = "";
 }

my $self = $class->SUPER::parse($str);
 my($protocol, $code, $message);
 if ($status_line =~ /^\d{3} /) {
 ($code, $message) = split(' ', $status_line, 2);
 } else {
 ($protocol, $code, $message) = split(' ', $status_line, 3);
 }
 $self->protocol($protocol) if $protocol;
 $self->code($code) if defined($code);
 $self->message($message) if defined($message);
 $self;
}
sub clone
{
 my $self = shift;
 my $clone = bless $self->SUPER::clone, ref($self);
 $clone->code($self->code);
 $clone->message($self->message);
 $clone->request($self->request->clone) if $self->request;
 $clone;
}
sub code { shift->_elem('_rc', @_); }
sub message { shift->_elem('_msg', @_); }
sub previous { shift->_elem('_previous',@_); }
sub request { shift->_elem('_request', @_); }
sub status_line
{
 my $self = shift;
 my $code = $self->{'_rc'} || "000";
 my $mess = $self->{'_msg'} || HTTP::Status::status_message($code) || "Unknown code";
 return "$code $mess";
}
sub base
{
 my $self = shift;
 my $base = $self->header('Content-Base') || 
 $self->header('Content-Location') || 
 $self->header('Base'); 
 if ($base && $base =~ /^$URI::scheme_re:/o) {
 return $HTTP::URI_CLASS->new($base);
 }

my $req = $self->request;
 if ($req) {
 return $HTTP::URI_CLASS->new_abs($base, $req->uri);
 }
 return undef;
}
sub redirects {
 my $self = shift;
 my @r;
 my $r = $self;
 while (my $p = $r->previous) {
 push(@r, $p);
 $r = $p;
 }
 return @r unless wantarray;
 return reverse @r;
}
sub filename
{
 my $self = shift;
 my $file;

my $cd = $self->header('Content-Disposition');
 if ($cd) {
 require HTTP::Headers::Util;
 if (my @cd = HTTP::Headers::Util::split_header_words($cd)) {
 my ($disposition, undef, %cd_param) = @{$cd[-1]};
 $file = $cd_param{filename};
 if ($file && $file =~ /^=\?(.+?)\?(.+?)\?(.+)\?=$/) {
 my $charset = $1;
 my $encoding = uc($2);
 my $encfile = $3;

if ($encoding eq 'Q' || $encoding eq 'B') {
 local($SIG{__DIE__});
 eval {
 if ($encoding eq 'Q') {
 $encfile =~ s/_/ /g;
 require MIME::QuotedPrint;
 $encfile = MIME::QuotedPrint::decode($encfile);
 }
 else { 
 require MIME::Base64;
 $encfile = MIME::Base64::decode($encfile);
 }

require Encode;
 require encoding;
 my $locale_charset = encoding::_get_locale_encoding();
 Encode::from_to($encfile, $charset, $locale_charset);
 };

$file = $encfile unless $@;
 }
 }
 }
 }

my $uri;
 unless (defined($file) && length($file)) {
 if (my $cl = $self->header('Content-Location')) {
 $uri = URI->new($cl);
 }
 elsif (my $request = $self->request) {
 $uri = $request->uri;
 }

if ($uri) {
 $file = ($uri->path_segments)[-1];
 }
 }

if ($file) {
 $file =~ s,.*[\\/],,; 
 }

if ($file && !length($file)) {
 $file = undef;
 }

$file;
}
sub as_string
{
 require HTTP::Status;
 my $self = shift;
 my($eol) = @_;
 $eol = "\n" unless defined $eol;

my $status_line = $self->status_line;
 my $proto = $self->protocol;
 $status_line = "$proto $status_line" if $proto;

return join($eol, $status_line, $self->SUPER::as_string(@_));
}
sub dump
{
 my $self = shift;

my $status_line = $self->status_line;
 my $proto = $self->protocol;
 $status_line = "$proto $status_line" if $proto;

return $self->SUPER::dump(
 preheader => $status_line,
 @_,
 );
}
sub is_info { HTTP::Status::is_info (shift->{'_rc'}); }
sub is_success { HTTP::Status::is_success (shift->{'_rc'}); }
sub is_redirect { HTTP::Status::is_redirect (shift->{'_rc'}); }
sub is_error { HTTP::Status::is_error (shift->{'_rc'}); }
sub error_as_HTML
{
 require HTML::Entities;
 my $self = shift;
 my $title = 'An Error Occurred';
 my $body = HTML::Entities::encode($self->status_line);
 return <<EOM;
<html>
<head><title>$title</title></head>
<body>
<h1>$title</h1>
<p>$body</p>
</body>
</html>
EOM
}
sub current_age
{
 my $self = shift;
 my $time = shift;
 my $response_time = $self->client_date;
 my $date = $self->date;

my $age = 0;
 if ($response_time && $date) {
 $age = $response_time - $date; 
 $age = 0 if $age < 0;
 }

my $age_v = $self->header('Age');
 if ($age_v && $age_v > $age) {
 $age = $age_v; 
 }

if ($response_time) {
 my $request = $self->request;
 if ($request) {
 my $request_time = $request->date;
 if ($request_time && $request_time < $response_time) {
 $age += $response_time - $request_time;
 }
 }
 $age += ($time || time) - $response_time;
 }
 return $age;
}
sub freshness_lifetime
{
 my($self, %opt) = @_;
 for my $cc ($self->header('Cache-Control')) {
 for my $cc_dir (split(/\s*,\s*/, $cc)) {
 return $1 if $cc_dir =~ /^max-age\s*=\s*(\d+)/i;
 }
 }
 my $date = $self->date || $self->client_date || $opt{time} || time;
 if (my $expires = $self->expires) {
 return $expires - $date;
 }
 return undef if exists $opt{heuristic_expiry} && !$opt{heuristic_expiry};
 $opt{h_min} ||= 60;
 $opt{h_max} ||= 24 * 3600;
 $opt{h_lastmod_fraction} ||= 0.10; 
 $opt{h_default} ||= 3600;

if (my $last_modified = $self->last_modified) {
 my $h_exp = ($date - $last_modified) * $opt{h_lastmod_fraction};
 return $opt{h_min} if $h_exp < $opt{h_min};
 return $opt{h_max} if $h_exp > $opt{h_max};
 return $h_exp;
 }
 return $opt{h_min} if $opt{h_min} > $opt{h_default};
 return $opt{h_default};
}
sub is_fresh
{
 my($self, %opt) = @_;
 $opt{time} ||= time;
 my $f = $self->freshness_lifetime(%opt);
 return undef unless defined($f);
 return $f > $self->current_age($opt{time});
}
sub fresh_until
{
 my($self, %opt) = @_;
 $opt{time} ||= time;
 my $f = $self->freshness_lifetime(%opt);
 return undef unless defined($f);
 return $f - $self->current_age($opt{time}) + $opt{time};
}
1;
}
BEGIN { $INC{q{LWP/MediaTypes.pm}} = 1;
package LWP::MediaTypes;
require Exporter;
@ISA = qw(Exporter);
@EXPORT = qw(guess_media_type media_suffix);
@EXPORT_OK = qw(add_type add_encoding read_media_types);
$VERSION = "5.822";
use strict;
my %suffixType = (
 'txt' => 'text/plain',
 'html' => 'text/html',
 'gif' => 'image/gif',
 'jpg' => 'image/jpeg',
 'xml' => 'text/xml',
);
my %suffixExt = (
 'text/plain' => 'txt',
 'text/html' => 'html',
 'image/gif' => 'gif',
 'image/jpeg' => 'jpg',
 'text/xml' => 'xml',
);
my %suffixEncoding = (
 'Z' => 'compress',
 'gz' => 'gzip',
 'hqx' => 'x-hqx',
 'uu' => 'x-uuencode',
 'z' => 'x-pack',
 'bz2' => 'x-bzip2',
);
read_media_types();
sub _dump {
 require Data::Dumper;
 Data::Dumper->new([\%suffixType, \%suffixExt, \%suffixEncoding],
 [qw(*suffixType *suffixExt *suffixEncoding)])->Dump;
}
sub guess_media_type
{
 my($file, $header) = @_;
 return undef unless defined $file;

my $fullname;
 if (ref($file)) {
 $file = $file->path;
 }
 else {
 $fullname = $file; 
 }

my @encoding = ();
 my $ct = undef;
 for (file_exts($file)) {
 if (exists $suffixEncoding{$_}) {
 unshift(@encoding, $suffixEncoding{$_});
 next;
 }
 if (exists $suffixEncoding{lc $_}) {
 unshift(@encoding, $suffixEncoding{lc $_});
 next;
 }
 if (exists $suffixType{$_}) {
 $ct = $suffixType{$_};
 last;
 }
 if (exists $suffixType{lc $_}) {
 $ct = $suffixType{lc $_};
 last;
 }
 last;
 }
 unless (defined $ct) {
 if (defined $fullname) {
 $ct = (-T $fullname) ? "text/plain" : "application/octet-stream";
 }
 else {
 $ct = "application/octet-stream";
 }
 }

if ($header) {
 $header->header('Content-Type' => $ct);
 $header->header('Content-Encoding' => \@encoding) if @encoding;
 }

wantarray ? ($ct, @encoding) : $ct;
}
sub media_suffix {
 if (!wantarray && @_ == 1 && $_[0] !~ /\*/) {
 return $suffixExt{$_[0]};
 }
 my(@type) = @_;
 my(@suffix, $ext, $type);
 foreach (@type) {
 if (s/\*/.*/) {
 while(($ext,$type) = each(%suffixType)) {
 push(@suffix, $ext) if $type =~ /^$_$/;
 }
 }
 else {
 while(($ext,$type) = each(%suffixType)) {
 push(@suffix, $ext) if $type eq $_;
 }
 }
 }
 wantarray ? @suffix : $suffix[0];
}
sub file_exts
{
 require File::Basename;
 my @parts = reverse split(/\./, File::Basename::basename($_[0]));
 pop(@parts); 
 @parts;
}
sub add_type
{
 my($type, @exts) = @_;
 for my $ext (@exts) {
 $ext =~ s/^\.//;
 $suffixType{$ext} = $type;
 }
 $suffixExt{$type} = $exts[0] if @exts;
}
sub add_encoding
{
 my($type, @exts) = @_;
 for my $ext (@exts) {
 $ext =~ s/^\.//;
 $suffixEncoding{$ext} = $type;
 }
}
sub read_media_types
{
 my(@files) = @_;

local($/, $_) = ("\n", undef); 

my @priv_files = ();
 if($^O eq "MacOS") {
 push(@priv_files, "$ENV{HOME}:media.types", "$ENV{HOME}:mime.types")
 if defined $ENV{HOME}; 
 }
 else {
 push(@priv_files, "$ENV{HOME}/.media.types", "$ENV{HOME}/.mime.types")
 if defined $ENV{HOME}; 
 }
 my $typefile;
 unless (@files) {
 if($^O eq "MacOS") {
 @files = map {$_."LWP:media.types"} @INC;
 }
 else {
 @files = map {"$_/LWP/media.types"} @INC;
 }
 push @files, @priv_files;
 }
 for $typefile (@files) {
 local(*TYPE);
 open(TYPE, $typefile) || next;
 while (<TYPE>) {
 next if /^\s*#/; 
 next if /^\s*$/; 
 s/#.*//; 
 my($type, @exts) = split(' ', $_);
 add_type($type, @exts);
 }
 close(TYPE);
 }
}
1;
}
BEGIN { $INC{q{HTTP/Daemon.pm}} = 1;
package HTTP::Daemon;
use strict;
use vars qw($VERSION @ISA $PROTO $DEBUG);
$VERSION = "5.827";
use IO::Socket qw(AF_INET INADDR_ANY inet_ntoa);
@ISA=qw(IO::Socket::INET);
$PROTO = "HTTP/1.1";
sub new
{
 my($class, %args) = @_;
 $args{Listen} ||= 5;
 $args{Proto} ||= 'tcp';
 return $class->SUPER::new(%args);
}
sub accept
{
 my $self = shift;
 my $pkg = shift || "HTTP::Daemon::ClientConn";
 my ($sock, $peer) = $self->SUPER::accept($pkg);
 if ($sock) {
 ${*$sock}{'httpd_daemon'} = $self;
 return wantarray ? ($sock, $peer) : $sock;
 }
 else {
 return;
 }
}
sub url
{
 my $self = shift;
 my $url = $self->_default_scheme . "://";
 my $addr = $self->sockaddr;
 if (!$addr || $addr eq INADDR_ANY) {
 require Sys::Hostname;
 $url .= lc Sys::Hostname::hostname();
 }
 else {
 $url .= gethostbyaddr($addr, AF_INET) || inet_ntoa($addr);
 }
 my $port = $self->sockport;
 $url .= ":$port" if $port != $self->_default_port;
 $url .= "/";
 $url;
}
sub _default_port {
 80;
}
sub _default_scheme {
 "http";
}
sub product_tokens
{
 "libwww-perl-daemon/$HTTP::Daemon::VERSION";
}
package HTTP::Daemon::ClientConn;
use vars qw(@ISA $DEBUG);
use IO::Socket ();
@ISA=qw(IO::Socket::INET);
*DEBUG = \$HTTP::Daemon::DEBUG;
use HTTP::Request ();
use HTTP::Response ();
use HTTP::Status;
use HTTP::Date qw(time2str);
use LWP::MediaTypes qw(guess_media_type);
use Carp ();
my $CRLF = "\015\012"; 
my $HTTP_1_0 = _http_version("HTTP/1.0");
my $HTTP_1_1 = _http_version("HTTP/1.1");
sub get_request
{
 my($self, $only_headers) = @_;
 if (${*$self}{'httpd_nomore'}) {
 $self->reason("No more requests from this connection");
 return;
 }

$self->reason("");
 my $buf = ${*$self}{'httpd_rbuf'};
 $buf = "" unless defined $buf;

my $timeout = $ {*$self}{'io_socket_timeout'};
 my $fdset = "";
 vec($fdset, $self->fileno, 1) = 1;
 local($_);

READ_HEADER:
 while (1) {
 $buf =~ s/^(?:\015?\012)+//; 
 if ($buf =~ /\012/) { 
 if ($buf =~ /^\w+[^\012]+HTTP\/\d+\.\d+\015?\012/) {
 if ($buf =~ /\015?\012\015?\012/) {
 last READ_HEADER; 
 }
 elsif (length($buf) > 16*1024) {
 $self->send_error(413); 
 $self->reason("Very long header");
 return;
 }
 }
 else {
 last READ_HEADER; 
 }
 }
 elsif (length($buf) > 16*1024) {
 $self->send_error(414); 
 $self->reason("Very long first line");
 return;
 }
 print STDERR "Need more data for complete header\n" if $DEBUG;
 return unless $self->_need_more($buf, $timeout, $fdset);
 }
 if ($buf !~ s/^(\S+)[ \t]+(\S+)(?:[ \t]+(HTTP\/\d+\.\d+))?[^\012]*\012//) {
 ${*$self}{'httpd_client_proto'} = _http_version("HTTP/1.0");
 $self->send_error(400); 
 $self->reason("Bad request line: $buf");
 return;
 }
 my $method = $1;
 my $uri = $2;
 my $proto = $3 || "HTTP/0.9";
 $uri = "http://$uri" if $method eq "CONNECT";
 $uri = $HTTP::URI_CLASS->new($uri, $self->daemon->url);
 my $r = HTTP::Request->new($method, $uri);
 $r->protocol($proto);
 ${*$self}{'httpd_client_proto'} = $proto = _http_version($proto);
 ${*$self}{'httpd_head'} = ($method eq "HEAD");

if ($proto >= $HTTP_1_0) {
 my($key, $val);
 HEADER:
 while ($buf =~ s/^([^\012]*)\012//) {
 $_ = $1;
 s/\015$//;
 if (/^([^:\s]+)\s*:\s*(.*)/) {
 $r->push_header($key, $val) if $key;
 ($key, $val) = ($1, $2);
 }
 elsif (/^\s+(.*)/) {
 $val .= " $1";
 }
 else {
 last HEADER;
 }
 }
 $r->push_header($key, $val) if $key;
 }

my $conn = $r->header('Connection');
 if ($proto >= $HTTP_1_1) {
 ${*$self}{'httpd_nomore'}++ if $conn && lc($conn) =~ /\bclose\b/;
 }
 else {
 ${*$self}{'httpd_nomore'}++ unless $conn &&
 lc($conn) =~ /\bkeep-alive\b/;
 }

if ($only_headers) {
 ${*$self}{'httpd_rbuf'} = $buf;
 return $r;
 }
 my $te = $r->header('Transfer-Encoding');
 my $ct = $r->header('Content-Type');
 my $len = $r->header('Content-Length');
 for my $e ( $r->header('Expect') ) {
 if( lc($e) eq '100-continue' ) {
 $self->send_status_line(100);
 $self->send_crlf;
 }
 else {
 $self->send_error(417);
 $self->reason("Unsupported Expect header value");
 return;
 }
 }

if ($te && lc($te) eq 'chunked') {
 my $body = "";
 CHUNK:
 while (1) {
 print STDERR "Chunked\n" if $DEBUG;
 if ($buf =~ s/^([^\012]*)\012//) {
 my $chunk_head = $1;
 unless ($chunk_head =~ /^([0-9A-Fa-f]+)/) {
 $self->send_error(400);
 $self->reason("Bad chunk header $chunk_head");
 return;
 }
 my $size = hex($1);
 last CHUNK if $size == 0;

my $missing = $size - length($buf) + 2; 
 while ($missing > 0) {
 print STDERR "Need $missing more bytes\n" if $DEBUG;
 my $n = $self->_need_more($buf, $timeout, $fdset);
 return unless $n;
 $missing -= $n;
 }
 $body .= substr($buf, 0, $size);
 substr($buf, 0, $size+2) = '';

}
 else {
 return unless $self->_need_more($buf, $timeout, $fdset);
 }
 }
 $r->content($body);
 $r->remove_header('Transfer-Encoding');
 $r->header('Content-Length', length($body));

my($key, $val);
 FOOTER:
 while (1) {
 if ($buf !~ /\012/) {
 return unless $self->_need_more($buf, $timeout, $fdset);
 }
 else {
 $buf =~ s/^([^\012]*)\012//;
 $_ = $1;
 s/\015$//;
 if (/^([\w\-]+)\s*:\s*(.*)/) {
 $r->push_header($key, $val) if $key;
 ($key, $val) = ($1, $2);
 }
 elsif (/^\s+(.*)/) {
 $val .= " $1";
 }
 elsif (!length) {
 last FOOTER;
 }
 else {
 $self->reason("Bad footer syntax");
 return;
 }
 }
 }
 $r->push_header($key, $val) if $key;

}
 elsif ($te) {
 $self->send_error(501); 
 $self->reason("Unknown transfer encoding '$te'");
 return;

}
 elsif ($len) {
 my $missing = $len - length($buf);
 while ($missing > 0) {
 print "Need $missing more bytes of content\n" if $DEBUG;
 my $n = $self->_need_more($buf, $timeout, $fdset);
 return unless $n;
 $missing -= $n;
 }
 if (length($buf) > $len) {
 $r->content(substr($buf,0,$len));
 substr($buf, 0, $len) = '';
 }
 else {
 $r->content($buf);
 $buf='';
 }
 }
 elsif ($ct && $ct =~ m/^multipart\/\w+\s*;.*boundary\s*=\s*("?)(\w+)\1/i) {
 my $boundary = "$CRLF--$2--";
 my $index;
 while (1) {
 $index = index($buf, $boundary);
 last if $index >= 0;
 return unless $self->_need_more($buf, $timeout, $fdset);
 }
 $index += length($boundary);
 $r->content(substr($buf, 0, $index));
 substr($buf, 0, $index) = '';

}
 ${*$self}{'httpd_rbuf'} = $buf;

$r;
}
sub _need_more
{
 my $self = shift;
 if ($_[1]) {
 my($timeout, $fdset) = @_[1,2];
 print STDERR "select(,,,$timeout)\n" if $DEBUG;
 my $n = select($fdset,undef,undef,$timeout);
 unless ($n) {
 $self->reason(defined($n) ? "Timeout" : "select: $!");
 return;
 }
 }
 print STDERR "sysread()\n" if $DEBUG;
 my $n = sysread($self, $_[0], 2048, length($_[0]));
 $self->reason(defined($n) ? "Client closed" : "sysread: $!") unless $n;
 $n;
}
sub read_buffer
{
 my $self = shift;
 my $old = ${*$self}{'httpd_rbuf'};
 if (@_) {
 ${*$self}{'httpd_rbuf'} = shift;
 }
 $old;
}
sub reason
{
 my $self = shift;
 my $old = ${*$self}{'httpd_reason'};
 if (@_) {
 ${*$self}{'httpd_reason'} = shift;
 }
 $old;
}
sub proto_ge
{
 my $self = shift;
 ${*$self}{'httpd_client_proto'} >= _http_version(shift);
}
sub _http_version
{
 local($_) = shift;
 return 0 unless m,^(?:HTTP/)?(\d+)\.(\d+)$,i;
 $1 * 1000 + $2;
}
sub antique_client
{
 my $self = shift;
 ${*$self}{'httpd_client_proto'} < $HTTP_1_0;
}
sub force_last_request
{
 my $self = shift;
 ${*$self}{'httpd_nomore'}++;
}
sub head_request
{
 my $self = shift;
 ${*$self}{'httpd_head'};
}
sub send_status_line
{
 my($self, $status, $message, $proto) = @_;
 return if $self->antique_client;
 $status ||= RC_OK;
 $message ||= status_message($status) || "";
 $proto ||= $HTTP::Daemon::PROTO || "HTTP/1.1";
 print $self "$proto $status $message$CRLF";
}
sub send_crlf
{
 my $self = shift;
 print $self $CRLF;
}
sub send_basic_header
{
 my $self = shift;
 return if $self->antique_client;
 $self->send_status_line(@_);
 print $self "Date: ", time2str(time), $CRLF;
 my $product = $self->daemon->product_tokens;
 print $self "Server: $product$CRLF" if $product;
}
sub send_header
{
 my $self = shift;
 while (@_) {
 my($k, $v) = splice(@_, 0, 2);
 $v = "" unless defined($v);
 print $self "$k: $v$CRLF";
 }
}
sub send_response
{
 my $self = shift;
 my $res = shift;
 if (!ref $res) {
 $res ||= RC_OK;
 $res = HTTP::Response->new($res, @_);
 }
 my $content = $res->content;
 my $chunked;
 unless ($self->antique_client) {
 my $code = $res->code;
 $self->send_basic_header($code, $res->message, $res->protocol);
 if ($code =~ /^(1\d\d|[23]04)$/) {
 $res->remove_header("Content-Length");
 $content = "";
 }
 elsif ($res->request && $res->request->method eq "HEAD") {
 }
 elsif (ref($content) eq "CODE") {
 if ($self->proto_ge("HTTP/1.1")) {
 $res->push_header("Transfer-Encoding" => "chunked");
 $chunked++;
 }
 else {
 $self->force_last_request;
 }
 }
 elsif (length($content)) {
 $res->header("Content-Length" => length($content));
 }
 else {
 $self->force_last_request;
 $res->header('connection','close');
 }
 print $self $res->headers_as_string($CRLF);
 print $self $CRLF; 
 }
 if ($self->head_request) {
 }
 elsif (ref($content) eq "CODE") {
 while (1) {
 my $chunk = &$content();
 last unless defined($chunk) && length($chunk);
 if ($chunked) {
 printf $self "%x%s%s%s", length($chunk), $CRLF, $chunk, $CRLF;
 }
 else {
 print $self $chunk;
 }
 }
 print $self "0$CRLF$CRLF" if $chunked; 
 }
 elsif (length $content) {
 print $self $content;
 }
}
sub send_redirect
{
 my($self, $loc, $status, $content) = @_;
 $status ||= RC_MOVED_PERMANENTLY;
 Carp::croak("Status '$status' is not redirect") unless is_redirect($status);
 $self->send_basic_header($status);
 my $base = $self->daemon->url;
 $loc = $HTTP::URI_CLASS->new($loc, $base) unless ref($loc);
 $loc = $loc->abs($base);
 print $self "Location: $loc$CRLF";
 if ($content) {
 my $ct = $content =~ /^\s*</ ? "text/html" : "text/plain";
 print $self "Content-Type: $ct$CRLF";
 }
 print $self $CRLF;
 print $self $content if $content && !$self->head_request;
 $self->force_last_request; 
}
sub send_error
{
 my($self, $status, $error) = @_;
 $status ||= RC_BAD_REQUEST;
 Carp::croak("Status '$status' is not an error") unless is_error($status);
 my $mess = status_message($status);
 $error ||= "";
 $mess = <<EOT;
<title>$status $mess</title>
<h1>$status $mess</h1>
$error
EOT
 unless ($self->antique_client) {
 $self->send_basic_header($status);
 print $self "Content-Type: text/html$CRLF";
 print $self "Content-Length: " . length($mess) . $CRLF;
 print $self $CRLF;
 }
 print $self $mess unless $self->head_request;
 $status;
}
sub send_file_response
{
 my($self, $file) = @_;
 if (-d $file) {
 $self->send_dir($file);
 }
 elsif (-f _) {
 local(*F);
 sysopen(F, $file, 0) or
 return $self->send_error(RC_FORBIDDEN);
 binmode(F);
 my($ct,$ce) = guess_media_type($file);
 my($size,$mtime) = (stat _)[7,9];
 unless ($self->antique_client) {
 $self->send_basic_header;
 print $self "Content-Type: $ct$CRLF";
 print $self "Content-Encoding: $ce$CRLF" if $ce;
 print $self "Content-Length: $size$CRLF" if $size;
 print $self "Last-Modified: ", time2str($mtime), "$CRLF" if $mtime;
 print $self $CRLF;
 }
 $self->send_file(\*F) unless $self->head_request;
 return RC_OK;
 }
 else {
 $self->send_error(RC_NOT_FOUND);
 }
}
sub send_dir
{
 my($self, $dir) = @_;
 $self->send_error(RC_NOT_FOUND) unless -d $dir;
 $self->send_error(RC_NOT_IMPLEMENTED);
}
sub send_file
{
 my($self, $file) = @_;
 my $opened = 0;
 local(*FILE);
 if (!ref($file)) {
 open(FILE, $file) || return undef;
 binmode(FILE);
 $file = \*FILE;
 $opened++;
 }
 my $cnt = 0;
 my $buf = "";
 my $n;
 while ($n = sysread($file, $buf, 8*1024)) {
 last if !$n;
 $cnt += $n;
 print $self $buf;
 }
 close($file) if $opened;
 $cnt;
}
sub daemon
{
 my $self = shift;
 ${*$self}{'httpd_daemon'};
}
1;
}
BEGIN { $INC{q{HTTP/Server/Brick.pm}} = 1;
package HTTP::Server::Brick;
use version;
our $VERSION = qv(0.1.3);
use warnings;
use strict;
use HTTP::Daemon;
use HTTP::Status;
use LWP::MediaTypes;
use URI;
use constant DEBUG => $ENV{DEBUG} || 0;
my $__singleton;
my $__server_should_run = 0;
$SIG{__WARN__} = sub { $__singleton ? $__singleton->_log( error => '[warn] ' . shift ) : CORE::warn(@_) };
$SIG{__DIE__} = sub {
 CORE::die (@_) if $^S; 
 $__singleton->_log( error => '[die] ' . $_[0] ) if $__singleton;
 CORE::die (@_)
};
$SIG{HUP} = sub { $__server_should_run = 0; };
sub new {
 my ($this, %args) = @_;
 my $class = ref($this) || $this;

if ($args{daemon_class} and not
 eval { $args{daemon_class}->isa('HTTP::Daemon') }) {
 die "daemon_class argument '$args{daemon_class}'" .
 " must inherit from HTTP::Daemon";
 }

my $self = bless {
 _site_map => [],
 error_log => \*STDERR,
 access_log => \*STDOUT,
 directory_index_file => 'index.html',
 directory_indexing => 1,
 daemon_class => 'HTTP::Daemon',
 daemon_args => [],
 %args,
 }, $class;

$__singleton = $self;

return $self;
}
sub mount {
 my ($self, $uri, $args) = @_;

ref($args) eq 'HASH' or die 'third arg to mount must be a hashref';

my $depth;
 if ($uri eq '/') {
 $depth = 0;
 } else {
 $uri =~ s!/$!!;
 my @parts = split( m!/!, $uri );
 $depth = scalar(@parts) - 1; 
 }

$self->{_site_map}[$depth] ||= {};
 $self->{_site_map}[$depth]{$uri} = $args;
 if (!exists $args->{wildcard} && exists $args->{path} && -d $args->{path}) {
 $args->{wildcard} = 1;
 }

my $mount_type = exists $args->{handler} ? 'handler' :
 exists $args->{path} ? 'directory' : '(unknown)';
 $self->_log( error => 'Mounted' . ($args->{wildcard} ? ' wildcard' : '') . " $mount_type at $uri" );

1;
}
sub start {
 my $self = shift;

$__server_should_run = 1;
 unless ($self->{leave_sig_pipe_handler_alone}) {
 $self->{_old_sig_pipe_handler} = $SIG{'PIPE'};
 $SIG{'PIPE'} = 'IGNORE';
 }

$SIG{CHLD} = 'IGNORE' if $self->{fork};

$self->{daemon} = $self->{daemon_class}->new(
 ReuseAddr => 1,
 LocalPort => $self->{port},
 LocalHost => $self->{host},
 Timeout => 5,
 @{ $self->{daemon_args} },
 ) or die "Can't start daemon: $!";
 my $url_string = UNIVERSAL::can($self->{daemon}->url, 'as_string') ?
 $self->{daemon}->url->as_string :
 $self->{daemon}->url;

$self->_log(error => "Server started on $url_string");

while ($__server_should_run) {
 my $conn = $self->{daemon}->accept or next;
 next if $self->{fork} and fork;
 while (my $req = $conn->get_request) {
 my ($r_port, $r_iaddr) = Socket::unpack_sockaddr_in($conn->peername);
 my $ip = Socket::inet_ntoa($r_iaddr);
 $req->headers->remove_header('X-Brick-Remote-IP');
 $req->header('X-Brick-Remote-IP' => $ip) if defined $ip;

my ($submap, $match) = $self->_map_request($req);

if ($submap) {
 if (exists $submap->{path}) {
 $self->_handle_static_request( $conn, $req, $submap, $match);

} elsif (exists $submap->{handler}) {
 $self->_handle_dynamic_request( $conn, $req, $submap, $match);

} else {
 $self->_send_error($conn, $req, RC_INTERNAL_SERVER_ERROR, 'Corrupt Site Map');
 }

} else {
 $self->_send_error($conn, $req, RC_NOT_FOUND, ' Not Found in Site Map');
 }
 }
 exit if $self->{fork};
 }

unless ($self->{leave_sig_pipe_handler_alone}) {
 $SIG{'PIPE'} = $self->{_old_sig_pipe_handler};
 }

1;
}
sub _handle_static_request {
 my ($self, $conn, $req, $submap, $match) = @_;

my $path = $submap->{path} . '/' . $match->{path_info};

if (-d $path && $match->{full_path} !~ m!/$! ) {
 $conn->send_redirect( $match->{full_path} . '/', RC_SEE_OTHER );
 DEBUG && $self->_log(error => 'redirecting to path with / appended: ' . $match->{full_path});
 return;
 }

my $serve_path = -d $path ? "$path/$self->{directory_index_file}" : $path;

if (-r $serve_path) {
 my $code = $conn->send_file_response($serve_path);
 $self->_log_status($req, $code);

} elsif (-d $path && $self->{directory_indexing}) {

my $res = $self->_render_directory($path, $match->{full_path});
 $conn->send_response( $res );
 $self->_log( access => '[' . RC_OK . "] $match->{full_path}" );

} elsif (-d $path) {
 $self->_send_error($conn, $req, RC_FORBIDDEN, 'Directory Indexing Not Allowed' );

} else {
 $self->_send_error($conn, $req, RC_NOT_FOUND, 'File Not Found' );
 }
}
sub _handle_dynamic_request {
 my ($self, $conn, $req, $submap, $match) = @_;

my $res = HTTP::Response->new;
 $res->base($match->{full_path});
 $req->{mount_path} = $match->{mount_path};
 $req->{path_info} = $match->{path_info} ? '/' . $match->{path_info} : undef;
 my $url = $self->{daemon}->url;
 $url = URI->new($url) if ! ref $url;

if ($req->header('Host') =~ /^(.*):(.*)$/) {
 $req->{hostname} = $1;
 $req->{port} = $2;
 } elsif ($req->header('Host')) {
 $req->{hostname} = $req->header('Host');
 $req->{port} = $url->port;
 } else {
 $req->{hostname} = $url->host;
 $req->{port} = $url->port;
 }
 if ( my $return_code = eval { $submap->{handler}->($req, $res) } ) {

my $code = !$return_code ? RC_INTERNAL_SERVER_ERROR :
 $res->code ? $res->code :
 $return_code >= 100 ? $return_code : RC_OK;

$res->code($code);
 $res->header( 'Content-Type' ) || $res->header( 'Content-Type', 'text/html' );

if ($res->is_success) {
 $conn->send_response( $res );
 $self->_log( access => "[$code] $match->{full_path}" );

} elsif ($res->is_error) {
 $self->_send_error( $conn, $req, $res->code, $res->message );

} elsif ($res->is_redirect) {
 if (UNIVERSAL::can($res->{target_uri}, 'path')) {
 my $target = $res->{target_uri}->path;

if ($target !~ m!^/!) {
 $match->{full_path} =~ m!^(.*/)! and
 $target = $1 . $target;
 }
 $conn->send_redirect($target, $code);
 $self->_log( access => "[$code] Redirecting to " . $target );
 } else {
 $self->_send_error($conn, $req, RC_INTERNAL_SERVER_ERROR,
 'Handler Tried to Redirect Without Setting Target URI');
 }

} else {
 $self->_send_error($conn, $req,
 RC_NOT_IMPLEMENTED,
 'Handler Returned an Unimplemented Response Code: ' . $code);
 }
 } else {
 $self->_send_error($conn, $req, RC_INTERNAL_SERVER_ERROR, 'Handler Failed');
 $self->_log( error => "Handler Failed for mount: " . $match->{mount_path});
 $self->_log( error => $@ ) if $@;
 }

1;
}
sub _render_directory {
 my ($self, $path, $uri ) = @_;

my $res = HTTP::Response->new( RC_OK );
 $res->header( 'Content-type', 'text/html' );

$res->add_content(<<END_HEADER);
<html>
<head>
<title>Directory for $uri</title>
</head>
<body>
<h1>Directory for $uri</h1>
<blockquote><pre>
<a href="..">.. (Parent directory)</a>
END_HEADER

$res->add_content("<a href=\"$_\">$_</a>\n") for map {s!.*/!!; $_} sort glob "$path/*";

$res->add_content(<<END_FOOTER);
</pre></blockquote>
</body>
</html>
END_FOOTER

return $res;
}
sub _send_error {
 my ($self, $conn, $req, $code, $text) = @_;

$conn->send_error($code, $text);

$self->_log_status($req, $code, $text);
}
sub _log_status {
 my ($self, $req, $code, $text) = @_;

if ($code == RC_OK || $code == RC_UNAUTHORIZED || $code == RC_NOT_FOUND) {
 $self->_log( access => "[$code] " . $req->uri->path );
 }

$self->_log( error => "[$code] [" . $req->uri->path . '] ' . ($text || status_message($code)) )
 unless $code == RC_OK;
}
sub _map_request {
 my ($self, $request) = @_;

my $map = $self->{_site_map};

my $uri = $request->uri->path;

my @parts = split( m!/!, $uri );

my $depth = scalar(@parts) - 1;
 $depth = 0 if $depth == -1;
 my $match_depth = $depth;

while ($match_depth >= 0) {

my $mount_path = '/' . join('/', @parts[1..$match_depth]);

if ($map->[$match_depth] && exists $map->[$match_depth]{$mount_path}) {
 if ($match_depth != $depth && !$map->[$match_depth]{$mount_path}{wildcard}) {
 return;
 }

return(
 $map->[$match_depth]{$mount_path},
 {
 full_path => $uri,
 mount_path => $mount_path,
 path_info => join('/', @parts[$match_depth+1..$depth]),
 },
 );
 }

$match_depth--;
 }
}
LWP::MediaTypes::add_type('image/png' => qw(png));
LWP::MediaTypes::add_type('text/css' => qw(css));
LWP::MediaTypes::add_type('text/javascript' => qw(js));
sub add_type {
 my ($self, @args) = @_;

LWP::MediaTypes::add_type(@args);
}
sub _log {
 my ($self, $log_key, $text) = @_;

$self->{"${log_key}_log"}->print( '[' . localtime() . "] [$$] ", $text, "\n" );
}
1; 
}
BEGIN { $INC{q{Hash/Merge/Simple.pm}} = 1;
package Hash::Merge::Simple;
use warnings;
use strict;
our $VERSION = '0.04';
use vars qw/@ISA @EXPORT_OK/;
@ISA = qw/Exporter/;
@EXPORT_OK = qw/merge clone_merge dclone_merge/;
sub merge (@);
sub merge (@) {
 shift unless ref $_[0]; 
 my ($left, @right) = @_;

return $left unless @right;

return merge($left, merge(@right)) if @right > 1;

my ($right) = @right;

my %merge = %$left;

for my $key (keys %$right) {

my ($hr, $hl) = map { ref $_->{$key} eq 'HASH' } $right, $left;

if ($hr and $hl){
 $merge{$key} = merge($left->{$key}, $right->{$key});
 }
 else {
 $merge{$key} = $right->{$key};
 }
 }

return \%merge;
}
sub clone_merge {
 require Clone;
 my $result = merge @_;
 return Clone::clone( $result );
}
sub dclone_merge {
 require Storable;
 my $result = merge @_;
 return Storable::dclone( $result );
}
1; 
}
BEGIN { $INC{q{JSON/PP58.pm}} = 1;
package JSON::PP58;
use 5.008;
use strict;
my @properties;
$JSON::PP58::VERSION = '1.03';
BEGIN {

unless ( defined &utf8::is_utf8 ) {
 require Encode;
 *utf8::is_utf8 = *Encode::is_utf8;
 }

*JSON::PP::JSON_PP_encode_ascii = \&JSON::PP::_encode_ascii;
 *JSON::PP::JSON_PP_encode_latin1 = \&JSON::PP::_encode_latin1;
 *JSON::PP::JSON_PP_decode_surrogates = \&JSON::PP::_decode_surrogates;
 *JSON::PP::JSON_PP_decode_unicode = \&JSON::PP::_decode_unicode;

if ($] >= 5.008 and $] < 5.008003) { 
 package JSON::PP;
 require subs;
 subs->import('join');
 eval q|
            sub join {
                return '' if (@_ < 2);
                my $j   = shift;
                my $str = shift;
                for (@_) { $str .= $j . $_; }
                return $str;
            }
        |;
 }
}
sub JSON::PP::incr_parse {
 local $Carp::CarpLevel = 1;
 ( $_[0]->{_incr_parser} ||= JSON::PP::IncrParser->new )->incr_parse( @_ );
}
sub JSON::PP::incr_text : lvalue {
 $_[0]->{_incr_parser} ||= JSON::PP::IncrParser->new;

if ( $_[0]->{_incr_parser}->{incr_parsing} ) {
 Carp::croak("incr_text can not be called when the incremental parser already started parsing");
 }
 $_[0]->{_incr_parser}->{incr_text};
}
sub JSON::PP::incr_skip {
 ( $_[0]->{_incr_parser} ||= JSON::PP::IncrParser->new )->incr_skip;
}
sub JSON::PP::incr_reset {
 ( $_[0]->{_incr_parser} ||= JSON::PP::IncrParser->new )->incr_reset;
}
1;
}
BEGIN { $INC{q{JSON/PP.pm}} = 1;
package JSON::PP;
use 5.005;
use strict;
use base qw(Exporter);
use overload;
use Carp ();
use B ();
$JSON::PP::VERSION = '2.26000';
@JSON::PP::EXPORT = qw(encode_json decode_json from_json to_json);
use constant P_ASCII => 0;
use constant P_LATIN1 => 1;
use constant P_UTF8 => 2;
use constant P_INDENT => 3;
use constant P_CANONICAL => 4;
use constant P_SPACE_BEFORE => 5;
use constant P_SPACE_AFTER => 6;
use constant P_ALLOW_NONREF => 7;
use constant P_SHRINK => 8;
use constant P_ALLOW_BLESSED => 9;
use constant P_CONVERT_BLESSED => 10;
use constant P_RELAXED => 11;
use constant P_LOOSE => 12;
use constant P_ALLOW_BIGNUM => 13;
use constant P_ALLOW_BAREKEY => 14;
use constant P_ALLOW_SINGLEQUOTE => 15;
use constant P_ESCAPE_SLASH => 16;
use constant P_AS_NONBLESSED => 17;
use constant P_ALLOW_UNKNOWN => 18;
BEGIN {
 my @xs_compati_bit_properties = qw(latin1 ascii utf8 indent canonical space_before space_after allow_nonref shrink allow_blessed convert_blessed relaxed allow_unknown);
 my @pp_bit_properties = qw(allow_singlequote allow_bignum loose allow_barekey escape_slash as_nonblessed);

my $helper = $] >= 5.008 ? 'JSON::PP58'
 : $] >= 5.006 ? 'JSON::PP56'
 : 'JSON::PP5005'
 ;

eval qq| require $helper |;
 if ($@) { Carp::croak $@; }

for my $name (@xs_compati_bit_properties, @pp_bit_properties) {
 my $flag_name = 'P_' . uc($name);

eval qq/
            sub $name {
                my \$enable = defined \$_[1] ? \$_[1] : 1;

                if (\$enable) {
                    \$_[0]->{PROPS}->[$flag_name] = 1;
                }
                else {
                    \$_[0]->{PROPS}->[$flag_name] = 0;
                }

                \$_[0];
            }

            sub get_$name {
                \$_[0]->{PROPS}->[$flag_name] ? 1 : '';
            }
        /;
 }
}
my %encode_allow_method
 = map {($_ => 1)} qw/utf8 pretty allow_nonref latin1 self_encode escape_slash allow_blessed convert_blessed indent indent_length allow_bignum as_nonblessed/;
my %decode_allow_method
 = map {($_ => 1)} qw/utf8 allow_nonref loose allow_singlequote allow_bignum allow_barekey max_size relaxed/;
my $JSON; 
sub encode_json ($) { 
 ($JSON ||= __PACKAGE__->new->utf8)->encode(@_);
}
sub decode_json { 
 ($JSON ||= __PACKAGE__->new->utf8)->decode(@_);
}
sub to_json($) {
 Carp::croak ("JSON::PP::to_json has been renamed to encode_json.");
}
sub from_json($) {
 Carp::croak ("JSON::PP::from_json has been renamed to decode_json.");
}
sub new {
 my $class = shift;
 my $self = {
 max_depth => 512,
 max_size => 0,
 indent => 0,
 FLAGS => 0,
 fallback => sub { encode_error('Invalid value. JSON can only reference.') },
 indent_length => 3,
 };

bless $self, $class;
}
sub encode {
 return $_[0]->PP_encode_json($_[1]);
}
sub decode {
 return $_[0]->PP_decode_json($_[1], 0x00000000);
}
sub decode_prefix {
 return $_[0]->PP_decode_json($_[1], 0x00000001);
}
sub pretty {
 my ($self, $v) = @_;
 my $enable = defined $v ? $v : 1;

if ($enable) { 
 $self->indent(1)->indent_length(3)->space_before(1)->space_after(1);
 }
 else {
 $self->indent(0)->space_before(0)->space_after(0);
 }

$self;
}
sub max_depth {
 my $max = defined $_[1] ? $_[1] : 0x80000000;
 $_[0]->{max_depth} = $max;
 $_[0];
}
sub get_max_depth { $_[0]->{max_depth}; }
sub max_size {
 my $max = defined $_[1] ? $_[1] : 0;
 $_[0]->{max_size} = $max;
 $_[0];
}
sub get_max_size { $_[0]->{max_size}; }
sub filter_json_object {
 $_[0]->{cb_object} = defined $_[1] ? $_[1] : 0;
 $_[0]->{F_HOOK} = ($_[0]->{cb_object} or $_[0]->{cb_sk_object}) ? 1 : 0;
 $_[0];
}
sub filter_json_single_key_object {
 if (@_ > 1) {
 $_[0]->{cb_sk_object}->{$_[1]} = $_[2];
 }
 $_[0]->{F_HOOK} = ($_[0]->{cb_object} or $_[0]->{cb_sk_object}) ? 1 : 0;
 $_[0];
}
sub indent_length {
 if (!defined $_[1] or $_[1] > 15 or $_[1] < 0) {
 Carp::carp "The acceptable range of indent_length() is 0 to 15.";
 }
 else {
 $_[0]->{indent_length} = $_[1];
 }
 $_[0];
}
sub get_indent_length {
 $_[0]->{indent_length};
}
sub sort_by {
 $_[0]->{sort_by} = defined $_[1] ? $_[1] : 1;
 $_[0];
}
sub allow_bigint {
 Carp::carp("allow_bigint() is obsoleted. use allow_bignum() insted.");
}
{ 

my $max_depth;
 my $indent;
 my $ascii;
 my $latin1;
 my $utf8;
 my $space_before;
 my $space_after;
 my $canonical;
 my $allow_blessed;
 my $convert_blessed;

my $indent_length;
 my $escape_slash;
 my $bignum;
 my $as_nonblessed;

my $depth;
 my $indent_count;
 my $keysort;

sub PP_encode_json {
 my $self = shift;
 my $obj = shift;

$indent_count = 0;
 $depth = 0;

my $idx = $self->{PROPS};

($ascii, $latin1, $utf8, $indent, $canonical, $space_before, $space_after, $allow_blessed,
 $convert_blessed, $escape_slash, $bignum, $as_nonblessed)
 = @{$idx}[P_ASCII .. P_SPACE_AFTER, P_ALLOW_BLESSED, P_CONVERT_BLESSED,
 P_ESCAPE_SLASH, P_ALLOW_BIGNUM, P_AS_NONBLESSED];

($max_depth, $indent_length) = @{$self}{qw/max_depth indent_length/};

$keysort = $canonical ? sub { $a cmp $b } : undef;

if ($self->{sort_by}) {
 $keysort = ref($self->{sort_by}) eq 'CODE' ? $self->{sort_by}
 : $self->{sort_by} =~ /\D+/ ? $self->{sort_by}
 : sub { $a cmp $b };
 }

encode_error("hash- or arrayref expected (not a simple scalar, use allow_nonref to allow this)")
 if(!ref $obj and !$idx->[ P_ALLOW_NONREF ]);

my $str = $self->object_to_json($obj);

$str .= "\n" if ( $indent ); 

unless ($ascii or $latin1 or $utf8) {
 utf8::upgrade($str);
 }

if ($idx->[ P_SHRINK ]) {
 utf8::downgrade($str, 1);
 }

return $str;
 }

sub object_to_json {
 my ($self, $obj) = @_;
 my $type = ref($obj);

if($type eq 'HASH'){
 return $self->hash_to_json($obj);
 }
 elsif($type eq 'ARRAY'){
 return $self->array_to_json($obj);
 }
 elsif ($type) { 
 if (blessed($obj)) {

return $self->value_to_json($obj) if ( $obj->isa('JSON::PP::Boolean') );

if ( $convert_blessed and $obj->can('TO_JSON') ) {
 my $result = $obj->TO_JSON;
 if ( defined $result and $obj eq $result ) {
 encode_error( sprintf(
 "%s::TO_JSON method returned same object as was passed instead of a new one",
 ref $obj
 ) );
 }
 return $self->object_to_json( $result );
 }

return "$obj" if ( $bignum and _is_bignum($obj) );
 return $self->blessed_to_json($obj) if ($allow_blessed and $as_nonblessed); 

encode_error( sprintf("encountered object '%s', but neither allow_blessed "
 . "nor convert_blessed settings are enabled", $obj)
 ) unless ($allow_blessed);

return 'null';
 }
 else {
 return $self->value_to_json($obj);
 }
 }
 else{
 return $self->value_to_json($obj);
 }
 }

sub hash_to_json {
 my ($self, $obj) = @_;
 my ($k,$v);
 my %res;

encode_error("json text or perl structure exceeds maximum nesting level (max_depth set too low?)")
 if (++$depth > $max_depth);

my ($pre, $post) = $indent ? $self->_up_indent : ('', '');
 my $del = ($space_before ? ' ' : '') . ':' . ($space_after ? ' ' : '');

if ( my $tie_class = tied %$obj ) {
 if ( $tie_class->can('TIEHASH') ) {
 $tie_class =~ s/=.+$//;
 tie %res, $tie_class;
 }
 }
 my $has;

for my $k (keys %$obj) {
 my $v = $obj->{$k};
 $res{$k} = $self->object_to_json($v) || $self->value_to_json($v);
 $has = 1 unless ( $has );
 }

--$depth;
 $self->_down_indent if ($indent);

return '{' . ( $has ? $pre : '' ) 
 . ( $has ? join(",$pre", map { utf8::decode($_) if ($] < 5.008); 
 string_to_json($self, $_) . $del . $res{$_} 
 } _sort( $self, \%res )
 ) . $post 
 : ''
 )
 . '}';
 }

sub array_to_json {
 my ($self, $obj) = @_;
 my @res;

encode_error("json text or perl structure exceeds maximum nesting level (max_depth set too low?)")
 if (++$depth > $max_depth);

my ($pre, $post) = $indent ? $self->_up_indent : ('', '');

if (my $tie_class = tied @$obj) {
 if ( $tie_class->can('TIEARRAY') ) {
 $tie_class =~ s/=.+$//;
 tie @res, $tie_class;
 }
 }

for my $v (@$obj){
 push @res, $self->object_to_json($v) || $self->value_to_json($v);
 }

--$depth;
 $self->_down_indent if ($indent);

return '[' . ( @res ? $pre : '' ) . ( @res ? join( ",$pre", @res ) . $post : '' ) . ']';
 }

sub value_to_json {
 my ($self, $value) = @_;

return 'null' if(!defined $value);

my $b_obj = B::svref_2object(\$value); 
 my $flags = $b_obj->FLAGS;

return $value 
 if ( ( $flags & B::SVf_IOK or $flags & B::SVp_IOK
 or $flags & B::SVf_NOK or $flags & B::SVp_NOK
 ) and !($flags & B::SVf_POK )
 ); 

my $type = ref($value);

if(!$type){
 return string_to_json($self, $value);
 }
 elsif( blessed($value) and $value->isa('JSON::PP::Boolean') ){
 return $$value == 1 ? 'true' : 'false';
 }
 elsif ($type) {
 if ((overload::StrVal($value) =~ /=(\w+)/)[0]) {
 return $self->value_to_json("$value");
 }

if ($type eq 'SCALAR' and defined $$value) {
 return $$value eq '1' ? 'true'
 : $$value eq '0' ? 'false'
 : $self->{PROPS}->[ P_ALLOW_UNKNOWN ] ? 'null'
 : encode_error("cannot encode reference to scalar");
 }

if ( $self->{PROPS}->[ P_ALLOW_UNKNOWN ] ) {
 return 'null';
 }
 else {
 if ( $type eq 'SCALAR' or $type eq 'REF' ) {
 encode_error("cannot encode reference to scalar");
 }
 else {
 encode_error("encountered $value, but JSON can only represent references to arrays or hashes");
 }
 }

}
 else {
 return $self->{fallback}->($value)
 if ($self->{fallback} and ref($self->{fallback}) eq 'CODE');
 return 'null';
 }

}

my %esc = (
 "\n" => '\n',
 "\r" => '\r',
 "\t" => '\t',
 "\f" => '\f',
 "\b" => '\b',
 "\"" => '\"',
 "\\" => '\\\\',
 "\'" => '\\\'',
 );

sub string_to_json {
 my ($self, $arg) = @_;

$arg =~ s/([\x22\x5c\n\r\t\f\b])/$esc{$1}/g;
 $arg =~ s/\//\\\//g if ($escape_slash);
 $arg =~ s/([\x00-\x08\x0b\x0e-\x1f])/'\\u00' . unpack('H2', $1)/eg;

if ($ascii) {
 $arg = JSON_PP_encode_ascii($arg);
 }

if ($latin1) {
 $arg = JSON_PP_encode_latin1($arg);
 }

if ($utf8) {
 utf8::encode($arg);
 }

return '"' . $arg . '"';
 }

sub blessed_to_json {
 my $b_obj = B::svref_2object($_[1]);
 if ($b_obj->isa('B::HV')) {
 return $_[0]->hash_to_json($_[1]);
 }
 elsif ($b_obj->isa('B::AV')) {
 return $_[0]->array_to_json($_[1]);
 }
 else {
 return 'null';
 }
 }

sub encode_error {
 my $error = shift;
 Carp::croak "$error";
 }

sub _sort {
 my ($self, $res) = @_;
 defined $keysort ? (sort $keysort (keys %$res)) : keys %$res;
 }

sub _up_indent {
 my $self = shift;
 my $space = ' ' x $indent_length;

my ($pre,$post) = ('','');

$post = "\n" . $space x $indent_count;

$indent_count++;

$pre = "\n" . $space x $indent_count;

return ($pre,$post);
 }

sub _down_indent { $indent_count--; }

sub PP_encode_box {
 {
 depth => $depth,
 indent_count => $indent_count,
 };
 }
} 
sub _encode_ascii {
 join('',
 map {
 $_ <= 127 ?
 chr($_) :
 $_ <= 65535 ?
 sprintf('\u%04x', $_) : sprintf('\u%x\u%x', _encode_surrogates($_));
 } unpack('U*', $_[0])
 );
}
sub _encode_latin1 {
 join('',
 map {
 $_ <= 255 ?
 chr($_) :
 $_ <= 65535 ?
 sprintf('\u%04x', $_) : sprintf('\u%x\u%x', _encode_surrogates($_));
 } unpack('U*', $_[0])
 );
}
sub _encode_surrogates { 
 my $uni = $_[0] - 0x10000;
 return ($uni / 0x400 + 0xD800, $uni % 0x400 + 0xDC00);
}
sub _is_bignum {
 $_[0]->isa('Math::BigInt') or $_[0]->isa('Math::BigFloat');
}
my $max_intsize;
BEGIN {
 my $checkint = 1111;
 for my $d (5..30) {
 $checkint .= 1;
 my $int = eval qq| $checkint |;
 if ($int =~ /[eE]/) {
 $max_intsize = $d - 1;
 last;
 }
 }
}
{ 

my %escapes = ( 
 b => "\x8",
 t => "\x9",
 n => "\xA",
 f => "\xC",
 r => "\xD",
 '\\' => '\\',
 '"' => '"',
 '/' => '/',
 );

my $text; 
 my $at; 
 my $ch; 
 my $len; 
 my $depth; 
 my $encoding; 
 my $is_valid_utf8; 
 my $utf8_len; 
 my $utf8; 
 my $max_depth; 
 my $max_size;
 my $relaxed;
 my $cb_object;
 my $cb_sk_object;

my $F_HOOK;

my $allow_bigint; 
 my $singlequote; 
 my $loose; 
 my $allow_barekey; 

sub PP_decode_json {
 my ($self, $opt); 

($self, $text, $opt) = @_;

($at, $ch, $depth) = (0, '', 0);

if (!defined $text or ref $text) {
 decode_error("malformed text data.");
 }

my $idx = $self->{PROPS};

($utf8, $relaxed, $loose, $allow_bigint, $allow_barekey, $singlequote)
 = @{$idx}[P_UTF8, P_RELAXED, P_LOOSE .. P_ALLOW_SINGLEQUOTE];

if ( $utf8 ) {
 utf8::downgrade( $text, 1 ) or Carp::croak("Wide character in subroutine entry");
 }
 else {
 utf8::upgrade( $text );
 }

$len = length $text;

($max_depth, $max_size, $cb_object, $cb_sk_object, $F_HOOK)
 = @{$self}{qw/max_depth max_size cb_object cb_sk_object F_HOOK/};

if ($max_size > 1) {
 use bytes;
 my $bytes = length $text;
 decode_error(
 sprintf("attempted decode of JSON text of %s bytes size, but max_size is set to %s"
 , $bytes, $max_size), 1
 ) if ($bytes > $max_size);
 }
 my @octets = unpack('C4', $text);
 $encoding = ( $octets[0] and $octets[1]) ? 'UTF-8'
 : (!$octets[0] and $octets[1]) ? 'UTF-16BE'
 : (!$octets[0] and !$octets[1]) ? 'UTF-32BE'
 : ( $octets[2] ) ? 'UTF-16LE'
 : (!$octets[2] ) ? 'UTF-32LE'
 : 'unknown';

my $result = value();

if (!$idx->[ P_ALLOW_NONREF ] and !ref $result) {
 decode_error(
 'JSON text must be an object or array (but found number, string, true, false or null,'
 . ' use allow_nonref to allow this)', 1);
 }

if ($len >= $at) {
 my $consumed = $at - 1;
 white();
 if ($ch) {
 decode_error("garbage after JSON object") unless ($opt & 0x00000001);
 return ($result, $consumed);
 }
 }

$result;
 }

sub next_chr {
 return $ch = undef if($at >= $len);
 $ch = substr($text, $at++, 1);
 }

sub value {
 white();
 return if(!defined $ch);
 return object() if($ch eq '{');
 return array() if($ch eq '[');
 return string() if($ch eq '"' or ($singlequote and $ch eq "'"));
 return number() if($ch =~ /[0-9]/ or $ch eq '-');
 return word();
 }

sub string {
 my ($i, $s, $t, $u);
 my $utf16;
 my $is_utf8;

($is_valid_utf8, $utf8_len) = ('', 0);

$s = ''; 

if($ch eq '"' or ($singlequote and $ch eq "'")){
 my $boundChar = $ch if ($singlequote);

OUTER: while( defined(next_chr()) ){

if((!$singlequote and $ch eq '"') or ($singlequote and $ch eq $boundChar)){
 next_chr();

if ($utf16) {
 decode_error("missing low surrogate character in surrogate pair");
 }

utf8::decode($s) if($is_utf8);

return $s;
 }
 elsif($ch eq '\\'){
 next_chr();
 if(exists $escapes{$ch}){
 $s .= $escapes{$ch};
 }
 elsif($ch eq 'u'){ 
 my $u = '';

for(1..4){
 $ch = next_chr();
 last OUTER if($ch !~ /[0-9a-fA-F]/);
 $u .= $ch;
 }
 if ($u =~ /^[dD][89abAB][0-9a-fA-F]{2}/) { 
 $utf16 = $u;
 }
 elsif ($u =~ /^[dD][c-fC-F][0-9a-fA-F]{2}/) { 
 unless (defined $utf16) {
 decode_error("missing high surrogate character in surrogate pair");
 }
 $is_utf8 = 1;
 $s .= JSON_PP_decode_surrogates($utf16, $u) || next;
 $utf16 = undef;
 }
 else {
 if (defined $utf16) {
 decode_error("surrogate pair expected");
 }

if ( ( my $hex = hex( $u ) ) > 127 ) {
 $is_utf8 = 1;
 $s .= JSON_PP_decode_unicode($u) || next;
 }
 else {
 $s .= chr $hex;
 }
 }

}
 else{
 unless ($loose) {
 decode_error('illegal backslash escape sequence in string');
 }
 $s .= $ch;
 }
 }
 else{

if ( ord $ch > 127 ) {
 if ( $utf8 ) {
 unless( $ch = is_valid_utf8($ch) ) {
 $at -= 1;
 decode_error("malformed UTF-8 character in JSON string");
 }
 else {
 $at += $utf8_len - 1;
 }
 }
 else {
 utf8::encode( $ch );
 }

$is_utf8 = 1;
 }

if (!$loose) {
 if ($ch =~ /[\x00-\x1f\x22\x5c]/) { 
 $at--;
 decode_error('invalid character encountered while parsing JSON string');
 }
 }

$s .= $ch;
 }
 }
 }

decode_error("unexpected end of string while parsing JSON string");
 }

sub white {
 while( defined $ch ){
 if($ch le ' '){
 next_chr();
 }
 elsif($ch eq '/'){
 next_chr();
 if(defined $ch and $ch eq '/'){
 1 while(defined(next_chr()) and $ch ne "\n" and $ch ne "\r");
 }
 elsif(defined $ch and $ch eq '*'){
 next_chr();
 while(1){
 if(defined $ch){
 if($ch eq '*'){
 if(defined(next_chr()) and $ch eq '/'){
 next_chr();
 last;
 }
 }
 else{
 next_chr();
 }
 }
 else{
 decode_error("Unterminated comment");
 }
 }
 next;
 }
 else{
 $at--;
 decode_error("malformed JSON string, neither array, object, number, string or atom");
 }
 }
 else{
 if ($relaxed and $ch eq '#') { 
 pos($text) = $at;
 $text =~ /\G([^\n]*(?:\r\n|\r|\n))/g;
 $at = pos($text);
 next_chr;
 next;
 }

last;
 }
 }
 }

sub array {
 my $a = [];

decode_error('json text or perl structure exceeds maximum nesting level (max_depth set too low?)')
 if (++$depth > $max_depth);

next_chr();
 white();

if(defined $ch and $ch eq ']'){
 --$depth;
 next_chr();
 return $a;
 }
 else {
 while(defined($ch)){
 push @$a, value();

white();

if (!defined $ch) {
 last;
 }

if($ch eq ']'){
 --$depth;
 next_chr();
 return $a;
 }

if($ch ne ','){
 last;
 }

next_chr();
 white();

if ($relaxed and $ch eq ']') {
 --$depth;
 next_chr();
 return $a;
 }

}
 }

decode_error(", or ] expected while parsing array");
 }

sub object {
 my $o = {};
 my $k;

decode_error('json text or perl structure exceeds maximum nesting level (max_depth set too low?)')
 if (++$depth > $max_depth);
 next_chr();
 white();

if(defined $ch and $ch eq '}'){
 --$depth;
 next_chr();
 if ($F_HOOK) {
 return _json_object_hook($o);
 }
 return $o;
 }
 else {
 while (defined $ch) {
 $k = ($allow_barekey and $ch ne '"' and $ch ne "'") ? bareKey() : string();
 white();

if(!defined $ch or $ch ne ':'){
 $at--;
 decode_error("':' expected");
 }

next_chr();
 $o->{$k} = value();
 white();

last if (!defined $ch);

if($ch eq '}'){
 --$depth;
 next_chr();
 if ($F_HOOK) {
 return _json_object_hook($o);
 }
 return $o;
 }

if($ch ne ','){
 last;
 }

next_chr();
 white();

if ($relaxed and $ch eq '}') {
 --$depth;
 next_chr();
 if ($F_HOOK) {
 return _json_object_hook($o);
 }
 return $o;
 }

}

}

$at--;
 decode_error(", or } expected while parsing object/hash");
 }

sub bareKey { 
 my $key;
 while($ch =~ /[^\x00-\x23\x25-\x2F\x3A-\x40\x5B-\x5E\x60\x7B-\x7F]/){
 $key .= $ch;
 next_chr();
 }
 return $key;
 }

sub word {
 my $word = substr($text,$at-1,4);

if($word eq 'true'){
 $at += 3;
 next_chr;
 return $JSON::PP::true;
 }
 elsif($word eq 'null'){
 $at += 3;
 next_chr;
 return undef;
 }
 elsif($word eq 'fals'){
 $at += 3;
 if(substr($text,$at,1) eq 'e'){
 $at++;
 next_chr;
 return $JSON::PP::false;
 }
 }

$at--; 

decode_error("'null' expected") if ($word =~ /^n/);
 decode_error("'true' expected") if ($word =~ /^t/);
 decode_error("'false' expected") if ($word =~ /^f/);
 decode_error("malformed JSON string, neither array, object, number, string or atom");
 }

sub number {
 my $n = '';
 my $v;
 if($ch eq '0'){
 my $peek = substr($text,$at,1);
 my $hex = $peek =~ /[xX]/; 

if($hex){
 decode_error("malformed number (leading zero must not be followed by another digit)");
 ($n) = ( substr($text, $at+1) =~ /^([0-9a-fA-F]+)/);
 }
 else{ 
 ($n) = ( substr($text, $at) =~ /^([0-7]+)/);
 if (defined $n and length $n > 1) {
 decode_error("malformed number (leading zero must not be followed by another digit)");
 }
 }

if(defined $n and length($n)){
 if (!$hex and length($n) == 1) {
 decode_error("malformed number (leading zero must not be followed by another digit)");
 }
 $at += length($n) + $hex;
 next_chr;
 return $hex ? hex($n) : oct($n);
 }
 }

if($ch eq '-'){
 $n = '-';
 next_chr;
 if (!defined $ch or $ch !~ /\d/) {
 decode_error("malformed number (no digits after initial minus)");
 }
 }

while(defined $ch and $ch =~ /\d/){
 $n .= $ch;
 next_chr;
 }

if(defined $ch and $ch eq '.'){
 $n .= '.';

next_chr;
 if (!defined $ch or $ch !~ /\d/) {
 decode_error("malformed number (no digits after decimal point)");
 }
 else {
 $n .= $ch;
 }

while(defined(next_chr) and $ch =~ /\d/){
 $n .= $ch;
 }
 }

if(defined $ch and ($ch eq 'e' or $ch eq 'E')){
 $n .= $ch;
 next_chr;

if(defined($ch) and ($ch eq '+' or $ch eq '-')){
 $n .= $ch;
 next_chr;
 if (!defined $ch or $ch =~ /\D/) {
 decode_error("malformed number (no digits after exp sign)");
 }
 $n .= $ch;
 }
 elsif(defined($ch) and $ch =~ /\d/){
 $n .= $ch;
 }
 else {
 decode_error("malformed number (no digits after exp sign)");
 }

while(defined(next_chr) and $ch =~ /\d/){
 $n .= $ch;
 }

}

$v .= $n;

if ($v !~ /[.eE]/ and length $v > $max_intsize) {
 if ($allow_bigint) { 
 require Math::BigInt;
 return Math::BigInt->new($v);
 }
 else {
 return "$v";
 }
 }
 elsif ($allow_bigint) {
 require Math::BigFloat;
 return Math::BigFloat->new($v);
 }

return 0+$v;
 }

sub is_valid_utf8 {

$utf8_len = $_[0] =~ /[\x00-\x7F]/ ? 1
 : $_[0] =~ /[\xC2-\xDF]/ ? 2
 : $_[0] =~ /[\xE0-\xEF]/ ? 3
 : $_[0] =~ /[\xF0-\xF4]/ ? 4
 : 0
 ;

return unless $utf8_len;

my $is_valid_utf8 = substr($text, $at - 1, $utf8_len);

return ( $is_valid_utf8 =~ /^(?:
             [\x00-\x7F]
            |[\xC2-\xDF][\x80-\xBF]
            |[\xE0][\xA0-\xBF][\x80-\xBF]
            |[\xE1-\xEC][\x80-\xBF][\x80-\xBF]
            |[\xED][\x80-\x9F][\x80-\xBF]
            |[\xEE-\xEF][\x80-\xBF][\x80-\xBF]
            |[\xF0][\x90-\xBF][\x80-\xBF][\x80-\xBF]
            |[\xF1-\xF3][\x80-\xBF][\x80-\xBF][\x80-\xBF]
            |[\xF4][\x80-\x8F][\x80-\xBF][\x80-\xBF]
        )$/x ) ? $is_valid_utf8 : '';
 }

sub decode_error {
 my $error = shift;
 my $no_rep = shift;
 my $str = defined $text ? substr($text, $at) : '';
 my $mess = '';
 my $type = $] >= 5.008 ? 'U*'
 : $] < 5.006 ? 'C*'
 : utf8::is_utf8( $str ) ? 'U*' 
 : 'C*'
 ;

for my $c ( unpack( $type, $str ) ) { 
 $mess .= $c == 0x07 ? '\a'
 : $c == 0x09 ? '\t'
 : $c == 0x0a ? '\n'
 : $c == 0x0d ? '\r'
 : $c == 0x0c ? '\f'
 : $c < 0x20 ? sprintf('\x{%x}', $c)
 : $c < 0x80 ? chr($c)
 : sprintf('\x{%x}', $c)
 ;
 if ( length $mess >= 20 ) {
 $mess .= '...';
 last;
 }
 }

unless ( length $mess ) {
 $mess = '(end of string)';
 }

Carp::croak (
 $no_rep ? "$error" : "$error, at character offset $at (before \"$mess\")"
 );
 }

sub _json_object_hook {
 my $o = $_[0];
 my @ks = keys %{$o};

if ( $cb_sk_object and @ks == 1 and exists $cb_sk_object->{ $ks[0] } and ref $cb_sk_object->{ $ks[0] } ) {
 my @val = $cb_sk_object->{ $ks[0] }->( $o->{$ks[0]} );
 if (@val == 1) {
 return $val[0];
 }
 }

my @val = $cb_object->($o) if ($cb_object);
 if (@val == 0 or @val > 1) {
 return $o;
 }
 else {
 return $val[0];
 }
 }

sub PP_decode_box {
 {
 text => $text,
 at => $at,
 ch => $ch,
 len => $len,
 depth => $depth,
 encoding => $encoding,
 is_valid_utf8 => $is_valid_utf8,
 };
 }
} 
sub _decode_surrogates { 
 my $uni = 0x10000 + (hex($_[0]) - 0xD800) * 0x400 + (hex($_[1]) - 0xDC00);
 my $un = pack('U*', $uni);
 utf8::encode( $un );
 return $un;
}
sub _decode_unicode {
 my $un = pack('U', hex shift);
 utf8::encode( $un );
 return $un;
}
BEGIN {
 eval 'require Scalar::Util';
 unless($@){
 *JSON::PP::blessed = \&Scalar::Util::blessed;
 }
 else{ 
 eval 'sub UNIVERSAL::a_sub_not_likely_to_be_here { ref($_[0]) }';
 *JSON::PP::blessed = sub {
 local($@, $SIG{__DIE__}, $SIG{__WARN__});
 ref($_[0]) ? eval { $_[0]->a_sub_not_likely_to_be_here } : undef;
 };
 }
}
$JSON::PP::true = do { bless \(my $dummy = 1), "JSON::PP::Boolean" };
$JSON::PP::false = do { bless \(my $dummy = 0), "JSON::PP::Boolean" };
sub is_bool { defined $_[0] and UNIVERSAL::isa($_[0], "JSON::PP::Boolean"); }
sub true { $JSON::PP::true }
sub false { $JSON::PP::false }
sub null { undef; }
package JSON::PP::Boolean;
use overload (
 "0+" => sub { ${$_[0]} },
 "++" => sub { $_[0] = ${$_[0]} + 1 },
 "--" => sub { $_[0] = ${$_[0]} - 1 },
 fallback => 1,
);
package JSON::PP::IncrParser;
use strict;
use constant INCR_M_WS => 0; 
use constant INCR_M_STR => 1; 
use constant INCR_M_BS => 2; 
use constant INCR_M_JSON => 3; 
$JSON::PP::IncrParser::VERSION = '1.01';
my $unpack_format = $] < 5.006 ? 'C*' : 'U*';
sub new {
 my ( $class ) = @_;

bless {
 incr_nest => 0,
 incr_text => undef,
 incr_parsing => 0,
 incr_p => 0,
 }, $class;
}
sub incr_parse {
 my ( $self, $coder, $text ) = @_;

$self->{incr_text} = '' unless ( defined $self->{incr_text} );

if ( defined $text ) {
 if ( utf8::is_utf8( $text ) and !utf8::is_utf8( $self->{incr_text} ) ) {
 utf8::upgrade( $self->{incr_text} ) ;
 utf8::decode( $self->{incr_text} ) ;
 }
 $self->{incr_text} .= $text;
 }

my $max_size = $coder->get_max_size;

if ( defined wantarray ) {

$self->{incr_mode} = INCR_M_WS;

if ( wantarray ) {
 my @ret;

$self->{incr_parsing} = 1;

do {
 push @ret, $self->_incr_parse( $coder, $self->{incr_text} );

unless ( !$self->{incr_nest} and $self->{incr_mode} == INCR_M_JSON ) {
 $self->{incr_mode} = INCR_M_WS;
 }

} until ( !$self->{incr_text} );

$self->{incr_parsing} = 0;

return @ret;
 }
 else { 
 $self->{incr_parsing} = 1;
 my $obj = $self->_incr_parse( $coder, $self->{incr_text} );
 $self->{incr_parsing} = 0 if defined $obj; 
 return $obj ? $obj : undef; 
 }

}
}
sub _incr_parse {
 my ( $self, $coder, $text, $skip ) = @_;
 my $p = $self->{incr_p};
 my $restore = $p;

my @obj;
 my $len = length $text;

if ( $self->{incr_mode} == INCR_M_WS ) {
 while ( $len > $p ) {
 my $s = substr( $text, $p, 1 );
 $p++ and next if ( 0x20 >= unpack($unpack_format, $s) );
 $self->{incr_mode} = INCR_M_JSON;
 last;
 }
 }

while ( $len > $p ) {
 my $s = substr( $text, $p++, 1 );

if ( $s eq '"' ) {
 if ( $self->{incr_mode} != INCR_M_STR ) {
 $self->{incr_mode} = INCR_M_STR;
 }
 else {
 $self->{incr_mode} = INCR_M_JSON;
 unless ( $self->{incr_nest} ) {
 last;
 }
 }
 }

if ( $self->{incr_mode} == INCR_M_JSON ) {

if ( $s eq '[' or $s eq '{' ) {
 if ( ++$self->{incr_nest} > $coder->get_max_depth ) {
 Carp::croak('json text or perl structure exceeds maximum nesting level (max_depth set too low?)');
 }
 }
 elsif ( $s eq ']' or $s eq '}' ) {
 last if ( --$self->{incr_nest} <= 0 );
 }
 }

}

$self->{incr_p} = $p;

return if ( $self->{incr_mode} == INCR_M_JSON and $self->{incr_nest} > 0 );

return '' unless ( length substr( $self->{incr_text}, 0, $p ) );

local $Carp::CarpLevel = 2;

$self->{incr_p} = $restore;
 $self->{incr_c} = $p;

my ( $obj, $tail ) = $coder->decode_prefix( substr( $self->{incr_text}, 0, $p ) );

$self->{incr_text} = substr( $self->{incr_text}, $p );
 $self->{incr_p} = 0;

return $obj or '';
}
sub incr_text {
 if ( $_[0]->{incr_parsing} ) {
 Carp::croak("incr_text can not be called when the incremental parser already started parsing");
 }
 $_[0]->{incr_text};
}
sub incr_skip {
 my $self = shift;
 $self->{incr_text} = substr( $self->{incr_text}, $self->{incr_c} );
 $self->{incr_p} = 0;
}
sub incr_reset {
 my $self = shift;
 $self->{incr_text} = undef;
 $self->{incr_p} = 0;
 $self->{incr_mode} = 0;
 $self->{incr_nest} = 0;
 $self->{incr_parsing} = 0;
}
1;
}
BEGIN { $INC{q{JSON.pm}} = 1;
package JSON;
use strict;
use Carp ();
use base qw(Exporter);
@JSON::EXPORT = qw(from_json to_json jsonToObj objToJson encode_json decode_json);
BEGIN {
 $JSON::VERSION = '2.16';
 $JSON::DEBUG = 0 unless (defined $JSON::DEBUG);
}
my $Module_XS = 'JSON::XS';
my $Module_PP = 'JSON::PP';
my $XS_Version = '2.26';
my @PublicMethods = qw/ascii latin1 utf8 pretty indent space_before space_after relaxed canonical allow_nonref allow_blessed convert_blessed filter_json_object filter_json_single_key_object shrink max_depth max_size encode decode decode_prefix allow_unknown/;
my @Properties = qw/ascii latin1 utf8 indent space_before space_after relaxed canonical allow_nonref allow_blessed convert_blessed shrink max_depth max_size allow_unknown/;
my @XSOnlyMethods = qw//; 
my @PPOnlyMethods = qw/indent_length sort_by allow_singlequote allow_bignum loose allow_barekey escape_slash as_nonblessed/; 
my $_INSTALL_DONT_DIE = 1; 
my $_INSTALL_ONLY = 2; 
my $_ALLOW_UNSUPPORTED = 0;
my $_UNIV_CONV_BLESSED = 0;
unless ($JSON::Backend) {
 $JSON::DEBUG and Carp::carp("Check used worker module...");

my $backend = exists $ENV{PERL_JSON_BACKEND} ? $ENV{PERL_JSON_BACKEND} : 1;

if ($backend eq '1' or $backend =~ /JSON::XS\s*,\s*JSON::PP/) {
 _load_xs($_INSTALL_DONT_DIE) or _load_pp();
 }
 elsif ($backend eq '0' or $backend eq 'JSON::PP') {
 _load_pp();
 }
 elsif ($backend eq '2' or $backend eq 'JSON::XS') {
 _load_xs();
 }
 else {
 Carp::croak "The value of environmental variable 'PERL_JSON_BACKEND' is invalid.";
 }
}
sub import {
 my $pkg = shift;
 my @what_to_export;
 my $no_export;

for my $tag (@_) {
 if ($tag eq '-support_by_pp') {
 if (!$_ALLOW_UNSUPPORTED++) {
 JSON::Backend::XS
 ->support_by_pp(@PPOnlyMethods) if ($JSON::Backend eq $Module_XS);
 }
 next;
 }
 elsif ($tag eq '-no_export') {
 $no_export++, next;
 }
 elsif ( $tag eq '-convert_blessed_universally' ) {
 eval q|
                require B;
                *UNIVERSAL::TO_JSON = sub {
                    my $b_obj = B::svref_2object( $_[0] );
                    return    $b_obj->isa('B::HV') ? { %{ $_[0] } }
                            : $b_obj->isa('B::AV') ? [ @{ $_[0] } ]
                            : undef
                            ;
                }
            | if ( !$_UNIV_CONV_BLESSED++ );
 next;
 }
 push @what_to_export, $tag;
 }

return if ($no_export);

__PACKAGE__->export_to_level(1, $pkg, @what_to_export);
}
sub jsonToObj {
 my $alternative = 'from_json';
 if (defined $_[0] and UNIVERSAL::isa($_[0], 'JSON')) {
 shift @_; $alternative = 'decode';
 }
 Carp::carp "'jsonToObj' will be obsoleted. Please use '$alternative' instead.";
 return JSON::from_json(@_);
};
sub objToJson {
 my $alternative = 'to_json';
 if (defined $_[0] and UNIVERSAL::isa($_[0], 'JSON')) {
 shift @_; $alternative = 'encode';
 }
 Carp::carp "'objToJson' will be obsoleted. Please use '$alternative' instead.";
 JSON::to_json(@_);
};
sub to_json ($@) {
 my $json = new JSON;

if (@_ == 2 and ref $_[1] eq 'HASH') {
 my $opt = $_[1];
 for my $method (keys %$opt) {
 $json->$method( $opt->{$method} );
 }
 }

$json->encode($_[0]);
}
sub from_json ($@) {
 my $json = new JSON;

if (@_ == 2 and ref $_[1] eq 'HASH') {
 my $opt = $_[1];
 for my $method (keys %$opt) {
 $json->$method( $opt->{$method} );
 }
 }

return $json->decode( $_[0] );
}
sub true { $JSON::true }
sub false { $JSON::false }
sub null { undef; }
sub require_xs_version { $XS_Version; }
sub backend {
 my $proto = shift;
 $JSON::Backend;
}
sub is_xs {
 return $_[0]->module eq $Module_XS;
}
sub is_pp {
 return $_[0]->module eq $Module_PP;
}
sub pureperl_only_methods { @PPOnlyMethods; }
sub property {
 my ($self, $name, $value) = @_;

if (@_ == 1) {
 my %props;
 for $name (@Properties) {
 my $method = 'get_' . $name;
 if ($name eq 'max_size') {
 my $value = $self->$method();
 $props{$name} = $value == 1 ? 0 : $value;
 next;
 }
 $props{$name} = $self->$method();
 }
 return \%props;
 }
 elsif (@_ > 3) {
 Carp::croak('property() can take only the option within 2 arguments.');
 }
 elsif (@_ == 2) {
 if ( my $method = $self->can('get_' . $name) ) {
 if ($name eq 'max_size') {
 my $value = $self->$method();
 return $value == 1 ? 0 : $value;
 }
 $self->$method();
 }
 }
 else {
 $self->$name($value);
 }
}
sub _load_xs {
 my $opt = shift;

$JSON::DEBUG and Carp::carp "Load $Module_XS.";
 JSON::Boolean::_overrride_overload($Module_XS);
 JSON::Boolean::_overrride_overload($Module_PP);

eval qq|
        use $Module_XS $XS_Version ();
    |;

if ($@) {
 if (defined $opt and $opt & $_INSTALL_DONT_DIE) {
 $JSON::DEBUG and Carp::carp "Can't load $Module_XS...($@)";
 return 0;
 }
 Carp::croak $@;
 }

unless (defined $opt and $opt & $_INSTALL_ONLY) {
 _set_module( $JSON::Backend = $Module_XS );
 my $data = join("", <DATA>); 
 close(DATA);
 eval $data;
 JSON::Backend::XS->init;
 }

return 1;
};
sub _load_pp {
 my $opt = shift;

$JSON::DEBUG and Carp::carp "Load $Module_PP.";
 JSON::Boolean::_overrride_overload($Module_XS);
 JSON::Boolean::_overrride_overload($Module_PP);

eval qq| require $Module_PP |;
 if ($@) {
 Carp::croak $@;
 }

unless (defined $opt and $opt & $_INSTALL_ONLY) {
 _set_module( $JSON::Backend = $Module_PP );
 JSON::Backend::PP->init;
 }
};
sub _set_module {
 my $module = shift;

local $^W;
 no strict qw(refs);

$JSON::true = ${"$module\::true"};
 $JSON::false = ${"$module\::false"};

push @JSON::ISA, $module;
 push @{"$module\::Boolean::ISA"}, qw(JSON::Boolean);

*{"JSON::is_bool"} = \&{"$module\::is_bool"};

for my $method ($module eq $Module_XS ? @PPOnlyMethods : @XSOnlyMethods) {
 *{"JSON::$method"} = sub {
 Carp::carp("$method is not supported in $module.");
 $_[0];
 };
 }

return 1;
}
package JSON::Boolean;
my %Installed;
sub _overrride_overload {
 return if ($Installed{ $_[0] }++);

my $boolean = $_[0] . '::Boolean';

eval sprintf(q|
        package %s;
        use overload (
            '""' => sub { ${$_[0]} == 1 ? 'true' : 'false' },
            'eq' => sub {
                my ($obj, $op) = ref ($_[0]) ? ($_[0], $_[1]) : ($_[1], $_[0]);
                if ($op eq 'true' or $op eq 'false') {
                    return "$obj" eq 'true' ? 'true' eq $op : 'false' eq $op;
                }
                else {
                    return $obj ? 1 == $op : 0 == $op;
                }
            },
        );
    |, $boolean);

if ($@) { Carp::croak $@; }

return 1;
}
package JSON::Backend::PP;
sub init {
 local $^W;
 no strict qw(refs);
 *{"JSON::decode_json"} = \&{"JSON::PP::decode_json"};
 *{"JSON::encode_json"} = \&{"JSON::PP::encode_json"};
 *{"JSON::PP::is_xs"} = sub { 0 };
 *{"JSON::PP::is_pp"} = sub { 1 };
 return 1;
}
package JSON;
1;
}
BEGIN { $INC{q{YAML/Tiny.pm}} = 1;
package YAML::Tiny;
use strict;
use Carp 'croak';
sub HAVE_UTF8 () { $] >= 5.007003 }
BEGIN {
 if ( HAVE_UTF8 ) {
 eval "require utf8;";
 die "Failed to load UTF-8 support" if $@;
 }
 require 5.004;
 require Exporter;
 $YAML::Tiny::VERSION = '1.40';
 @YAML::Tiny::ISA = qw{Exporter};
 @YAML::Tiny::EXPORT = qw{Load Dump};
 @YAML::Tiny::EXPORT_OK = qw{LoadFile DumpFile freeze thaw};
 $YAML::Tiny::errstr = '';
}
my @UNPRINTABLE = qw(z x01 x02 x03 x04 x05 x06 a x08 t n v f r x0e x0f x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x1a e x1c x1d x1e x1f);
my %UNESCAPES = (
 z => "\x00", a => "\x07", t => "\x09",
 n => "\x0a", v => "\x0b", f => "\x0c",
 r => "\x0d", e => "\x1b", '\\' => '\\',
);
my %QUOTE = map { $_ => 1 } qw{null Null NULL y Y yes Yes YES n N no No NO true True TRUE false False FALSE on On ON off Off OFF};
sub new {
 my $class = shift;
 bless [ @_ ], $class;
}
sub read {
 my $class = ref $_[0] ? ref shift : shift;
 my $file = shift or return $class->_error( 'You did not specify a file name' );
 return $class->_error( "File '$file' does not exist" ) unless -e $file;
 return $class->_error( "'$file' is a directory, not a file" ) unless -f _;
 return $class->_error( "Insufficient permissions to read '$file'" ) unless -r _;
 local $/ = undef;
 local *CFG;
 unless ( open(CFG, $file) ) {
 return $class->_error("Failed to open file '$file': $!");
 }
 my $contents = <CFG>;
 unless ( close(CFG) ) {
 return $class->_error("Failed to close file '$file': $!");
 }

$class->read_string( $contents );
}
sub read_string {
 my $class = ref $_[0] ? ref shift : shift;
 my $self = bless [], $class;
 my $string = $_[0];
 unless ( defined $string ) {
 return $self->_error("Did not provide a string to load");
 }
 if ( $string =~ /^(?:\376\377|\377\376|\377\376\0\0|\0\0\376\377)/ ) {
 return $self->_error("Stream has a non UTF-8 BOM");
 } else {
 $string =~ s/^\357\273\277//;
 }
 utf8::decode($string) if HAVE_UTF8;
 return $self unless length $string;
 unless ( $string =~ /[\012\015]+\z/ ) {
 return $self->_error("Stream does not end with newline character");
 }
 my @lines = grep { ! /^\s*(?:\#.*)?\z/ }
 split /(?:\015{1,2}\012|\015|\012)/, $string;
 @lines and $lines[0] =~ /^\%YAML[: ][\d\.]+.*\z/ and shift @lines;
 while ( @lines ) {
 if ( $lines[0] =~ /^---\s*(?:(.+)\s*)?\z/ ) {
 shift @lines;
 if ( defined $1 and $1 !~ /^(?:\#.+|\%YAML[: ][\d\.]+)\z/ ) {
 push @$self, $self->_read_scalar( "$1", [ undef ], \@lines );
 next;
 }
 }

if ( ! @lines or $lines[0] =~ /^(?:---|\.\.\.)/ ) {
 push @$self, undef;
 while ( @lines and $lines[0] !~ /^---/ ) {
 shift @lines;
 }

} elsif ( $lines[0] =~ /^\s*\-/ ) {
 my $document = [ ];
 push @$self, $document;
 $self->_read_array( $document, [ 0 ], \@lines );

} elsif ( $lines[0] =~ /^(\s*)\S/ ) {
 my $document = { };
 push @$self, $document;
 $self->_read_hash( $document, [ length($1) ], \@lines );

} else {
 croak("YAML::Tiny failed to classify the line '$lines[0]'");
 }
 }

$self;
}
sub _read_scalar {
 my ($self, $string, $indent, $lines) = @_;
 $string =~ s/\s*\z//;
 return undef if $string eq '~';
 if ( $string =~ /^\'(.*?)\'\z/ ) {
 return '' unless defined $1;
 $string = $1;
 $string =~ s/\'\'/\'/g;
 return $string;
 }
 if ( $string =~ /^\"((?:\\.|[^\"])*)\"\z/ ) {
 $string = $1;
 $string =~ s/\\"/"/g;
 $string =~ s/\\([never\\fartz]|x([0-9a-fA-F]{2}))/(length($1)>1)?pack("H2",$2):$UNESCAPES{$1}/gex;
 return $string;
 }
 if ( $string =~ /^[\'\"!&]/ ) {
 croak("YAML::Tiny does not support a feature in line '$lines->[0]'");
 }
 return {} if $string eq '{}';
 return [] if $string eq '[]';
 return $string unless $string =~ /^[>|]/;
 croak("YAML::Tiny failed to find multi-line scalar content") unless @$lines;
 $lines->[0] =~ /^(\s*)/;
 $indent->[-1] = length("$1");
 if ( defined $indent->[-2] and $indent->[-1] <= $indent->[-2] ) {
 croak("YAML::Tiny found bad indenting in line '$lines->[0]'");
 }
 my @multiline = ();
 while ( @$lines ) {
 $lines->[0] =~ /^(\s*)/;
 last unless length($1) >= $indent->[-1];
 push @multiline, substr(shift(@$lines), length($1));
 }

my $j = (substr($string, 0, 1) eq '>') ? ' ' : "\n";
 my $t = (substr($string, 1, 1) eq '-') ? '' : "\n";
 return join( $j, @multiline ) . $t;
}
sub _read_array {
 my ($self, $array, $indent, $lines) = @_;

while ( @$lines ) {
 if ( $lines->[0] =~ /^(?:---|\.\.\.)/ ) {
 while ( @$lines and $lines->[0] !~ /^---/ ) {
 shift @$lines;
 }
 return 1;
 }
 $lines->[0] =~ /^(\s*)/;
 if ( length($1) < $indent->[-1] ) {
 return 1;
 } elsif ( length($1) > $indent->[-1] ) {
 croak("YAML::Tiny found bad indenting in line '$lines->[0]'");
 }

if ( $lines->[0] =~ /^(\s*\-\s+)[^\'\"]\S*\s*:(?:\s+|$)/ ) {
 my $indent2 = length("$1");
 $lines->[0] =~ s/-/ /;
 push @$array, { };
 $self->_read_hash( $array->[-1], [ @$indent, $indent2 ], $lines );

} elsif ( $lines->[0] =~ /^\s*\-(\s*)(.+?)\s*\z/ ) {
 shift @$lines;
 push @$array, $self->_read_scalar( "$2", [ @$indent, undef ], $lines );

} elsif ( $lines->[0] =~ /^\s*\-\s*\z/ ) {
 shift @$lines;
 unless ( @$lines ) {
 push @$array, undef;
 return 1;
 }
 if ( $lines->[0] =~ /^(\s*)\-/ ) {
 my $indent2 = length("$1");
 if ( $indent->[-1] == $indent2 ) {
 push @$array, undef;
 } else {
 push @$array, [ ];
 $self->_read_array( $array->[-1], [ @$indent, $indent2 ], $lines );
 }

} elsif ( $lines->[0] =~ /^(\s*)\S/ ) {
 push @$array, { };
 $self->_read_hash( $array->[-1], [ @$indent, length("$1") ], $lines );

} else {
 croak("YAML::Tiny failed to classify line '$lines->[0]'");
 }

} elsif ( defined $indent->[-2] and $indent->[-1] == $indent->[-2] ) {
 return 1;

} else {
 croak("YAML::Tiny failed to classify line '$lines->[0]'");
 }
 }

return 1;
}
sub _read_hash {
 my ($self, $hash, $indent, $lines) = @_;

while ( @$lines ) {
 if ( $lines->[0] =~ /^(?:---|\.\.\.)/ ) {
 while ( @$lines and $lines->[0] !~ /^---/ ) {
 shift @$lines;
 }
 return 1;
 }
 $lines->[0] =~ /^(\s*)/;
 if ( length($1) < $indent->[-1] ) {
 return 1;
 } elsif ( length($1) > $indent->[-1] ) {
 croak("YAML::Tiny found bad indenting in line '$lines->[0]'");
 }
 unless ( $lines->[0] =~ s/^\s*([^\'\" ][^\n]*?)\s*:(\s+|$)// ) {
 if ( $lines->[0] =~ /^\s*[?\'\"]/ ) {
 croak("YAML::Tiny does not support a feature in line '$lines->[0]'");
 }
 croak("YAML::Tiny failed to classify line '$lines->[0]'");
 }
 my $key = $1;
 if ( length $lines->[0] ) {
 $hash->{$key} = $self->_read_scalar( shift(@$lines), [ @$indent, undef ], $lines );
 } else {
 shift @$lines;
 unless ( @$lines ) {
 $hash->{$key} = undef;
 return 1;
 }
 if ( $lines->[0] =~ /^(\s*)-/ ) {
 $hash->{$key} = [];
 $self->_read_array( $hash->{$key}, [ @$indent, length($1) ], $lines );
 } elsif ( $lines->[0] =~ /^(\s*)./ ) {
 my $indent2 = length("$1");
 if ( $indent->[-1] >= $indent2 ) {
 $hash->{$key} = undef;
 } else {
 $hash->{$key} = {};
 $self->_read_hash( $hash->{$key}, [ @$indent, length($1) ], $lines );
 }
 }
 }
 }

return 1;
}
sub write {
 my $self = shift;
 my $file = shift or return $self->_error('No file name provided');
 open( CFG, '>' . $file ) or return $self->_error(
 "Failed to open file '$file' for writing: $!"
 );
 print CFG $self->write_string;
 close CFG;

return 1;
}
sub write_string {
 my $self = shift;
 return '' unless @$self;
 my $indent = 0;
 my @lines = ();
 foreach my $cursor ( @$self ) {
 push @lines, '---';
 if ( ! defined $cursor ) {
 } elsif ( ! ref $cursor ) {
 $lines[-1] .= ' ' . $self->_write_scalar( $cursor, $indent );
 } elsif ( ref $cursor eq 'ARRAY' ) {
 unless ( @$cursor ) {
 $lines[-1] .= ' []';
 next;
 }
 push @lines, $self->_write_array( $cursor, $indent, {} );
 } elsif ( ref $cursor eq 'HASH' ) {
 unless ( %$cursor ) {
 $lines[-1] .= ' {}';
 next;
 }
 push @lines, $self->_write_hash( $cursor, $indent, {} );

} else {
 croak("Cannot serialize " . ref($cursor));
 }
 }

join '', map { "$_\n" } @lines;
}
sub _write_scalar {
 my $string = $_[1];
 return '~' unless defined $string;
 return "''" unless length $string;
 if ( $string =~ /[\x00-\x08\x0b-\x0d\x0e-\x1f\"\'\n]/ ) {
 $string =~ s/\\/\\\\/g;
 $string =~ s/"/\\"/g;
 $string =~ s/\n/\\n/g;
 $string =~ s/([\x00-\x1f])/\\$UNPRINTABLE[ord($1)]/g;
 return qq|"$string"|;
 }
 if ( $string =~ /(?:^\W|\s)/ or $QUOTE{$string} ) {
 return "'$string'";
 }
 return $string;
}
sub _write_array {
 my ($self, $array, $indent, $seen) = @_;
 if ( $seen->{refaddr($array)}++ ) {
 die "YAML::Tiny does not support circular references";
 }
 my @lines = ();
 foreach my $el ( @$array ) {
 my $line = ('  ' x $indent) . '-';
 my $type = ref $el;
 if ( ! $type ) {
 $line .= ' ' . $self->_write_scalar( $el, $indent + 1 );
 push @lines, $line;

} elsif ( $type eq 'ARRAY' ) {
 if ( @$el ) {
 push @lines, $line;
 push @lines, $self->_write_array( $el, $indent + 1, $seen );
 } else {
 $line .= ' []';
 push @lines, $line;
 }

} elsif ( $type eq 'HASH' ) {
 if ( keys %$el ) {
 push @lines, $line;
 push @lines, $self->_write_hash( $el, $indent + 1, $seen );
 } else {
 $line .= ' {}';
 push @lines, $line;
 }

} else {
 die "YAML::Tiny does not support $type references";
 }
 }

@lines;
}
sub _write_hash {
 my ($self, $hash, $indent, $seen) = @_;
 if ( $seen->{refaddr($hash)}++ ) {
 die "YAML::Tiny does not support circular references";
 }
 my @lines = ();
 foreach my $name ( sort keys %$hash ) {
 my $el = $hash->{$name};
 my $line = ('  ' x $indent) . "$name:";
 my $type = ref $el;
 if ( ! $type ) {
 $line .= ' ' . $self->_write_scalar( $el, $indent + 1 );
 push @lines, $line;

} elsif ( $type eq 'ARRAY' ) {
 if ( @$el ) {
 push @lines, $line;
 push @lines, $self->_write_array( $el, $indent + 1, $seen );
 } else {
 $line .= ' []';
 push @lines, $line;
 }

} elsif ( $type eq 'HASH' ) {
 if ( keys %$el ) {
 push @lines, $line;
 push @lines, $self->_write_hash( $el, $indent + 1, $seen );
 } else {
 $line .= ' {}';
 push @lines, $line;
 }

} else {
 die "YAML::Tiny does not support $type references";
 }
 }

@lines;
}
sub _error {
 $YAML::Tiny::errstr = $_[1];
 undef;
}
sub errstr {
 $YAML::Tiny::errstr;
}
sub Dump {
 YAML::Tiny->new(@_)->write_string;
}
sub Load {
 my $self = YAML::Tiny->read_string(@_);
 unless ( $self ) {
 croak("Failed to load YAML document from string");
 }
 if ( wantarray ) {
 return @$self;
 } else {
 return $self->[-1];
 }
}
BEGIN {
 *freeze = *Dump;
 *thaw = *Load;
}
sub DumpFile {
 my $file = shift;
 YAML::Tiny->new(@_)->write($file);
}
sub LoadFile {
 my $self = YAML::Tiny->read($_[0]);
 unless ( $self ) {
 croak("Failed to load YAML document from '" . ($_[0] || '') . "'");
 }
 if ( wantarray ) {
 return @$self;
 } else {
 return $self->[-1];
 }
}
BEGIN {
 eval {
 require Scalar::Util;
 };
 if ( $@ ) {
 eval <<'END_PERL';
sub refaddr {
	my $pkg = ref($_[0]) or return undef;
	if (!!UNIVERSAL::can($_[0], 'can')) {
		bless $_[0], 'Scalar::Util::Fake';
	} else {
		$pkg = undef;
	}
	"$_[0]" =~ /0x(\w+)/;
	my $i = do { local $^W; hex $1 };
	bless $_[0], $pkg if defined $pkg;
	$i;
}
END_PERL
 } else {
 Scalar::Util->import('refaddr');
 }
}
1;
}
BEGIN { $INC{q{Config/Any/YAML/Tiny.pm}} = 1;
package Config::Any::YAML::Tiny;
use strict;
use warnings;
use base 'Config::Any::Base';
sub extensions {
 return qw(yml yaml);
}
sub load {
 my $class = shift;
 my $file = shift;

require YAML::Tiny;
 return YAML::Tiny->read( $file )->[0];
}
sub requires_any_of { 'YAML::Tiny' }
1;
}
BEGIN { $INC{q{MME/Util.pm}} = 1;
package MME::Util;
use strict;
use warnings;
use Config::Any;
use Hash::Merge::Simple qw/merge/;
use Data::AsObject;
sub _inflate_args {
 if ( ref eq 'ARRAY' ) {
 my $ref = $_;
 local $_;
 _inflate_args( $_ ) for @$ref;
 } else {
 $_ = Data::AsObject::dao( $_ )
 }
}
sub load_args_for {

my %p = ( extension => '%args' );
 if ( ref $_[-1] and ref $_[-1] eq 'HASH' ) {
 %p = ( %p, %{ pop @_ } );
 }

my @args_list;
 for ( @_ ) {
 my $args_file = join( '.', $_, $p{extension} );
 next unless -r $args_file;
 unshift @args_list, $args_file;
 }

my $args_from_file = Config::Any->load_files({
 files => \@args_list, use_ext => 0, flatten_to_hash => 1,
 force_plugins => [ map{"Config::Any::${_}"} qw/YAML::Tiny JSON/ ],
 });

if ( my @err = grep{ not exists $args_from_file->{$_} } @args_list ) {
 die "Error while loading args files: @err";
 }

my $args = merge( @{$args_from_file}{ @args_list } );

_inflate_args() for grep{ ref } values %$args;

return $args;
}
sub load_config_from {
 my $file = shift;

return {} unless -e "$file";

my $conf = Config::Any->load_files({
 files => [ "$file" ], use_ext => 0, flatten_to_hash => 1,
 force_plugins => [ map{"Config::Any::${_}"} qw/YAML::Tiny JSON/ ],
 });

die "Invalid config file: ${file}" unless values %$conf;

return [ values %$conf ]->[0];
}
1;
}
BEGIN { $INC{q{MME/Plugin/Args/JSON.pm}} = 1;
package MME::Plugin::Args::JSON;
use strict;
use warnings;
use base qw(HTML::Mason::Plugin);
use MME::Util;
sub start_component_hook {
 my ($self, $context) = @_;
 my $comp = $context->comp;
 $context->request->notes( '__mp_aj_modified_comps', {} )
 unless defined $context->request->notes( '__mp_aj_modified_comps' );
 return if $comp->is_subcomp;
 return if exists $context->request->{wrapper_index}{ $comp->comp_id };
 return unless %{ $comp->declared_args };

return unless $comp->can( 'source_file' );

my $sargs = MME::Util::load_args_for( $comp->source_file );
 $context->request->notes( '__mp_aj_modified_comps' )->{ $comp } = 1;

unshift @{ $context->args }, %{ $sargs };
}
sub end_component_hook {
 my ( $self, $context ) = @_;
 my $comp = $context->comp;
 delete $context->request->notes( '__mp_aj_modified_comps' )->{$comp}
 or return;

my $sargs = MME::Util::load_args_for( $comp->source_file );
 splice @{ $context->request->{args} }, 0, scalar @{[%$sargs]};
}
sub end_request_hook {
 my ( $self, $context ) = @_;
 $context->request->notes( '__mp_aj_modified_comps', undef )
}
1;
}
package MME::Server;
use strict;
use warnings;
use HTML::Mason::Interp;
use HTTP::Server::Brick;
use HTTP::Status;
use JSON;
use Carp qw/carp confess/;
use Path::Class qw/dir file/;
use FindBin qw/$Bin/;
use MME::Plugin::Args::JSON;
use Getopt::Long;
use Sys::Hostname;
use Cwd;
use Data::Dumper;
use URI::QueryParam;
use MME::Util;
sub print_help {
 print <<HELP;
$0 - The Mason Mockup Engine

MME is a single-file, pure-perl webserver primarily intended to be used by
HTML coders during template development. Great care has been taken to make
usage as unintrusive as possible, all you need is perl 5.8.1+ installation on
(hopefully) any platform that is supported by perl (at least Linux, Mas OS X
and Strawberry Perl on WinXP should work out of the box).

TEMPLATES:
Templates are handled by HTML::Mason 1.42 (without dhandler support) but
currently there is no way to influence parameters, so you are stuck with
defaults for autohandler_name and so on.

ARGS EMULATION:
Neither body nor query parameters are passed on to the templates (give me a
holler when you need that), instead you can place an [component_file].\%args
file beside any component file to specify (additional) args passed to it.
You are free to use either JSON (parsed by JSON::PP) or YAML (parsed by
YAML::Tiny) format for this file. The args are filtered through
Data::AsObject, so you can easily say:

  <\%args> \$my_value </\%args>
  Value: <\% \$my_value->deep->deep->magic \%>
  # for ( \$my_value->members ) { \$m->print( \$_ ) }

Component args take precedence over autohandler args in the request component.

SECURITY CONSIDERATIONS:
Never EVER use this for any purpose other than local template development.
I don't think it will eat kittens if left alone, but on the other hand, I
didn't spend ANY time trying to figure out if it might. You have been warned!

UNSUPPORTED MASON FEATURES:
  - dhandler's (but you wouldn't want to explain them to your layouters anyways)

USAGE:
  $0 [--debug] [--port PORT] [--static DIR]+ [ROOT]
  $0 --help
  $0 --usage

    ROOT
       Mason comp_root to serve by this instance (defaults to current dir)

    -s DIR | --static DIR
       List of directories served directly without intervention by HTML::Mason
       When no static dirs are given, the default [css,js,gfx,img,static] is used

    -p PORT | --port PORT
       Port where the webserver listens for requests

    -h | --help
       Print this message and exit

    --usage
       Print terse usage info and exit

    --debug
       Log some additional information about the request

DEVELOPMENT
  Currently, this program does exactly what I need, so expect no further
  development in terms of features. If you have an itch to scratch, I'll
  happily accept patches (or even better pull requests), though.

  I won't bother to build stand-alone binaries (with embedded perl), but
  if anyone will, I'll find a place to host them.

  The repository resides at: http://github.com/willert/mason-mockup-engine

  Have fun!

HELP
}
sub print_usage {
 print <<USAGE;
$0 - The Mason Mockup Engine

Usage:
  $0 [--debug] [--port PORT] [--static DIR]+ ROOT
  $0 --help
  $0 --usage

USAGE
}
sub run {
 GetOptions( \ my %p, 'static|s=s@', 'port|p=i', 'debug', 'help|h', 'usage' )
 or do{ print_usage() and exit 1 };

do{ print_help() and exit } if $p{help} or @ARGV > 1;

$p{port} ||= 3000;

die "Could not serve more than one Mason directory" if @ARGV > 1;

$p{root} = $ARGV[0] if @ARGV == 1;

if ( not $p{root} ) {
 my $default = dir( $Bin );
 if ( -d $default->subdir( 'root' )->stringify ) {
 $p{root} = $default->subdir( 'root' )->stringify;
 } elsif ( -d $default->parent->subdir( 'root' )->stringify ) {
 $p{root} = $default->parent->subdir( 'root' )->stringify;
 }
 }

die "Root directory not given and no default directory 'root' found"
 unless defined $p{root};

die "Root directory $p{root} doesn't exists" unless -d $p{root};

$p{static} = [qw/css js gfx img static/] unless $p{static};

my $comp_root = dir( $p{root} )->absolute;
 my @static = map{
 die "Please specify static directories as relative URI paths"
 if /^\// or /\\/;
 $comp_root->subdir( split '/', $_ );
 } @{ $p{static} };

my $local_args = MME::Util::load_config_from(
 $comp_root->file('config.%inc')
 );

my $interp = HTML::Mason::Interp->new(
 use_object_files => 0,
 comp_root => $comp_root->stringify,
 code_cache_max_size => 0,
 plugins => [qw/MME::Plugin::Args::JSON/],
 preamble => <<'MASON',
        $m->comp( $m->current_comp->name . '.%inc', params => \@_ )
          if $m->comp_exists( $m->current_comp->name . '.%inc' );
MASON
 %{ $local_args },
 );

my $server = HTTP::Server::Brick->new(
 port => $p{port},
 daemon_args => [ Timeout => 0.3 ],
 );

$server->mount( '/' => {
 handler => sub {
 my ($req, $res) = @_;
 $req->{path_info} ||= q{};

my $comp_path = $comp_root->file(
 grep{$_} split '/', $req->{path_info}
 );
 $comp_path = dir( $comp_path )->file( 'index.html' )
 if -d $comp_path->stringify;

if ( not -f $comp_path->stringify ) {
 $comp_path =~ s/\.html$//;
 $comp_path = file( $comp_path );
 }

my $comp;
 for ( $comp_path, dir( $comp_path )->file( 'index' ) ) {
 $comp = eval{ $interp->load(
 '/' . $comp_path->relative($comp_root)->as_foreign('Unix')
 )};
 if ( my $err = $@ ) {
 $res->content_type( 'text/plain' );
 $res->add_content( "Internal server error:\n\n" . $err );
 return 1;
 }
 }

if ( not $comp ) {
 warn "Can't find component for path $comp_path" if $p{debug};
 $res->code( RC_NOT_FOUND );
 return 1;
 }

my @call_chain;
 my $current_comp = $comp;
 while ( my $parent = $current_comp->parent ) {
 next unless $parent->is_file_based;
 my $path = file( $parent->source_file );
 unshift @call_chain, $path if $comp_root->contains( $path );
 } continue {
 $current_comp = $current_comp->parent
 }

print STDERR "Call chain is: @call_chain\n" if $p{debug};

my $args = MME::Util::load_args_for( @call_chain );

printf STDERR "Args: %s\n", Dumper( $args ) if $p{debug};

my $mason_request = $interp->make_request(
 comp => $comp,
 args => [ %{ $req->uri->query_form_hash }, %$args ],
 out_method => sub{ $res->add_content( @_ ) },
 );

eval{ $mason_request->exec };

if ( my $err = $@ ) {
 $res->content_type( 'text/plain' );
 $res->add_content( "Internal server error:\n\n" . $err );
 return 1;
 }

1;
 },
 wildcard => 1,
 });

for my $static_dir ( @static ) {
 my $uri = $static_dir->relative($comp_root)->as_foreign('Unix');
 my $path = $comp_root->subdir($static_dir);
 $server->mount( "/$uri" => { path => "$static_dir", wildcard => 1 } );
 }

$server->_log( error => "Serving Mason components in $comp_root" );
 $server->start;
}
1;

  MME::Server->run();

