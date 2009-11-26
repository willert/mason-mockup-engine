package Config::Any::YAML::Tiny;

use strict;
use warnings;

use base 'Config::Any::Base';

=head1 NAME

Config::Any::YAML - Load YAML config files

=head1 DESCRIPTION

Loads YAML files. Example:

    ---
    name: TestApp
    Controller::Foo:
        foo: bar
    Model::Baz:
        qux: xyzzy


=head1 METHODS

=head2 extensions( )

return an array of valid extensions (C<yml>, C<yaml>).

=cut

sub extensions {
    return qw( yml yaml );
}

=head2 load( $file )

Attempts to load C<$file> as a YAML file.

=cut

sub load {
    my $class = shift;
    my $file  = shift;

    require YAML::Tiny;
    return YAML::Tiny->read( $file )->[0];
}

=head2 requires_any_of( )

Specifies that this modules requires one of L<YAML::Syck> (0.70) or L<YAML> in
order to work.

=cut

sub requires_any_of { 'YAML::Tiny' }

=head1 AUTHOR

Brian Cassidy E<lt>bricas@cpan.orgE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright 2007 by Brian Cassidy

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=head1 SEE ALSO

=over 4

=item * L<Catalyst>

=item * L<Config::Any>

=item * L<YAML>

=item * L<YAML::Syck>

=back

=cut

1;
