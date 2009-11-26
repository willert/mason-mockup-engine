package MME::Util;

=head1 NAME

MME::Tools

=head1 SYNOPSIS

 use MME::Tools;
 my $foo = MME::Tools->new();

=head1 DESCRIPTION

B<MME::Tools> is a collection of utility functions for MME

=cut

use strict;
use warnings;

use Config::Any;
use Hash::Merge::Simple qw/merge/;
use Data::AsObject;

sub load_args_for {

  my @args_list;
  for ( @_ ) {
    my $args_file = $_ . '.%args';
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

  $_ = Data::AsObject::dao( $_ ) for grep{ ref } values %$args;

  return $args;
}

1;

__END__

=head1 SEE ALSO

L<perl>

=head1 AUTHOR

Sebastian Willert, C<willert@gmail.com>

=head1 COPYRIGHT

This program solely owned by its author. Redistribution is prohibited.

=cut

