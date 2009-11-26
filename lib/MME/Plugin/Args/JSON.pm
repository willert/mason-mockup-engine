package MME::Plugin::Args::JSON;

use strict;
use warnings;

use base qw(HTML::Mason::Plugin);

=head1 NAME

MME::Plugin::Args::JSON

=head1 SYNOPSIS

 use MME::Plugin::Args::JSON;
 my $foo = MME::Plugin::Args::JSON->new();

=head1 DESCRIPTION

B<MME::Plugin::Args::JSON> scans the component
hierarchy for any accompanying C<'.json'> files,
loads and merges them and pass them on as component
args.

=cut

use MME::Util;

sub start_component_hook {
  my ($self, $context) = @_;
  my $comp = $context->comp;

  # prepare the notes we will use during this request
  $context->request->notes( '__mp_aj_modified_comps', {} )
    unless defined $context->request->notes( '__mp_aj_modified_comps' );

  # ignore subcomponents
  return if $comp->is_subcomp;

  # components in the call chain already
  # get all request args and thus the stash
  return if exists $context->request->{wrapper_index}{ $comp->comp_id };

  # do nothing if the resp. conponent isn't
  # interested in named arguments
  return unless %{ $comp->declared_args };

  return unless $comp->can( 'source_file' );

  my $sargs = MME::Util::load_args_for( $comp->source_file );

  # mark this component so we can restore the arg list later on
  $context->request->notes( '__mp_aj_modified_comps' )->{ $comp } = 1;

  unshift @{ $context->args }, %{ $sargs };

}

sub end_component_hook {
  my ( $self, $context ) = @_;
  my $comp = $context->comp;

  # fetch and unset components modification status from
  # request notes, return if there is nothing to do
  delete $context->request->notes( '__mp_aj_modified_comps' )->{$comp}
   or return;

  my $sargs = MME::Util::load_args_for( $comp->source_file );

  # remove stash from argument list (this is fatal if a component
  # has mucked around with the aliased @_ while also using named
  # arguments, but anyone doing this doesn't deserve better)
  splice @{ $context->request->{args} }, 0, scalar @{[%$sargs]};

}

sub end_request_hook {
  my ( $self, $context ) = @_;
  # in theory these should die with the current request, but I
  # still want to unset them explicitly here to avoid potential
  # circular references
  $context->request->notes( '__mp_aj_modified_comps', undef )
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

