#!/usr/bin/env perl

use strict;
use warnings;

use HTML::Mason::Interp;

use HTTP::Server::Brick;
use HTTP::Status;

use JSON;
use Carp qw/carp confess/;

use Path::Class qw/ dir file /;

use MME::Plugin::Args::JSON;
use Getopt::Long;
use Sys::Hostname;

use Cwd;

use Data::Dumper;

use MME::Util;

sub run {
  GetOptions( \ my %p, 'static=s@', 'port=i', 'debug' )
    or die "Invalid parms";

  $p{port} ||= 3000;

  die "Could not serve more than one Mason directory" if @ARGV > 1;

  $p{root} = @ARGV == 1 ? $ARGV[0] : getcwd();
  die "Root directory $p{root} doesn't exists" unless -d $p{root};

  $p{static} = [qw/ css js gfx static /] unless $p{static};

  my $comp_root = dir( $p{root} )->absolute;
  my @static = map{
    die "Please specify static directories as relative URI paths"
      if /^\// or /\\/;
    $comp_root->subdir( split '/', $_ );
  } @{ $p{static} };

  my $interp = HTML::Mason::Interp->new(
    use_object_files    => 0,
    comp_root           => $comp_root->stringify,
    code_cache_max_size => 0,
    plugins             => [qw/ MME::Plugin::Args::JSON /],
  );

  my $server = HTTP::Server::Brick->new( port => $p{port} );

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

      my $comp = $interp->load( '/' . $comp_path->relative( $comp_root ) );
      if ( not $comp ) {
        warn "Can't find component for path $comp_path" if $p{debug};
        $res->code( RC_NOT_FOUND );
        return 1;
      }

      my @call_chain;
      my $current_comp = $comp_path;
      while ( $comp_root->contains( $current_comp ) ) {
        unshift @call_chain, $current_comp;
      } continue {
        $current_comp = $current_comp->basename eq 'autohandler'
          ? $current_comp->dir->parent->file('autohandler')
            : $current_comp->dir->file('autohandler');
      }

      my $args = MME::Util::load_args_for( @call_chain );

      printf STDERR "Args: %s\n", Dumper( $args ) if $p{debug};

      my $mason_request = $interp->make_request(
        comp => $comp,
        args => [ %$args ],
        out_method => sub{ $res->add_content( @_ ) },
      );

      $mason_request->exec;

      # my %deps = %INC;
      # foreach my $mod ( keys %deps ) {
      #   delete $deps{ $mod } unless $deps{ $mod } =~ m|\bdeps/|;
      # }
      # $server->_log( error => sprintf(
      #   "Loaded modules:\n%s", Dumper( \%deps )
      # )) if $p{debug};

      1;
    },
    wildcard => 1,
  });

  for my $static_dir ( @static ) {
    my $uri = $static_dir->relative($comp_root)->as_foreign('Unix');
    $server->mount( $uri => { path => $comp_root } );
  }

  $server->_log( error => "Serving Mason components in $comp_root" );


  # start accepting requests (won't return unless/until process
  # receives a HUP signal)
  $server->start;

}

1;
