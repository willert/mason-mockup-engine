package MME::Server;

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


USAGE:
  $0 [--debug] [--port PORT] [--static DIR]+ [ROOT]
  $0 --help
  $0 --usage

    ROOT
       Mason comp_root to serve by this instance (defaults to current dir)

    -s DIR | --static DIR
       List of directories served directly without intervention by HTML::Mason
       When no static dirs are given, the default [css,js,gfx,static] is used

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

  do{ print_help() and exit } if $p{help};

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
