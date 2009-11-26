#!/usr/bin/perl

# filter takes a filename as arg (otherwise stdin) and sends
# pod-stripped content to stdout.

use Pod::Parser;
package MyParser;
use base Pod::Parser;

sub initialize {
  my $self = shift;
  $self->parseopts(-want_nonPODs=>1);
}
sub preprocess_paragraph {
    my ($self, $text, $line_num) = @_;
    print $text if $self->cutting;
    return $text;
}

package main;

my $parser = new MyParser();
open my $null, '> /dev/null';
$parser->parse_from_filehandle( undef, $null );
