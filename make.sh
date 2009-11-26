echo 'use 5.8.4;

BEGIN{
  $INC{q{Params/ValidatePP.pm}} = 1;
  $INC{q{Data/AsObject/Array.pm}} = 1;
  $INC{q{Data/AsObject/Hash.pm}} = 1;
  $INC{q{HTML/Mason.pm}} = 1;
  $INC{q{Path/Class/File.pm}} = 1;
  $INC{q{Path/Class/Dir.pm}} = 1;
  $ENV{PERL_JSON_BACKEND} = "JSON::PP";
  $DB::sub;
}' > mme.pl

for X in \
  Params/Validate.pm \
  Params/ValidatePP.pm \
  Path/Class/Entity.pm \
  Path/Class/File.pm \
  Path/Class/Dir.pm \
  Path/Class.pm \
  AutoLoader.pm \
  Class/Container.pm \
  Class/Data/Inheritable.pm \
  Config/Any.pm \
  Config/Any/Base.pm \
  Config/Any/JSON.pm \
  Data/AsObject.pm \
  Data/AsObject/Array.pm \
  Data/AsObject/Hash.pm \
  Devel/StackTrace.pm \
  Exception/Class/Base.pm \
  Exception/Class.pm \
  File/Temp.pm \
  HTML/Entities.pm \
  HTML/Mason/MethodMaker.pm \
  HTML/Mason/Exceptions.pm \
  HTML/Mason/Tools.pm \
  HTML/Mason/Utils.pm \
  HTML/Mason/Escapes.pm \
  HTML/Mason/Cache/BaseCache.pm \
  HTML/Mason/Plugin/Context.pm \
  HTML/Mason/Request.pm \
  HTML/Mason/ComponentSource.pm \
  HTML/Mason/Resolver.pm \
  HTML/Mason/Resolver/File.pm \
  HTML/Mason/Interp.pm \
  HTML/Mason.pm \
  HTML/Mason/Component.pm \
  HTML/Mason/Component/FileBased.pm \
  HTML/Mason/Component/Subcomponent.pm \
  HTML/Mason/Lexer.pm \
  HTML/Mason/Compiler.pm \
  HTML/Mason/Compiler/ToObject.pm \
  HTML/Mason/Plugin.pm \
  URI/Escape.pm \
  URI.pm \
  URI/_query.pm \
  URI/_generic.pm \
  URI/_server.pm \
  URI/_punycode.pm \
  URI/_idna.pm \
  URI/http.pm \
  HTTP/Date.pm \
  HTTP/Status.pm \
  HTTP/Headers.pm \
  HTTP/Message.pm \
  HTTP/Request.pm \
  HTTP/Response.pm \
  LWP/MediaTypes.pm \
  HTTP/Daemon.pm \
  HTTP/Server/Brick.pm \
  Hash/Merge/Simple.pm \
  JSON/PP58.pm \
  JSON/PP.pm \
  JSON.pm \
  YAML/Tiny.pm ;
do
    echo 'BEGIN { $INC{q{'$X'}} = 1;'
    cat deps/$X |
      perl -ni -e ' 1 .. /__(DATA|END)__/ ? /__(DATA|END)__/ ? () : print : (); '
    echo '}\n\n'
done >> mme.pl

for X in Config/Any/YAML/Tiny.pm MME/Util.pm MME/Plugin/Args/JSON.pm ; do
    echo 'BEGIN { $INC{q{'$X'}} = 1;'
    cat lib/$X | perl -ni -e \
        ' 1 .. /__(DATA|END)__/ ? /__(DATA|END)__/ ? () : print : (); '
    echo '}\n\n'
done >> mme.pl

cat lib/MME/Server.pm >> mme.pl

perl -MPerl::Squish -e 'Perl::Squish->file("mme.pl")';

# restore shebang that was dropped while squishing
( echo '#!/usr/bin/perl'; cat mme.pl ) | sponge mme.pl

echo '
  MME::Server->run();
' >> mme.pl

perl -MFile::Slurp -e 'my $code = File::Slurp::read_file( "mme.pl" ); $code =~ s|
package HTML::Mason::Plugin::Context;
use strict;
use warnings;
||;File::Slurp::write_file( "mme.pl", $code );'

chmod a+x mme.pl