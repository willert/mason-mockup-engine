rm -Rf deps
cp -av local/lib/perl5/ deps
cd deps/
rm -Rf x86_64-linux-gnu-thread-multi/ PAR.pm CPAN* UNIVERSAL/ Archive/ local/ Test \
  Module/ Spiffe* Squirrel* Mouse* Bundle/ Any/ Getopt/ArgvFile.pm TAP/ Task/ App/
find . -name '*pod' | xargs rm


ls | grep -v '^[A-Z]\|data\|file.pm\|Heuristic\|http.pm' | xargs rm -Rf
