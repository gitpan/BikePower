use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile
  ('NAME'	   => 'BikePower',
   'VERSION_FROM' => 'BikePower.pm', # finds $VERSION
   'EXE_FILES'    => ['bikepwr', 'tkbikepwr'],
   'dist'      => {'COMPRESS'=>'gzip -9f', 'SUFFIX' => '.gz',
		   'POSTOP'=>'-$(CHMOD) 644 $(DISTVNAME).tar$(SUFFIX)'},
   'PREREQ_PM' => {'Tk'             => 402.002,
		   'Data::Dumper'   => 2.08,
		   'Tk::FileDialog' => 0,
		   'Tk::Pod'        => 0,
		  },
  );

sub MY::postamble {
    if (is_in_path("strpp")) {
	'all:: bikepower.de.html bikepower.en.html

bikepower.de.html bikepower.en.html: bikepower.html.spp wwwdefaults.spp
	-chmod +w bikepower.de.html bikepower.en.html
	strpp --dir=. bikepower.html.spp
	chmod -w bikepower.de.html bikepower.en.html

';
    } else {
	'';
    }
}

sub is_in_path {
    my($prog) = @_;
    foreach (split(/:/, $ENV{PATH})) {
        return $_ if -x "$_/$prog";
    }
    undef;
}
