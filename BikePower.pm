#!/usr/local/bin/perl
# -*- perl -*-

#
# $Id: BikePower.pm,v 2.5.1.3 1998/05/22 19:05:26 eserte Exp $
# Author: Slaven Rezic
#
# Copyright: see at bottom of file
#
# Mail: eserte@cs.tu-berlin.de
# WWW:  http://user.cs.tu-berlin.de/~eserte/
#

package BikePower;

use strict;
use vars qw($m_s__per__mi_h $m_s__per__km_h $Nt__per__lb $kg__per__Nt
	    $Watts__per__Cal_hr $Watts__per__horsepower
	    $NOSAVE
	    @out %fmt @air_density %members
	    @tk_interfaces %icons
	    $VERSION
 	   );

$VERSION = '0.13';

# Conversion factors
$m_s__per__mi_h         = 0.44704; # meters/second per miles/hour
$m_s__per__km_h         = (1000.0 / 3600.0); # m/s per km/h
$Nt__per__lb            = 4.4482;
$kg__per__Nt            = 0.102;
$Watts__per__Cal_hr     = 1.163; # Watts per dietary Calories/hour
# 1 dietary Calorie == 1000 calories
$Watts__per__horsepower = 745.700;

$NOSAVE = 1 << 0;

# Air Density table for dry air between -30 to +44 degrees C
#
# Taken from the Handbook of Chemistry and Physics, Thirtieth
# Edition
#
# This table does not include the changes for air pressure or
# humity.
@air_density =
  (1.5147, 1.5083, 1.5019, 1.4955, 1.4892, # -30°C 3.6°F
   1.4829, 1.4767, 1.4706, 1.4645, 1.4584, # -25 
   1.3951, 1.3896, 1.3841, 1.3787, 1.3734, # -20 
   1.3680, 1.3628, 1.3575, 1.3523, 1.3472, # -15 
   1.3420, 1.3370, 1.3319, 1.3269, 1.3219, # -10 
   1.3170, 1.3121, 1.3072, 1.3024, 1.2977, # - 5 
   1.2929, 1.2882, 1.2835, 1.2789, 1.2742, #   0 
   1.2697, 1.2651, 1.2606, 1.2561, 1.2517, #   5 
   1.2472, 1.2428, 1.2385, 1.2342, 1.2299, #  10 
   1.2256, 1.2214, 1.2171, 1.2130, 1.2088, #  15 
   1.2047, 1.2006, 1.1965, 1.1925, 1.1885, #  20 
   1.1845, 1.1805, 1.1766, 1.1727, 1.1688, #  25 
   1.1649, 1.1611, 1.1573, 1.1535, 1.1498, #  30 
   1.1460, 1.1423, 1.1387, 1.1350, 1.1314, #  35 
   1.1277, 1.1242, 1.1206, 1.1170, 1.1135  #  40°C 138.6°F 
  );

# members
# maybe accessed by hash or by method
%members =
  ('imperial'    => [[], 0, 'metric/imperial flag', undef],
   'T_a'         => [['temperature'], 20, 'temperature [°C]', undef],
   'given'       => [[], 'v', 'resolve for v/P/C', undef],
   'first_C'     => [[], 500, 'consumption [cal/h]', undef],
   'first_V'     => [[], 30,  'velocity [km/h]', undef],
   'first_P'     => [[], 200, 'power output [Watts]', undef],
   'V_incr'      => [[], 2,   'velocity increment in table', undef],
   'P_incr'      => [[], 25,  'power increment in table', undef],
   'C_incr'      => [[], 100.0, 
		     'consumed_power increment in table', undef],
   'N_entry'     => [[], 10,  'number of entries in table', undef],
   'C_a'         => [[], 0.90, 'air resistance coefficient', undef],
   'A1'          => [[], 0,   'linear coefficient of air resistance', undef],
   'A2'          => [[], undef,
		     'quadratic coefficient of air resistance', $NOSAVE],
   'A_c'         => [[], 0.3080527, 'frontal area of the cyclist in meters^2',
		     undef],
   'T'           => [['transmission_efficiency'], 0.95,
		     'transmission efficiency of bicycle drivetrain', undef],
   'E'           => [['human_efficiency'], 0.249,
		     'efficiency of human in cycling', undef],
   'H'           => [['headwind'], 0.0,
		     'velocity of headwind [meters/second]', undef],
   'R'           => [['rolling_friction'], 0.0047,
		     'coefficient of rolling friction', undef],
   'G'           => [['grade'], 0, 'grade of hill', undef],
   'Wc'          => [['weight_cyclist'], 77, 'weight of cyclist [kg]', undef],
   'Wm'          => [['weight_machine'], 10,
		     'weight of machine and clothing [kg]', undef],
   'BM_rate'     => [[], 1.4, 
		     'basal metabolism rate [Watts/kg of body weight]', undef],
   'cross_wind'  => [[], 0, 'the wind given is a cross wind', undef],

   'P'           => [['power'], undef, 'power in Watts', $NOSAVE],
   'V'           => [['velocity'], undef, 'velocity in m/s', $NOSAVE],
   'C'           => [['consumption'], undef, 'consumption in Cal/hr', $NOSAVE],
  );

my $member;
my $i=0;
foreach $member (keys %members) {
    foreach ($member, @{$members{$member}->[0]}) {
	my $sub = q#sub # . $_ . q# {
    my($self, $val) = @_;
    if (defined $val) {
	$self->{'# . $member . q#'} = $val;
    } else {
	$self->{'# . $member . q#'};
    }
}#;
        eval $sub;
    }
}

# output variables
@out = qw(V F Pa Pr Pg Pt P hp heat B C kJh);
# format for printf
%fmt = qw(V	%5.1f
	  F	%4.1f
	  Pa	%4.0f
	  Pr	%4.0f
	  Pg	%5.0f
	  Pt	%4.0f
	  P	%5.0f
	  hp	%5.2f
	  heat	%5.0f
	  B	%3.0f
	  C	%5.0f
	  kJh	%5.0f);

## XXX vielleicht sollten die Werte im Hash die echten SI-Werte
## sein. FETCH und STORE machen dann anhand von metric die
## Umwandlung. Funktioniert es mit -variable und Tk?
sub TIEHASH {
    my($class, %a) = @_;

    my $s = {};
    bless $s, $class;

    if ($a{'-no-ini'} || !$s->load_defaults) {
	if (!$a{'-no-default'}) {
	    $s->default;
	}
    }
    $s->set_values(%a);
    if (!$s->given && !$a{'-no-default'}) { $s->given('v') }

    $s;
}

sub new { shift->TIEHASH(@_) }

sub _nosave {
    my($k) = @_;
    return 1 if !exists $members{$k};
    my $v = $members{$k};
    return 0 if !defined $v->[3];
    return $v->[3] & $NOSAVE;
}

sub clone {
    my($class, $old, %args) = @_;
    $args{'-no-ini'} = $args{'-no-default'} = 1;
    my $new = $class->new(%args);
    my($k, $v);
    while(($k, $v) = each %members) {
	next if _nosave($k) || $old->{$k} eq '';
	$new->{$k} = $old->{$k};
    }
    $new;
}

sub default {
    my($self) = @_;
    my($k, $v);
    while(($k, $v) = each %members) {
	next if _nosave($k);
	$self->{$k} = $v->[1];
    }
    $self;
}

sub set_values {
    my($self, %a) = @_;
    my($k, $v);
    while(($k, $v) = each %a) {
	if ($k !~ /^[_\-]/ && defined $v && $v ne '') {
	    $self->{$k} = $v;
	}
    }
}

sub _default_filename {
    my $home = eval { (getpwuid($<))[7] } || $ENV{'HOME'} || '';
    $home . ($^O eq 'MSWin32' ? "/bikepwr.pl" : "/.bikepower.pl");
}

sub load_defaults {
    my($self, $file) = @_;
    $file = _default_filename unless $file;
    my $x;
    if (! -r $file) {
	return undef;
    }
    eval 'do "$file"';
    if ($@) {
	warn $@;
	return undef;
    }
    my($k, $v);
    while(($k, $v) = each %$x) {
	$self->{$k} = $v;
    }
    1;
}

sub save_defaults {
    my($self, $file) = @_;
    $file = _default_filename unless $file;
    my $x;
    my($k, $v);
    while(($k, $v) = each %$self) {
	if ($k !~ /^[_\-]/ && !_nosave($k) && $v ne '' ) {
	    $x->{$k} = $v;
	}
    }
    if (!open(FILE, ">$file")) {
	warn "Can't open file: $!";
	return undef;
    }
    eval { require Data::Dumper };
    if (!$@) {
	print FILE Data::Dumper->Dump([$x], ['x']), "\n";
    } else {
	print FILE "$x = {\n";
	while(($k, $v) = each %$x) {
	    print FILE $k, "=> '", $v, "'\n";
	}
	print FILE "}\n";
    }
    close FILE;
    1;
}

sub FETCH {
    my($self, $key) = @_;
    $self->{$key};
}

sub STORE {
    my($self, $key, $value) = @_;
    $self->{$key} = $value;
}

sub _numify {
    my($s) = @_;
    if ($s =~ /^\s*(\S+)/) {
	$1;
    } else {
	$s;
    }
}

sub weight_cyclist_N { $_[0]->weight_cyclist / $kg__per__Nt }
sub weight_machine_N { $_[0]->weight_machine / $kg__per__Nt }
sub total_weight     { $_[0]->weight_cyclist + $_[0]->weight_machine }
sub total_weight_N   { $_[0]->weight_cyclist_N + $_[0]->weight_machine_N }
sub velocity_kmh     { $_[0]->velocity / $m_s__per__km_h }
sub V_incr_ms        { $_[0]->V_incr * $m_s__per__km_h }
sub air_density      { $air_density[int($_[0]->temperature + 30)] }
sub calc_A2          {
    my $self = shift;
    if (defined $self->A2) {
	$self->A2;
    } else {
	($self->C_a * $self->air_density / 2) * _numify($self->A_c);
    }
}
sub BM               { $_[0]->BM_rate  * $_[0]->weight_cyclist }
sub C_incr_W_cal_hr  { $_[0]->C_incr * $Watts__per__Cal_hr }

sub sqr { $_[0] * $_[0] }

sub calc {
    my $self = shift;
    # effective Headwind
    my $eff_H = $self->headwind * ($self->cross_wind ? .7 : 1);
    my $A_c   = _numify($self->A_c);
    my $R     = _numify($self->rolling_friction);
    my $A2    = $self->calc_A2;
    my($F_a);

    if ($self->given eq 'P' || $self->given eq 'C') {
	# Given P, solve for V by bisection search
	# True Velocity lies in the interval [V_lo, V_hi].
	my $P_try;
	my $V_lo = 0;
	my $V    = 64;
	my $V_hi = 128;
	while ($V - $V_lo > 0.001) {
	    $F_a = $A2 * sqr($V + $eff_H) + $self->A1 * ($V + $eff_H);
	    if ($V + $eff_H < 0) {
		$F_a *= -1;
	    }
	    $P_try = ($V / $self->transmission_efficiency) 
	      * ($F_a + ($R + $self->grade) * $self->total_weight_N);
	    if ($P_try < $self->power) {
		$V_lo = $V;
	    } else {
		$V_hi = $V;
	    }
	    $V = 0.5 * ($V_lo + $V_hi);
	}
	$self->velocity($V);
    }
    
    # Calculate the force (+/-) of the air
    $F_a = $A2 * sqr($self->velocity + $eff_H) 
      + $self->A1 * ($self->velocity + $eff_H);
    if ($self->velocity + $eff_H < 0) {
	$F_a *= -1;
    }

    # Calculate the force or rolling restance
    my $F_r  =  $R * $self->total_weight_N;

    # Calculate the force (+/-) of the grade
    my $F_g  =  $self->grade * $self->total_weight_N;

    # Calculate the total force
    my $F  =  $F_a + $F_r + $F_g;

    # Calculate Power in Watts
    $self->power($self->velocity * $F / $self->transmission_efficiency);

    my $P_t;
    # Calculate Calories and drivetrain loss
    if ($self->power > 0) {
	$self->consumption($self->power / $self->human_efficiency + $self->BM);
	$P_t  =  (1.0 - $self->transmission_efficiency) * $self->power;
    } else {
	$self->consumption($self->BM);
	$P_t  =  0.0;
    }

    $self->{_out}{'Pa'}   = $self->velocity * $F_a;
    $self->{_out}{'Pr'}   = $self->velocity * $F_r;
    $self->{_out}{'Pg'}   = $self->velocity * $F_g;
    $self->{_out}{'Pt'}   = $P_t;
    $self->{_out}{'P'}    = $self->power;
    $self->{_out}{'hp'}   = $self->power / $Watts__per__horsepower;
    $self->{_out}{'heat'} = $self->consumption - ($self->BM + $self->power);
    $self->{_out}{'C'}    = $self->consumption;
    $self->{_out}{'B'}    = $self->BM;
    if (!$self->imperial) {
	$self->{_out}{'V'}    = $self->velocity_kmh;
	$self->{_out}{'F'}    = $kg__per__Nt * $F;
#	$self->{_out}{'kJh'}  = (3600.0 / 1000.0) * $self->consumption;
	$self->{_out}{'kJh'}  = $self->consumption / $Watts__per__Cal_hr; # really Cal/hr
    } else {
	$self->{_out}{'V'}    = $self->velocity / $m_s__per__mi_h;
	$self->{_out}{'F'}    = $F / $Nt__per__lb;
	$self->{_out}{'kJh'}  = $self->consumption / $Watts__per__Cal_hr; # really Cal/hr
    }
}

sub display_parameters {
    my($self) = @_;
    if (!$self->imperial) {
	printf
	  "grade of hill = %5.1f%%                    headwind = %4.1f km/h\n",
	  100.0 * $self->grade, $self->headwind / $m_s__per__km_h;
	printf
	  "weight:  cyclist %5.1f + machine %4.1f  =  total %5.1f kg\n",
	  $self->weight_cyclist, $self->weight_machine, $self->total_weight;
    } else {
	printf
	  "grade of hill = %5.1f%%                    headwind = %4.1f mi/h\n",
	  100.0 * $self->grade, $self->headwind / $m_s__per__mi_h;
	printf
	  "weight:  cyclist %5.1f + machine %4.1f  =  total %5.1f lb\n"; # XXX
    }
    printf
      "rolling friction coeff = %6.4f           BM rate = %5.2f W/kg\n",
      _numify($self->rolling_friction), $self->BM_rate;
    printf
      "air resistance coeff =  (%6.4f, %g)\n",
      $self->calc_A2, $self->A1;
    printf
      "efficiency:  transmission = %5.1f%%        human = %4.1f%%\n",
      100.0 * $self->transmission_efficiency,
      100.0 * $self->human_efficiency;
    print "\n";
}

sub _init_output {
    my($self) = @_;
    if ($self->given eq 'C') {
	$self->power($self->human_efficiency * 
		     ($self->first_C * $Watts__per__Cal_hr - $self->BM));
	$self->P_incr($self->human_efficiency * $self->C_incr_W_cal_hr);
    } elsif ($self->given eq 'P') {
	$self->power($self->first_P);
    } else {
	$self->velocity($self->first_V * $m_s__per__km_h); # m/s
    }
}

sub _incr_output {
    my($self) = @_;
    if ($self->given eq 'P' || $self->given eq 'C') {
	$self->power($self->power + $self->P_incr);
    } else {
	$self->velocity($self->velocity + $self->V_incr_ms);
    }
}

sub output {
    my($self) = @_;

    if (!$self->imperial) {
	print
	  "  kph  F_kg   P_a  P_r   P_g  P_t    P    hp   heat   " .
#	    "BM     C    kJ/hr \n";
	    "BM     C    Cal/hr\n";
    } else {
	print
	  "  mph  F_lb   P_a  P_r   P_g  P_t    P    hp   heat   " .
	    "BM     C    Cal/hr\n";
    }
    my $entry;
    for ($entry = 0; $entry < $self->N_entry; $entry++) {
	$self->calc();
	printf
	  "$fmt{'V'}  $fmt{'F'}  $fmt{'Pa'} $fmt{'Pr'} $fmt{'Pg'} $fmt{'Pt'} ".
	    "$fmt{'P'} $fmt{'hp'} $fmt{'heat'}  $fmt{'B'}  $fmt{'C'}   ".
	      "$fmt{'kJh'}\n",
	      map { $self->{'_out'}{$_} } @out;
	$self->_incr_output;
    }
}

sub tk_output {
    my($self) = @_;
    $self->_init_output;

    my $entry;
    for ($entry = 0; $entry < $self->N_entry; $entry++) {
	$self->calc();
	my $out;
	foreach $out (@out) {
	    $self->{'_lab'}{$out}->[$entry]->configure
	      (-text => sprintf($fmt{$out},
				$self->{'_out'}{$out}));
	}
	$self->_incr_output;
    }
}


sub tk_interface {
    my($self, $parent) = @_;

    require Tk::Balloon;
    require FindBin;
    push(@INC, $FindBin::Bin);

    my $entry = 'Entry';
    eval { require Tk::NumEntry;
	   Tk::NumEntry->VERSION(1.02);
	   require Tk::NumEntryPlain;
	   Tk::NumEntryPlain->VERSION(0.05);
       };
    if (!$@) { $entry = 'NumEntry' }

    my $automatic = 0;

    my $top = $parent->Toplevel(-title => 'Bikepower');
    $self->{'_top'} = $top;
    push(@tk_interfaces, $top);

    $top->optionAdd("*font" => '-*-helvetica-medium-r-*-14-*',
		    'startupFile');

    my $menuframe = $top->Frame(-relief => 'raised',
				-borderwidth => 2,
			       );

    my $mb_file = $menuframe->Menubutton(-text => 'File',
					 -underline => 0);
    $mb_file->pack(-side => 'left');
    $mb_file->command(-label => 'New',
		      -underline => 0,
 		      -command => sub { my $bp = new BikePower;
					$bp->tk_interface($parent) });
    $mb_file->command(-label => 'Clone',
		      -underline => 1,
 		      -command => sub { my $bp = clone BikePower $self;
					$bp->tk_interface($parent) });
    $mb_file->command(-label => 'Close',
		      -underline => 0,
 		      -command => sub { $top->destroy });

    my $mb_set = $menuframe->Menubutton(-text => 'Settings',
					-underline => 0);
    $mb_set->pack(-side => 'left');
    $mb_set->command(-label => 'Load defaults',
		     -underline => 5,
		     -command => sub { $self->load_defaults });
    $mb_set->command(-label => 'Load...',
		     -underline => 0,
		     -command => sub {
			 my $file;
			 eval { 
			     $file = $top->getOpenFile
			       (-defaultextension => '*.pl');
			 };
			 if ($@) {
			     require Tk::FileDialog;
			     $self->{'_load_fd'} =
			       $top->FileDialog(-Create => 0,
						-ShowAll => 1,
						-FPat => "*.pl");
			     $file = $self->{'_load_fd'}->Show;
			 }
			 if (defined $file) {
			     $self->load_defaults($file);
			 }
		     });
    $mb_set->command(-label => 'Save as default',
		     -underline => 5,
		     -command => sub { $self->save_defaults });
    $mb_set->command(-label => 'Save as...',
		     -underline => 0,
		     -command => sub {
			 my $file;
			 eval { 
			     $file = $top->getSaveFile
			       (-defaultextension => '*.pl');
			 };
			 if ($@) {
			     require Tk::FileDialog;
			     $self->{'_save_fd'} = 
			       $top->FileDialog(-Create => 1,
						-ShowAll => 1,
						-FPat => "*.pl");
			     $file = $self->{'_save_fd'}->Show;
			     if ($file) {
				 if ($file !~ /\.pl$/) {
				     $file .= ".pl";
				 }
				 if (-e $file) {
				     require Tk::Dialog;
				     my $d = $top->Dialog
				       (-title => 'Warning',
					-text  => 'Overwrite existing file <'
					. $file . '>?',
					-default_button => 'No',
					-buttons => ['Yes', 'No'],
					-popover => 'cursor');
				     return if $d->Show ne 'Yes';
				 }
			     }
			 }
			 if (defined $file) {
			     $self->save_defaults($file);
			 }
		     });

    my $mb_help = $menuframe->Menubutton(-text => 'Help',
					 -underline => 0);
    $mb_help->pack(-side => 'right');
    $mb_help->command(-label => 'About...',
		      -underline => 0,
		      -command => sub { 
			  require Tk::Dialog;
			  $top->Dialog(-text =>
				       "BikePower.pm $VERSION\n" .
				       "(c) 1997,1998 Slaven Rezic")->Show;
		      },
		     );
    $mb_help->command(-label => 'Reference...',    
		      -underline => 0,
		      -command => sub { 
			  require Tk::Pod;
			  Tk::Pod->Dir($FindBin::Bin);
			  $top->Pod(-file => 'BikePower.pm');
		      });

    $menuframe->pack(-fill => 'x');

    my $f = $top->Frame->pack;
    my $b = $f->Balloon;

    my $icon;
    foreach $icon ('crouch', 'pack_end', 'pack_middle', 'racing', 'standing',
		   'tuck', 'upright', 'up_down', 'change_wind') {
	if (!defined $icons{$icon}) {
	    eval { 
		$icons{$icon} =
		  $f->Pixmap(-file => Tk::findINC("BikePower/$icon.xpm"));
	    };
	}
    }

    my $row = 0;

    my $calc_button;
    my $autocalc = sub {
	$calc_button->invoke if $automatic;
    };

    my $labentry = sub {
	my($top, $row, $text, $varref, $unit, %a) = @_;
	my $entry = ($a{-forceentry} ? 'Entry' : $entry);
	$top->Label(-text => $text)->grid(-row => $row,
					  -column => 0,
					  -sticky => 'w');
	my $w;
	if (exists $a{-choices}) {
	    require Tk::BrowseEntry;
	    $w = $top->BrowseEntry(-variable => $varref,
				   ($Tk::VERSION >= 800
				    ? (-browsecmd => $autocalc)
				    : ()
				   ),
				  )->grid(-row => $row,
					  -column => 1,
					  -sticky => 'w');
	    $w->insert("end", @{$a{-choices}});
	} else {
	    $w = $top->$entry(-textvariable => $varref,
			      ($entry eq 'NumEntry' && exists $a{-resolution}
			       ? (-resolution => $a{-resolution})
			       : ()
			      ),
			     )->grid(-row => $row,
				     -column => 1,
				     -sticky => 'w');
	}
	$w->bind('<FocusOut>' => $autocalc);
	if (defined $unit) {
	    $top->Label(-text => $unit)->grid(-row => $row,
					      -column => 2,
					      -sticky => 'w');
	}
    };

    &$labentry($f, $row, 'Temperature:', \$self->{'T_a'}, '°C'); $row++;

    &$labentry($f, $row, 'Velocity of headwind:', \$self->{'H'}, 'm/s');
    if (defined $icons{'change_wind'}) {
 	my $btn = $f->Button(-image => $icons{'change_wind'},
			     -command => sub { $self->{'H'} = -$self->{'H'};
					       &$autocalc;
					   },
			    )->grid(-row => $row,
				    -column => 3,
				    -sticky => 'w',
				    -padx => 3);
	$b->attach($btn, -msg => 'toggle headwind and backwind');
    }
    $row++;
    $f->Checkbutton(-text => 'Crosswind',
		    -variable => \$self->{'cross_wind'},
		    -command => $autocalc,
		   )->grid(-row => $row,
			   -column => 0,
			   -sticky => 'w',
			   -ipady => 0,
			  ); $row++;

    &$labentry($f, $row, 'Grade of hill:', \$self->{'G'}, 'm/m',
	       -resolution => 0.01);
    if (defined $icons{'up_down'}) {
 	my $btn =$f->Button(-image => $icons{'up_down'},
			    -command => sub { $self->{'G'} = -$self->{'G'};
					      &$autocalc;
					  },
			   )->grid(-row => $row,
				   -column => 3,
				   -sticky => 'w',
				   -padx => 3);
	$b->attach($btn, -msg => 'toggle up and down hill');
    }
    $row++;

    my @std_a_c =
      ('0.6566873 (standing)',
       '0.4925155 (upright)',
       '0.4297982 (crouch)',
       '0.3080527 (racing crouch)',
       '0.2674709 (full downhill tuck)',
       '0.2213353 (end of pack of 1 or more riders)',
       '0.1844627 (in the middle of a pack)');
    &$labentry($f, $row, '', \$self->{'A_c'}, 'm^2',
	       -choices => \@std_a_c);
    my $ac_frame = $f->Frame(-relief => 'raised',
			     -borderwidth => 2)->grid(-row => $row,
						      -column => 0,
						      -sticky => 'w'); $row++;
    my $ac_menu = $ac_frame->Menubutton(-text => 'Frontal area:',
					-padx => 0,
					-pady => 0)->pack;
    $b->attach($ac_menu, -msg => 'set air resistance');
    my $i = 0;
    foreach ('standing', 'upright', 'crouch', 'racing',
	     'tuck', 'pack_end', 'pack_middle') {
	{
	    my $i = $i; # wegen des Closures...
	    $ac_menu->command((defined $icons{$_}
			       ? (-image => $icons{$_})
			       : (-label => $_)),
			      -command => sub { $self->{'A_c'} = $std_a_c[$i];
						&$autocalc;
					    });
	}
	$i++;
    }

    &$labentry($f, $row, 'Transmission efficiency:', \$self->{'T'}, undef,
	       -resolution => 0.01); $row++;
    &$labentry($f, $row, 'Rolling friction:', \$self->{'R'}, undef,
	       -choices =>
	       ['0.004  (narrow tubular tires, lowest)',
		'0.0047 (26 x 1.125 inch tires)',
		'0.0051 (27 x 1.25 inch tires)',
		'0.0055 (narrow tubular tires, highest)',
		'0.0066 (26 x 1.375)',
		'0.0120 (mountain bike tires)']
	      ); $row++;
    &$labentry($f, $row, 'Weight of cyclist:', \$self->{'Wc'}, 'kg');
    $row++;
    &$labentry($f, $row, 'Weight of bike+clothes:', \$self->{'Wm'}, 'kg');
    $row++;
    
    my $res_frame = $top->Frame(-bg => 'yellow')->pack(-fill => 'x',
						       -ipady => 5);
    $res_frame->optionAdd('*' . substr($res_frame->PathName, 1) . "*background"
			  => 'yellow', 'userDefault');
    $row = 0;
    $res_frame->Label(-text => 'Resolve for:')->grid(-row => $row,
						     -column => 0,
						     -sticky => 'w');
    my $first_label = $res_frame->Label(-text => 'first')->grid(-row => $row,
								-column => 1);
    $b->attach($first_label, -msg => 'first value in table');
    $res_frame->Label(-text => 'increment')->grid(-row => $row,
						  -column => 2);

    my $w;

    $row++;
    $res_frame->Radiobutton(-text => 'velocity',
			    -variable => \$self->{'given'},
			    -value => 'v',
			    -command => $autocalc,
			   )->grid(-row => $row,
				   -column => 0,
				   -sticky => 'w');
    $w = $res_frame->$entry(-textvariable => \$self->{'first_V'},
			    -width => 8,
			   )->grid(-row => $row,
				   -column => 1,
				   -sticky => 'w');
    $w->bind('<FocusOut>' => $autocalc);
    $w = $res_frame->$entry(-textvariable => \$self->{'V_incr'},
			    -width => 8,
			   )->grid(-row => $row,
				   -column => 2,
				   -sticky => 'w');
    $w->bind('<FocusOut>' => $autocalc);
    $row++;
    $res_frame->Radiobutton(-text => 'power',
			    -variable => \$self->{'given'},
			    -value => 'P',
			    -command => $autocalc,
			   )->grid(-row => $row,
				   -column => 0,
				   -sticky => 'w');
    $w = $res_frame->$entry(-textvariable => \$self->{'first_P'},
			    -width => 8,
			   )->grid(-row => $row,
				   -column => 1,
				   -sticky => 'w');
    $w->bind('<FocusOut>' => $autocalc);
    $w = $res_frame->$entry(-textvariable => \$self->{'P_incr'},
			    -width => 8,
			   )->grid(-row => $row,
				   -column => 2,
				   -sticky => 'w');
    $w->bind('<FocusOut>' => $autocalc);
    $row++;
    $res_frame->Radiobutton(-text => 'consumption',
			    -variable => \$self->{'given'},
			    -value => 'C',
			    -command => $autocalc,
			   )->grid(-row => $row,
				   -column => 0,
				   -sticky => 'w');
    $w = $res_frame->$entry(-textvariable => \$self->{'first_C'},
			    -width => 8,
			   )->grid(-row => $row,
				   -column => 1,
				   -sticky => 'w');
    $w->bind('<FocusOut>' => $autocalc);
    $w = $res_frame->$entry(-textvariable => \$self->{'C_incr'},
			    -width => 8,
			   )->grid(-row => $row,
				   -column => 2,
				   -sticky => 'w');
    $w->bind('<FocusOut>' => $autocalc);
    $row++;
    $calc_button = $res_frame->Button
      (-text => 'Calc!',
       -fg => 'white',
       -bg => 'red',
       -command => sub { $self->tk_output },
      )->grid(-row => 1,
	      -rowspan => 2,
	      -column => 5,
	      -padx => 5);
    $top->bind($top, "<Return>" => $autocalc);
    $b->attach($calc_button, -msg => 'start calculation');

    my $auto_calc_check = $res_frame->Checkbutton
      (-text => 'automatic',
       -variable => \$automatic,
       -command => sub {
	   $calc_button->invoke if $automatic;
       },
      )->grid(-row => 3,
	      -column => 5,
	      -padx => 5);
    $b->attach($auto_calc_check,
	       -msg => 'immediate calculation when values change');
    my $output_frame = $top->Frame(-bg => '#ffdead')->pack(-fill => 'x');
    my $output_frame_name = '*' . substr($output_frame->PathName, 1);
    $output_frame->optionAdd($output_frame_name . "*background" 
			     => '#ffdead', 'userDefault');
    $output_frame->optionAdd($output_frame_name . "*relief" 
			     => 'ridge', 'userDefault');
    $output_frame->optionAdd($output_frame_name . "*borderWidth" 
			     => 1, 'userDefault');
    my $col = 0;
    my $v_label = $output_frame->Label(-text => 'v',
				       -width => 5,
				      )->grid(-row => 0,
					      -column => $col); $col++;
    $b->attach($v_label, -msg => 'velocity [km/h]');
    my $F_label = $output_frame->Label(-text => 'F',
				       -width => 4,
				      )->grid(-row => 0,
					      -column => $col); $col++;
    $b->attach($F_label, -msg => 'total force resisting forward motion [kg]');
    my $Pa_label = $output_frame->Label(-text => 'Pa',
					-width => 4,
				       )->grid(-row => 0,
					       -column => $col); $col++;
    $b->attach($Pa_label,
	       -msg => 'power output to overcome air resistance [W]');
    my $Pr_label = $output_frame->Label(-text => 'Pr',
					-width => 4,
				       )->grid(-row => 0,
					       -column => $col); $col++;
    $b->attach($Pr_label,
	       -msg => 'power output to overcome rolling friction [W]');
    my $Pg_label = $output_frame->Label(-text => 'Pg',
					-width => 5,
				       )->grid(-row => 0,
					       -column => $col); $col++;
    $b->attach($Pg_label, -msg => 'power output to climb grade [W]');
    my $Pt_label = $output_frame->Label(-text => 'Pt',
					-width => 4,
				       )->grid(-row => 0,
					       -column => $col); $col++;
    $b->attach($Pt_label,
	       -msg => 'power loss due to drivetrain inefficiency [W]');
    my $P_label = $output_frame->Label(-text => 'P',
				       -width => 5,
				      )->grid(-row => 0,
					      -column => $col); $col++;
    $b->attach($P_label, -msg => 'total power output [W]');
    my $hp_label = $output_frame->Label(-text => 'hp',
					-width => 5,
				       )->grid(-row => 0,
					       -column => $col); $col++;
    $b->attach($hp_label, -msg => 'total power output [hp]');
    my $heat_label = $output_frame->Label(-text => 'heat',
					  -width => 5,
					 )->grid(-row => 0,
						 -column => $col); $col++;
    $b->attach($heat_label,
	       -msg => 'power wasted due to human inefficiency [W]');
    my $BM_label = $output_frame->Label(-text => 'BM',
					-width => 3,
				       )->grid(-row => 0,
					       -column => $col); $col++;
    $b->attach($BM_label, -msg => 'basal metabolism [W]');
    my $C_label = $output_frame->Label(-text => 'C',
				       -width => 5,
				      )->grid(-row => 0,
					      -column => $col); $col++;
    $b->attach($C_label, -msg => 'total power consumption [W]');
    my $kJh_label = $output_frame->Label(#-text => 'kJ/h',
					 -text => 'cal/h',
					 -width => 5,
					)->grid(-row => 0,
						-column => $col); $col++;
    $b->attach($kJh_label, -msg => #'total power consumption [kJ/h]'
	       'total power consumption [cal/h]');

    {
	my $entry;
	for($entry = 0; $entry < $self->{'N_entry'}; $entry++) {
	    $col = 0;
	    my $out;
	    foreach $out (@out) {
		$self->{'_lab'}{$out}->[$entry] = $output_frame->Label;
		$self->{'_lab'}{$out}->[$entry]->grid
		  (-row => 1 + $entry,
		   -column => $col,
		   -sticky => 'ew'); $col++;
	    }
	}
    }

    $top;
}

1;

__END__

=head1 NAME

BikePower - bicycle power-output calculator with command-line and Tk interface

=head1 SYNOPSIS

    use Tk;
    use BikePower;
    $top = new MainWindow;
    BikePower::tk_interface($top);

or

    use BikePower;
    BikePower::output();

=head1 DESCRIPTION

XXX

=head1 TODO

    + überprüfen, ob sich TiedListbox mit BrowseEntry kombinieren läßt
      (für Luftwiderstand und Reifengröße)
    - Möglichkeit: mehrere Tk-Interfaces teilen sich Felder (Checkbutton)

=head1 AUTHOR

Slaven Rezic (eserte@cs.tu-berlin.de)

Original program bike_power.c by Ken Roberts (roberts@cs.columbia.edu),
Dept of Computer Science, Columbia University, New York.

Copyright (c) 1997,1998 Slaven Rezic. All rights reserved.
This package is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

=cut

