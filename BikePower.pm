#!/usr/local/bin/perl
# -*- perl -*-

#
# $Id: BikePower.pm,v 2.5.1.4 1998/06/29 23:18:05 eserte Exp $
# Author: Slaven Rezic
#
# Copyright: see at bottom of file
#
# Mail: eserte@cs.tu-berlin.de
# WWW:  http://user.cs.tu-berlin.de/~eserte/
#

use strict;

package BikePower;

use vars qw($m_s__per__mi_h $m_s__per__km_h $Nt__per__lb $kg__per__Nt
	    $Watts__per__Cal_hr $Watts__per__horsepower
	    $NOSAVE
	    @out %fmt @air_density %members
	    $VERSION
 	   );

$VERSION = '0.14';

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
  (1.5147, 1.5083, 1.5019, 1.4955, 1.4892, # -30�C 3.6�F
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
   1.1277, 1.1242, 1.1206, 1.1170, 1.1135  #  40�C 138.6�F 
  );

# members
# maybe accessed by hash or by method
%members =
  ('imperial'    => [[], 0, 'metric/imperial flag', undef],
   'T_a'         => [['temperature'], 20, 'temperature [�C]', undef],
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

sub tk_interface {
    require BikePower::Tk;
    BikePower::Tk::tk_interface(@_);
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

    + �berpr�fen, ob sich TiedListbox mit BrowseEntry kombinieren l��t
      (f�r Luftwiderstand und Reifengr��e)
    - M�glichkeit: mehrere Tk-Interfaces teilen sich Felder (Checkbutton)

=head1 AUTHOR

Slaven Rezic (eserte@cs.tu-berlin.de)

Original program bike_power.c by Ken Roberts (roberts@cs.columbia.edu),
Dept of Computer Science, Columbia University, New York.

Copyright (c) 1997,1998 Slaven Rezic. All rights reserved.
This package is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

=cut

