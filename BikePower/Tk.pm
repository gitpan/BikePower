# -*- perl -*-

#
# $Id: Tk.pm,v 1.1 1998/06/29 18:08:43 eserte Exp $
# Author: Slaven Rezic
#
# Copyright: see BikePower.pm
#
# Mail: eserte@cs.tu-berlin.de
# WWW:  http://user.cs.tu-berlin.de/~eserte/
#

use strict;

package Tie::Lang;

sub TIEHASH {
    my($pkg, $lang_def_ref, $lang) = @_;
    my $self = {};
    bless $self, $pkg;
    $self->{LangDef} = $lang_def_ref;
    $self->set_lang($lang || 'en');
    $self;
}

sub FETCH {
    my($self, $key) = @_;
    if (exists $self->{LangDef}{$self->{Lang}}{$key}) {
	$self->{LangDef}{$self->{Lang}}{$key};
    } else {
	$key;
    }
}

sub STORE  { die }
sub DELETE { die }

sub set_lang {
    my($self, $newlang) = @_;
    $self->{Lang} = $newlang;
}

package BikePower::Tk;
use BikePower;
use vars qw($VERSION @tk_interfaces %icons);
$VERSION = '0.01';

# language strings
my $lang_s =
  {'en' => 
   {
   },
   'de' =>
   {
    'File' => 'Datei',
    'New' => 'Neu',
    'Clone' => 'Klonen',
    'Close' => 'Schließen',
    'Settings' => 'Einstellungen',
    'Load defaults' => 'Voreinstellung laden',
    'Load...' => 'Laden...',
    'Save as default' => 'Als Voreinstellung sichern',
    'Save as...' => 'Sichern als...',
    'Warning' => 'Warnung',
    'Overwrite existing file <%s>?' => 'Bereits vorhandene Datei <%s> überschreiben?',
    'No' => 'Nein',
    'Yes' => 'Ja',
    'Help' => 'Hilfe',
    'About...' => 'Über...',
    'Reference...' => 'Referenz...',
    'Temperature' => 'Temperatur',
    'Velocity of headwind' => 'Gegenwind',
    'toggle headwind and backwind' => 'zwischen Gegen- und Rückenwind umschalten',
    'Crosswind' => 'Seitenwind',
    'Grade of hill' => 'Steigung',
    'toggle up and down hill' => 'zwischen Steigung und Gefälle umschalten',
    'standing' => 'stehend',
    'upright' => 'aufrecht',
    'crouch' => 'geduckt',
    'racing crouch' => 'geduckt (Rennstellung (?))',
    'full downhill tuck' => 'Abfahrtshaltung (?)',
    'end of pack of 1 or more riders' => 'am Ende eines Verbandes',
    'in the middle of a pack' => 'in der Mitte eines Verbandes',
    'Frontal area' => 'Vorderfläche (?)',
    'set air resistance' => 'Luftwiderstand setzen',
    'Transmission efficiency' => 'Effizienz der Übertragung',
    'Rolling friction' => 'Rollwiderstand',
    'narrow tubular tires, lowest' => 'schmale röhrenförmige Reifen, niedrigster Wert',
    ' inch tires' => '"-Reifen',
    'narrow tubular tires, highest' => 'schmale röhrenförmige Reifen, höchster Wert',
    'mountain bike tires' => 'Mountainbike-Reifen',
    'Weight of cyclist' => 'Fahrergewicht',
    'Weight of bike+clothes' => 'Gewicht von Rad+Kleidung',
    'Resolve for' => 'Lösen für',
    'first' => 'Erster Wert',
    'first value in table' => 'erster Wert in der Tabelle',
    'increment' => 'Erhöhung',
    'velocity' => 'Geschwindigkeit',
    'power' => 'Leistung',
    'consumption' => 'Verbrauch',
    'Calc' => 'Berechnen',
    'start calculation' => 'Berechnung starten',
    'automatic' => 'automatisch',
    'immediate calculation when values change' => 'sofortige Berechnung bei Wertänderung',
   },
  };

sub tk_output {
    my($self) = @_;
    $self->_init_output;

    my $entry;
    for ($entry = 0; $entry < $self->N_entry; $entry++) {
	$self->calc();
	my $out;
	foreach $out (@BikePower::out) {
	    $self->{'_lab'}{$out}->[$entry]->configure
	      (-text => sprintf($BikePower::fmt{$out},
				$self->{'_out'}{$out}));
	}
	$self->_incr_output;
    }
}


sub tk_interface {
    my($self, $parent, %args) = @_;

    my %s;
    tie %s, 'Tie::Lang', $lang_s, $args{'-lang'} || 'en';

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

    my $mb_file = $menuframe->Menubutton(-text => $s{'File'},
					 -underline => 0);
    $mb_file->pack(-side => 'left');
    $mb_file->command(-label => $s{'New'},
		      -underline => 0,
 		      -command => sub { my $bp = new BikePower;
					$bp->tk_interface($parent) });
    $mb_file->command(-label => $s{'Clone'},
		      -underline => 1,
 		      -command => sub { my $bp = clone BikePower $self;
					$bp->tk_interface($parent, %args) });
    $mb_file->command(-label => $s{'Close'},
		      -underline => 0,
 		      -command => sub { $top->destroy });

    my $mb_set = $menuframe->Menubutton(-text => $s{'Settings'},
					-underline => 0);
    $mb_set->pack(-side => 'left');
    $mb_set->command
      (-label => $s{'Load defaults'},
       -underline => 5,
       -command => sub { $self->load_defaults });
    $mb_set->command
      (-label => $s{'Load...'},
       -underline => 0,
       -command => sub {
	   my $file;
	   eval { 
	       $file = $top->getOpenFile
		 (-defaultextension => '*.pl');
	   };
	   if ($@) {
	       require Tk::FileSelect;
	       $self->{'_load_fd'} =
		 $top->FileSelect(-create => 0,
				  -filter => "*.pl");
	       $file = $self->{'_load_fd'}->Show;
	   }
	   if (defined $file) {
	       $self->load_defaults($file);
	   }
       });
    $mb_set->command
      (-label => $s{'Save as default'},
       -underline => 5,
       -command => sub { $self->save_defaults });
    $mb_set->command
      (-label => $s{'Save as...'},
       -underline => 0,
       -command => sub {
	   my $file;
	   eval { 
	       $file = $top->getSaveFile
		 (-defaultextension => '*.pl');
	   };
	   if ($@) {
	       require Tk::FileSelect;
	       $self->{'_save_fd'} = 
		 $top->FileSelect(-create => 1,
				  -filter => "*.pl");
	       $file = $self->{'_save_fd'}->Show;
	       if ($file) {
		   if ($file !~ /\.pl$/) {
		       $file .= ".pl";
		   }
		   if (-e $file) {
		       require Tk::Dialog;
		       my $d = $top->Dialog
			 (-title => $s{'Warning'},
			  -text  => sprintf($s{'Overwrite existing file <%s>?'}, $file),
			  -default_button => $s{'No'},
			  -buttons => [$s{'Yes'}, $s{'No'}],
			  -popover => 'cursor');
		       return if $d->Show ne $s{'Yes'};
		   }
	       }
	   }
	   if (defined $file) {
	       $self->save_defaults($file);
	   }
       });

    my $mb_help = $menuframe->Menubutton(-text => $s{'Help'},
					 -underline => 0);
    $mb_help->pack(-side => 'right');
    $mb_help->command
      (-label => $s{'About...'},
       -underline => 0,
       -command => sub { 
	   require Tk::Dialog;
	   $top->Dialog(-text =>
			"BikePower.pm $BikePower::VERSION\n" .
			"(c) 1997,1998 Slaven Rezic")->Show;
       },
      );
    $mb_help->command
      (-label => $s{'Reference...'},
       -underline => 0,
       -command => sub { 
	   eval {
	       require Tk::Pod;
	       Tk::Pod->Dir($FindBin::Bin);
	       $top->Pod(-file => 'BikePower.pm');
	   };
	   if ($@) {
	       require Tk::Dialog;
	       $top->Dialog(-text => "Error: $@")->Show;
	   }
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

    &$labentry($f, $row, $s{'Temperature'} . ':', \$self->{'T_a'}, '°C');
    $row++;

    &$labentry($f, $row, $s{'Velocity of headwind'} . ':',
	       \$self->{'H'}, 'm/s');
    if (defined $icons{'change_wind'}) {
 	my $btn = $f->Button(-image => $icons{'change_wind'},
			     -command => sub { $self->{'H'} = -$self->{'H'};
					       &$autocalc;
					   },
			    )->grid(-row => $row,
				    -column => 3,
				    -sticky => 'w',
				    -padx => 3);
	$b->attach($btn, -msg => $s{'toggle headwind and backwind'});
    }
    $row++;
    $f->Checkbutton(-text => $s{'Crosswind'},
		    -variable => \$self->{'cross_wind'},
		    -command => $autocalc,
		   )->grid(-row => $row,
			   -column => 0,
			   -sticky => 'w',
			   -ipady => 0,
			  ); $row++;

    &$labentry($f, $row, $s{'Grade of hill'} . ':', \$self->{'G'}, 'm/m',
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
	$b->attach($btn, -msg => $s{'toggle up and down hill'});
    }
    $row++;

    my @std_a_c =
      ('0.6566873 (' . $s{'standing'} . ')',
       '0.4925155 (' . $s{'upright'} . ')',
       '0.4297982 (' . $s{'crouch'} . ')',
       '0.3080527 (' . $s{'racing crouch'} . ')',
       '0.2674709 (' . $s{'full downhill tuck'} . ')',
       '0.2213353 (' . $s{'end of pack of 1 or more riders'} . ')',
       '0.1844627 (' . $s{'in the middle of a pack'} . ')');
    &$labentry($f, $row, '', \$self->{'A_c'}, 'm^2',
	       -choices => \@std_a_c);
    my $ac_frame = $f->Frame(-relief => 'raised',
			     -borderwidth => 2)->grid(-row => $row,
						      -column => 0,
						      -sticky => 'w'); $row++;
    my $ac_menu = $ac_frame->Menubutton(-text => $s{'Frontal area'} . ':',
					-padx => 0,
					-pady => 0)->pack;
    $b->attach($ac_menu, -msg => $s{'set air resistance'});
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

    &$labentry($f, $row, $s{'Transmission efficiency'} . ':',
	       \$self->{'T'}, undef,
	       -resolution => 0.01); $row++;
    &$labentry($f, $row, $s{'Rolling friction'} . ':', \$self->{'R'}, undef,
	       -choices =>
	       ['0.004  (' . $s{'narrow tubular tires, lowest'} . ')',
		'0.0047 (26 x 1.125' . $s{' inch tires'} . ')',
		'0.0051 (27 x 1.25' . $s{' inch tires'} . ')',
		'0.0055 (' . $s{'narrow tubular tires, highest'} . ')',
		'0.0066 (26 x 1.375' . $s{' inch tires'} . ')',
		'0.0120 (' . $s{'mountain bike tires'} . ')']
	      ); $row++;
    &$labentry($f, $row, $s{'Weight of cyclist'} . ':',
	       \$self->{'Wc'}, 'kg');
    $row++;
    &$labentry($f, $row, $s{'Weight of bike+clothes'} . ':',
	       \$self->{'Wm'}, 'kg');
    $row++;
    
    my $res_frame = $top->Frame(-bg => 'yellow')->pack(-fill => 'x',
						       -ipady => 5);
    $res_frame->optionAdd('*' . substr($res_frame->PathName, 1) . "*background"
			  => 'yellow', 'userDefault');
    $row = 0;
    $res_frame->Label(-text => $s{'Resolve for'} . ':'
		     )->grid(-row => $row,
			     -column => 0,
			     -sticky => 'w');
    my $first_label = $res_frame->Label(-text => $s{'first'}
				       )->grid(-row => $row,
					       -column => 1);
    $b->attach($first_label, -msg => $s{'first value in table'});
    $res_frame->Label(-text => $s{'increment'})->grid(-row => $row,
							-column => 2);

    my $w;

    $row++;
    $res_frame->Radiobutton(-text => $s{'velocity'},
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
    $res_frame->Radiobutton(-text => $s{'power'},
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
    $res_frame->Radiobutton(-text => $s{'consumption'},
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
      (-text => $s{'Calc'} . '!',
       -fg => 'white',
       -bg => 'red',
       -command => sub { tk_output($self) },
      )->grid(-row => 1,
	      -rowspan => 2,
	      -column => 5,
	      -padx => 5);
    $top->bind($top, "<Return>" => $autocalc);
    $b->attach($calc_button, -msg => $s{'start calculation'});

    my $auto_calc_check = $res_frame->Checkbutton
      (-text => $s{'automatic'},
       -variable => \$automatic,
       -command => sub {
	   $calc_button->invoke if $automatic;
       },
      )->grid(-row => 3,
	      -column => 5,
	      -padx => 5);
    $b->attach($auto_calc_check,
	       -msg => $s{'immediate calculation when values change'});
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
    $b->attach($v_label, -msg => $s{'velocity'} . ' [km/h]');
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
	    foreach $out (@BikePower::out) {
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

