# -*- perl -*-

BEGIN { $| = 1; print "1..5\n"; }
END {print "not ok 1\n" unless $loaded;}
use BikePower;
use BikePower::Tk;
$loaded = 1;
print "ok 1\n";

$o = new BikePower
  '-no-ini' => 1,
  '-no-default' => 1,
  'V_incr' => 2,
  'C_a' => '0.9',
  'A_c' => '0.4925155 (upright)',
  'Wm' => 19,
  'E' => '0.249',
  'G' => '0',
  'H' => '0',
  'first_C' => 500,
  'C_incr' => 100,
  'A1' => '0',
  'R' => '0.0066 (26 x 1.375)',
  'T_a' => 20,
  'T' => '0.95',
  'first_P' => 50,
  'given' => 'v',
  'Wc' => 68,
  'BM_rate' => '1.4',
  'P_incr' => 50,
  'cross_wind' => '0',
  'first_V' => 16,
  'N_entry' => 10
;

if (!$o->isa('BikePower')) {
    print "not ";
}
print "ok 2\n";

$o->velocity(30/3.6); # supply velocity in m/s
if ($o->velocity != 30/3.6) {
    print "not ";
}
print "ok 3\n";

# XXX maybe rounding errors are possible?!? check it on other machines!

$o->calc;
if (int($o->power) != 212) { # Watts
    print "not ";
}
print "ok 4\n";

$o->given('P');
$o->power(200);
$o->calc;
if (sprintf("%.1f", $o->velocity*3.6) ne "29.3") {
    print "not ";
}
print "ok 5\n";

eval {
    use Tk;
    $top = new MainWindow;
    $t = $o->tk_interface($top);
    $top->update;
    $top->destroy;
    print "ok 6\n"; # XXX 
};
