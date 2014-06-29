#!/usr/bin/perl
## Made by Pierre Mavro/Deimosfr <deimos@deimos.fr>

use strict;
use warnings;
use utf8;
use Getopt::Long;

# Get CPU usage
sub get_network_usage {
# Convert if number is too big
sub octal_convert {
my $value = shift;
if ($value < 1000) {
	$value=sprintf "%.1fK", $value;
} elsif ($value < 1000000) {
$value=sprintf "%.1fM", $value / 1024;
} elsif ($value < 1000000000) {
$value=sprintf "%.1fG", $value / 1024 / 1024;
}
return $value;
}

my $rx=0;
my $tx=0;
# Parse sar command output
open (SAR, 'sar -n DEV 1 1 |') or die "Can't open sar $?";
while (<SAR>) {
	chomp $_;
	# Need this regex to catch Average lines in multiple languages
	next if ($_ !~ /^\w+\s*:\s+/);
	if ($_ =~ /^.*?\d+\.\d+\s*\d+\.\d+\s*(\d+\.\d+)\s*(\d+\.\d+)/) {
		$rx+=$1;
		$tx+=$2;
	}
}
close(SAR);

# Transform to avoid only KB output
$rx=octal_convert($rx);
$tx=octal_convert($tx);

return($rx,$tx);
}

# Print output
sub print_output {
my $rx = shift;
my $tx = shift;

print "NET: \xe2\x87\xa3$rx \xe2\x87\xa1$tx\n"x2;
}

my ($rx,$tx)=get_network_usage();
print_output($rx,$tx);
