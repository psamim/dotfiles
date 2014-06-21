#!/bin/php
<?php
error_reporting(0);
$command = "/home/samim/.cabal/bin/arbtt-stats -c day --for-each=day --output-format=csv";
exec($command, $days, $return);
$days = explode(",", $days[count($days)-1]);
echo 'Today: ' . $days[2] . "\n";
echo $days[2] . "\n";
if (strtotime($days[2]) > strtotime("04:00:00")) {
	echo '#FF0000';
}
