#!/bin/perl

print "Conflicting land uses:\ncol row uses\n";

$_=<>;
while (<>) {
 s/^\s*(.*?)\s*$/$1/;
 last if ($_ !~ /./);
 ($x,$y,$lat,$lon,$r,$s,$elat,$elon,$elev,$land) = split /\s+/, $_, 10;
 $uses[$r][$s].="$land,";
 }

$badct=0;
$_=<>;
$_=<>;
while (<>) {
 s/^\s*(.*?)\s*$/$1/;
 last if ($_ !~ /./);
 ($r,$s,$rest) = split /\s+/, $_, 3;
 @tmp = split /\,/, $uses[$r][$s];
 for ($i=1; $i<=$#tmp; $i++) {
   if ($tmp[$i] ne $tmp[0]) {
     print "$r $s $uses[$r][$s]\n";
     $badct++;
     last;
     }
   }
 }

print "\n$badct EASE-Grid cells with duplicate domain points have conflicting land uses\n";
