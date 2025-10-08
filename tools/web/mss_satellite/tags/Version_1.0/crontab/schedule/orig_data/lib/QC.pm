#!/bin/perl

package QC;

require Exporter;
use vars qw(@ISA @EXPORT $VERSION);

@ISA = qw(Exporter);
@EXPORT = qw( $ZERO_CP $PONE_CP $MONE_CP $ZERO_UK $PONE_UK $MONE_UK $UNKNOWN $OTHER );

$ZERO_CP = 0;
$PONE_CP = 1;
$MONE_CP = 2;
$ZERO_UK = 3;
$PONE_UK = 4;
$MONE_UK = 5;
$UNKNOWN = 6;
$OTHER = 7;

1;
