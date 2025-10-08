#! /usr/bin/perl -w

##Module---------------------------------------------------------------
# <p>The <code>QCFConstants</code> module is a collection of constant
# functions used for the QCF format.  It contains flags and the missing
# value.</p>
#
# @author Joel Clawson
# @version 2.0 This was removed from the Version1 Conversions.pm module
# since I did not think that it belonged with the static functions since 
# the Conversions.pm module could be used by any number of different 
# conversions which may have different constants.
#
# @author Joel Clawson
# @version 4.0 The precipitation flags were added along with the 
# functions that take in a set of flags and return the one with the
# highest precedence.
##Module---------------------------------------------------------------
package QCFConstants;
use strict;
use lib "/net/work/lib/perl/Utilities";
require Exporter;
our @ISA = ("Exporter");
our @EXPORT = qw(getFlagPrecedence getHighestQCFPrecedenceFlag $BAD_FLAG $DUBIOUS_FLAG $ESTIMATE_FLAG $GOOD_FLAG $GLITCH_FLAG $INCALCUABLE_FLAG $MISSING_FLAG $NEG_PRECIP_FLAG $NO_READING_FLAG $TRACE_PRECIP_FLAG $UNCHECKED_FLAG $VALUE_DOES_NOT_FIT_FLAG $MISSING $PRECIP_ACCUM_CODE $PRECIP_ACCUM_END_CODE $PRECIP_DELETE_CODE $PRECIP_MISSING_CODE $PRECIP_NO_QUALIFIER_CODE $PRECIP_PROB_AMT_CODE $PRECIP_SUSPECT_AMT_CODE $PRECIP_TRACE_CODE);

our $BAD_FLAG = "B";
our $DUBIOUS_FLAG = "D";
our $ESTIMATE_FLAG = "E";
our $GOOD_FLAG = "G";
our $GLITCH_FLAG = "X";
our $INCALCUABLE_FLAG = "I";
our $MISSING_FLAG = "M";
our $NEG_PRECIP_FLAG = "C";
our $NO_READING_FLAG = "N";
our $TRACE_PRECIP_FLAG = "T";
our $UNCHECKED_FLAG = "U";
our $VALUE_DOES_NOT_FIT_FLAG = "C";

our $MISSING = -999.99;

our $PRECIP_ACCUM_CODE = 1;
our $PRECIP_ACCUM_END_CODE = 2;
our $PRECIP_DELETE_CODE = 3;
our $PRECIP_MISSING_CODE = 7;
our $PRECIP_NO_QUALIFIER_CODE = 0;
our $PRECIP_PROB_AMT_CODE = 5;
our $PRECIP_SUSPECT_AMT_CODE = 6;
our $PRECIP_TRACE_CODE = 4;

##------------------------------------------------------------------------
# @signature Hash getFlagPrecedence()
# <p>Get the hash that contains the flag precedence order.  The flags
# that have a higher precedence have a higher value.</p>
#
# @output $flag_precedence The flag precedence order for QCF flags.
##------------------------------------------------------------------------
sub getFlagPrecedence {
    my %flag_precedence;
    $flag_precedence{$TRACE_PRECIP_FLAG} = 1;
    $flag_precedence{$GOOD_FLAG} = 2;
    $flag_precedence{$UNCHECKED_FLAG} = 3;
    $flag_precedence{$DUBIOUS_FLAG} = 4;
    $flag_precedence{$ESTIMATE_FLAG} = 5;
    $flag_precedence{$BAD_FLAG} = 6;
    $flag_precedence{$GLITCH_FLAG} = 7;
    $flag_precedence{$INCALCUABLE_FLAG} = 8;
    $flag_precedence{$VALUE_DOES_NOT_FIT_FLAG} = 9;
    $flag_precedence{$NO_READING_FLAG} = 10;
    $flag_precedence{$MISSING_FLAG} = 11;
    return %flag_precedence;
}

sub getHighestPrecipPrecedenceFlag {
    my ($max,@flags) = @_;
    my $highest = $PRECIP_NO_QUALIFIER_CODE;
    my %precedence = ($PRECIP_NO_QUALIFIER_CODE => 1,
		      $PRECIP_MISSING_CODE => 2,
		      $PRECIP_DELETE_CODE => 3,
		      $PRECIP_SUSPECT_AMT_CODE => 4,
		      $PRECIP_PROB_AMT_CODE => 5,
		      $PRECIP_TRACE_CODE => 6,
		      $PRECIP_ACCUM_CODE => 7,
		      $PRECIP_ACCUM_END_CODE => 8);

    foreach my $flag (@flags) {
	$highest = $precedence{$highest} < $precedence{$flag} ? $flag : $highest;
    }

    return $precedence{$highest} > $precedence{$max} ? $max : $highest;
}

sub getHighestQCFPrecedenceFlag {
    my ($max,@flags) = @_;
    my $highest = $TRACE_PRECIP_FLAG;
    my %precedence = getFlagPrecedence();

    foreach my $flag (@flags) {
	$highest = $precedence{$highest} < $precedence{$flag} ? $flag : $highest;
    }

    return $precedence{$highest} > $precedence{$max} ? $max : $highest;
}

1;





