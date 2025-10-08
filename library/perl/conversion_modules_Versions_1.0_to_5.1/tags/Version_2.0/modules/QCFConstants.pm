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
##Module---------------------------------------------------------------
package QCFConstants;

use strict;
use lib ".";

##------------------------------------------------------------------------
# @signature String getBadFlag()
# <p>Get the flag used for bad values.</p>
#
# @output $flag Returns "B".
##------------------------------------------------------------------------
sub getBadFlag { return "B"; }

##------------------------------------------------------------------------
# @signature String getDubiousFlag()
# <p>Get the flag for questionable/dubious values.</p>
#
# @output $flag Returns "D".
##------------------------------------------------------------------------
sub getDubiousFlag { return "D"; }

##------------------------------------------------------------------------
# @signature String getEstimateFlag()
# <p>Get the flag used for an estimated value.</p>
#
# @output $flag Returns "E".
##------------------------------------------------------------------------
sub getEstimateFlag { return "E"; }

##------------------------------------------------------------------------
# @signature Hash getFlagPrecedence()
# <p>Get the hash that contains the flag precedence order.  The flags
# that have a higher precedence have a higher value.</p>
#
# @output $flag_precedence The flag precedence order for QCF flags.
##------------------------------------------------------------------------
sub getFlagPrecedence {
    my %flag_precedence;
    $flag_precedence{getTracePrecipFlag()} = 1;
    $flag_precedence{getGoodFlag()} = 2;
    $flag_precedence{getUncheckedFlag()} = 3;
    $flag_precedence{getDubiousFlag()} = 4;
    $flag_precedence{getEstimateFlag()} = 5;
    $flag_precedence{getBadFlag()} = 6;
    $flag_precedence{getGlitchFlag()} = 7;
    $flag_precedence{getIncalcuableFlag()} = 8;
    $flag_precedence{getValueDoesNotFitFlag()} = 9;
    $flag_precedence{getNoReadingFlag()} = 10;
    $flag_precedence{getMissingFlag()} = 11;
    return %flag_precedence;
}

##------------------------------------------------------------------------
# @signature String getGoodFlag()
# <p>Get the flag used for a good value.</p>
#
# @output $flag Returns "G".
##------------------------------------------------------------------------
sub getGoodFlag { return "G"; }

##------------------------------------------------------------------------
# @signature String getGlitchFlag()
# <p>Get the flag used when there was a glitch for a value.</p>
#
# @output $flag Returns "X".
##------------------------------------------------------------------------
sub getGlitchFlag { return "X"; }

##------------------------------------------------------------------------
# @signature String getIncalcuableFlag()
# <p>Get the flag used for a value that cannot be calcualted.</p>
#
# @output $flag Returns "I".
##------------------------------------------------------------------------
sub getIncalcuableFlag { return "I"; }

##------------------------------------------------------------------------
# @signature String getMissing()
# <p>Get the value to use for missing values.</p>
#
# @output $value Returns "-999.99".
##------------------------------------------------------------------------
sub getMissing { return "-999.99"; }

##------------------------------------------------------------------------
# @signature String getMissingFlag()
# <p>Get the flag used for reporting missing values.
#
# @output $flag Returns "M".
##------------------------------------------------------------------------
sub getMissingFlag { return "M"; }

##------------------------------------------------------------------------
# @signature String getNegativePrecipFlag()
# <p>Get the flag used for a reported negative precipitation value.</p>
#
# @output $flag Returns "C".
##------------------------------------------------------------------------
sub getNegativePrecipFlag { return "C"; }

##------------------------------------------------------------------------
# @signature String getNoReadingFlag()
# <p>Get the flag used for reporting when a station does not have the
# equipment to measure a certain weather phenomenon.</p>
#
# @output $flag Returns "N".
##------------------------------------------------------------------------
sub getNoReadingFlag { return "N"; }

##------------------------------------------------------------------------
# @signature String getTracePrecipFlag()
# <p>Get the flag used for trace amounts of precipitation.
#
# @output $flag Returns "T".
##------------------------------------------------------------------------
sub getTracePrecipFlag { return "T"; }

##------------------------------------------------------------------------
# @signature String getUncheckedFlag()
# <p>Get the flag for when the value needs to be checked.</p>
#
# @output $flag Returns "U".
##------------------------------------------------------------------------
sub getUncheckedFlag { return "U"; }

##------------------------------------------------------------------------
# @signature String getValueDoesNotFitFlag()
# <p>Get the flag for when the value is too big for its field.</p>
#
# @output $flag Returns "C".
##------------------------------------------------------------------------
sub getValueDoesNotFitFlag { return "C"; }

1;





