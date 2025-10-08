#! /usr/bin/perl -w

##Module---------------------------------------------------------------
# <p>The <code>CEOPConstants</code> module is a collection of constant
# functions used for the CEOP format.  It contains flags and the missing
# value.</p>
#
# @author Joel Clawson
# @version 2.1 This is added to handle CEOP data.
##Module---------------------------------------------------------------
package CEOPConstants;

use strict;
use lib ".";

##------------------------------------------------------------------------
# @signature String getBadFlag()
# <p>Get the flag used for bad values.</p>
#
# @output $flag Returns 'B'.
##------------------------------------------------------------------------
sub getBadFlag { return 'B'; }

##------------------------------------------------------------------------
# @signature String getDubiousFlag()
# <p>Get the flag for questionable/dubious values.</p>
#
# @output $flag Returns 'D'.
##------------------------------------------------------------------------
sub getDubiousFlag { return 'D'; }

##------------------------------------------------------------------------
# @signature String getEstimateFlag()
# <p>Get the flag used for an estimated value.</p>
#
# @output $flag Returns 'I'.
##------------------------------------------------------------------------
sub getEstimateFlag { return 'I'; }

##------------------------------------------------------------------------
# @signature Hash getFlagPrecedence()
# <p>Get the hash that contains the flag precedence order.  The flags
# that have a higher precedence have a higher value.</p>
#
# @output $flag_precedence The flag precedence order for QCF flags.
##------------------------------------------------------------------------
sub getFlagPrecedence {
    my %flag_precedence;
    $flag_precedence{getUncheckedFlag()} = 1;
    $flag_precedence{getGoodFlag()} = 2;
    $flag_precedence{getDubiousFlag()} = 3;
    $flag_precedence{getEstimateFlag()} = 4;
    $flag_precedence{getBadFlag()} = 5;
    $flag_precedence{getMissingFlag()} = 6;
    $flag_precedence{getValueDoesNotFitFlag()} = 7;
    return %flag_precedence;
}

##------------------------------------------------------------------------
# @signature String getGoodFlag()
# <p>Get the flag used for a good value.</p>
#
# @output $flag Returns 'G'.
##------------------------------------------------------------------------
sub getGoodFlag { return 'G'; }

##------------------------------------------------------------------------
# @signature String getMissing()
# <p>Get the value to use for missing values.</p>
#
# @output $value Returns '-999.99'.
##------------------------------------------------------------------------
sub getMissing { return "-999.99"; }

##------------------------------------------------------------------------
# @signature String getMissingFlag()
# <p>Get the flag used for reporting missing values.
#
# @output $flag Returns 'M'.
##------------------------------------------------------------------------
sub getMissingFlag { return 'M'; }

##------------------------------------------------------------------------
# @signature String getNegativePrecipFlag()
# <p>Get the flag used for a reported negative precipitation value.</p>
#
# @output $flag Returns 'C'.
##------------------------------------------------------------------------
sub getNegativePrecipFlag { return 'C'; }

##------------------------------------------------------------------------
# @signature String getUncheckedFlag()
# <p>Get the flag for when the value needs to be checked.</p>
#
# @output $flag Returns 'U'.
##------------------------------------------------------------------------
sub getUncheckedFlag { return 'U'; }

##------------------------------------------------------------------------
# @signature String getValueDoesNotFitFlag()
# <p>Get the flag for when the value is too big for its field.</p>
#
# @output $flag Returns 'C'.
##------------------------------------------------------------------------
sub getValueDoesNotFitFlag { return 'C'; }

1;





