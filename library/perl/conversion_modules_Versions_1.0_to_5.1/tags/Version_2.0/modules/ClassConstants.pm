#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
#
# @author Joel Clawson
# @version 1.0 Original Creation
##Module------------------------------------------------------------------------
package ClassConstants;
use strict;

##------------------------------------------------------------------------------
# @signature float getBadFlag()
# <p>Get the value used for flagging a value as bad.
#
# @output $flag The bad flag: 3.0
##------------------------------------------------------------------------------
sub getBadFlag { return 3.0; }

##------------------------------------------------------------------------------
# @signature float getEstimateFlag()
# <p>Get the value used for flagging a value as estimated.
#
# @output $flag The estimate flag: 4.0
##------------------------------------------------------------------------------
sub getEstimateFlag { return 4.0; }

##------------------------------------------------------------------------------
# @signature float getGoodFlag()
# <p>Get the value used for flagging a value as good.
#
# @output $flag The good flag: 1.0
##------------------------------------------------------------------------------
sub getGoodFlag { return 1.0; }

##------------------------------------------------------------------------------
# @signature float getMissingFlag()
# <p>Get the value used for flagging a value as missing.
#
# @output $flag The missing flag: 9.0
##------------------------------------------------------------------------------
sub getMissingFlag { return 9.0; }

##------------------------------------------------------------------------------
# @signature float getQuestionableFlag()
# <p>Get the value used for flagging a value as questionable.
#
# @output $flag The questionable flag: 2.0
##------------------------------------------------------------------------------
sub getQuestionableFlag { return 2.0; }

##------------------------------------------------------------------------------
# @signature float getUncheckedFlag()
# <p>Get the value used for flagging a values as unchecked.
#
# @output $flag The unchecked flag: 99.0
##------------------------------------------------------------------------------
sub getUncheckedFlag { return 99.0; }

1;









