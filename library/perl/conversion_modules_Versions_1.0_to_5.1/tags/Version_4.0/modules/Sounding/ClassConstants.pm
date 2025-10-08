#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The ClassConstants.pm module is a module of static functions or a module
# to be inherited for other modules that will use the Class Sounding format.  It
# contains functions for constant values that are unique to the CLASS format.</p>
#
# @author Joel Clawson
# @version 3.0 No changes made.  Version number incremented to maintain integrity
# between the different modules.
#
# @author Joel Clawson
# @version 2.0 Original Creation
##Module------------------------------------------------------------------------
package Sounding::ClassConstants;
use strict;
require Exporter;
our @ISA = ("Exporter");
our @EXPORT = qw($BAD_FLAG $ESTIMATE_FLAG $GOOD_FLAG $MISSING_FLAG $QUESTIONABLE_FLAG $UNCHECKED_FLAG);

our $GOOD_FLAG = 1.0;
our $QUESTIONABLE_FLAG = 2.0;
our $BAD_FLAG = 3.0;
our $ESTIMATE_FLAG = 4.0;
our $MISSING_FLAG = 9.0;
our $UNCHECKED_FLAG = 99.0;

1;









