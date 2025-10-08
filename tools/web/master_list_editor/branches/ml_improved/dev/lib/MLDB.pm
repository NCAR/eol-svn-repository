package MLDB;

# --------------------------------------------------------------
# MLDB.pm - a simple module to return a connection to the 
#  needed database.
#
# Author: Dan Sullivan,
# Date: July, 2003
# 
# Subroutines:
#
# sub connect() - returns a connection to the ml database.
# --------------------------------------------------------------

use DBI;

sub connect
{
  return DBI->connect( "DBI:mysql:database=ml;host=hurricane",
                        "stormdba", "codiac", { RaiseError=>1} ) || die( "Unable to connect to database: ml" );
}

1;

