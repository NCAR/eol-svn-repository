#! /usr/bin/perl -w

##Module-----------------------------------------------------------------------
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlDatabase.pm>Link to MySqlDatabase.pm</a><br />
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlDataset.pm>Link to MySqlDataset.pm</a><br />
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlFile.pm>Link to MySqlFile.pm</a><br />
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlMSSFile.pm>Link to MySqlMSSFile.pm</a><br />
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlInserter.pm>Link to MySqlInserter.pm</a><br />
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlMSSInserter.pm>Link to MySqlMSSInserter.pm</a><br />
# <p>The MySqlMSSFile is a specialized MySqlFile that represents a file
# on the mass store system instead of on the local host system. All functions
# except new() are identical to those of MySqlFile.pm</p>
#
# @author Joel Clawson
##Module-----------------------------------------------------------------------
package MySqlMSSFile;
use strict;
use MySqlFile;
our @ISA = ("MySqlFile");

##-----------------------------------------------------------------------------
# @signature MySqlMSSFile new()
# <p>Create a new MySqlMSSFile.</p>
##-----------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);

    #$self->setHost("mass_store");
    $self->setHost("hpss");

    $self->setPurpose("data");

    return $self;
}

1;
