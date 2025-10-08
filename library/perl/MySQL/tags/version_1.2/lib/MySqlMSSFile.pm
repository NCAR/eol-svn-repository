#! /usr/bin/perl -w

##Module-----------------------------------------------------------------------
# <p>The MySqlMSSFile is a specialized MySqlFile that represents a file
# on the mass store system instead of on the local host system.</p>
#
# @author Joel Clawson
##Module-----------------------------------------------------------------------
package MySqlMSSFile;
use strict;
use MySqlFile;
our @ISA = ("MySqlFile");

##-----------------------------------------------------------------------------
# @signature MySqlFile new()
# <p>Create a new MySqlFile.</p>
##-----------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);

    $self->setHost("mass_store");
    $self->setPurpose("data");

    return $self;
}

##-----------------------------------------------------------------------------
# @signature void setFile(String dir, String file)
# <p>Set the location and name of the file.</p>
#
# @input $dir The directory where the file is stored.
# @input $file The name of the file.
##-----------------------------------------------------------------------------
sub setFile {
    my $self = shift;
    $self->{"directory"} = $_[0];
    $self->{"filename"} = $_[1];

    my @file_data;
    if ($ENV{"HOST"} =~ /^tornado/) {
       @file_data = split(' ',`/usr/local/bin/msls -l $_[0]/$_[1]`);
    } elsif ($ENV{"HOST"} =~ /^tsunami/) {
       @file_data = split(' ',`/net/local_lnx/dcs/bin/msls -l $_[0]/$_[1]`);
    } else {
       die("MySQLMSSFile does not location of msls for ".$ENV{"HOST"}."\n");
    }
    $self->{"size_kb"} = int($file_data[4]/1024)+1 if ($file_data[4] != 0);
}

1;







