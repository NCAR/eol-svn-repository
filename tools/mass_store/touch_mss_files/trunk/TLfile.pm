#!/usr/bin/perl

# class TLfile
package TLfile;

use lib "/net/work/lib/perl/MassStore";
use MSSfile;
use strict;

# @ISA governs inheritance. ISA stands for "is a" as in TLfile "is a" MSSfile
# @ISA loads the MSSfile class and declares that the TLfile class inherits
# methods from it.
our @ISA = qw(MSSfile);	# inherits from MSSfile

# constructor (extends the MSSfile constructor)
sub new {
    my ($class) = @_;

    #call the constructor of the parent class
    my $self = $class->SUPER::new();

    #add some new stuff
    $self->{_TLnumber} = undef;
    $self->{_pathTemplate} = undef;
    $self->{_header} = undef;
    $self->{_taplogRecord} = undef;

    bless $self, $class;

    return $self;
}
# Create path to file on Mass Store
sub get_path_to_file
{
    #/RAF/1983/765/fltno/LRT/Gxxxxx
    #/RAF/1985/260/fltno/RMGRxx
    #/RAF/1985/TL0091/HAWAxx
    #/RAF/TLxxxx/GGxxxxx. 
    # This method will parse the record from the taplog
    # file and create the MSS path to the file corresponding to this
    # record. Eg: Record:
    #  RF01A   TL0688   G51780   08/10/1984   08:42:03   09:18:27  0.491520
    # and the path generated is:
    #  /RAF/TL0688/G51780

    my ($self) = @_;

    my @column_headings = split(' ',$self->header);
    my @record_vals = split(' ',$self->taplogRecord);
    my @template = split('/',$self->pathTemplate);
    # template begins with a / so remove it from array
    if ($template[0] == "") {shift(@template);}
    my $numcols = scalar(@column_headings);
    my $numrecs = scalar(@record_vals);

    # Relate headings to values in a hash
    my %column = ();
    my $index = 0;
    my $maxindex = $numrecs;
    if ($numcols != $numrecs)
         {
	 #Match cols to recs up to whichever array is shorter.
	 if ($numcols < $numrecs) {$maxindex = $numcols};
         }
    while ($index < $maxindex) 
        {
	# print $column_headings[$index]."->";
	# print $record_vals[$index]."\n";

        # do some simple value checking
        if ($column_headings[$index] =~ /fltno/)
	    {
	    # remove * from end of fltno if it exists
	    if ($record_vals[$index] =~ /\*/)
	        {$record_vals[$index] =~ s/\*//;}

	    if ($record_vals[$index] !~ /^[RTF]F/) 
	        {
	        print "ERROR: Bad flight # ".$record_vals[$index]. +
		    " in record\n".$self->taplogRecord."Line not processed.\n";
	        $self->path("none");
	        return;
	        }
	    }
        if ($column_headings[$index] =~ /TLxxxx/ && 
	                $record_vals[$index] !~ /^TL/) 
	    {
	    print "ERROR: Bad TL # ".$record_vals[$index]." in record\n". +
	    	$self->taplogRecord."Line not processed.\n";
	    $self->path("none");
	    return;
	    }
        if ($column_headings[$index] =~ /Gxxxxx/ && 
	    $record_vals[$index] !~ /^[G0-9][A-Z0-9]{5}/) 
	    {
	    print "ERROR: Bad G # ".$record_vals[$index]." in record\n". +
	        $self->taplogRecord."Line not processed.\n";
	    $self->path("none");
	    return;
	    }

	$column{$column_headings[$index]} = $record_vals[$index];
	$index++;
        }

    # Loop through all the pieces in the template and replace those that
    # occur in the record, as determined by the column headings, with the
    # values from the record.
    my @mss_pieces = ("");
    foreach my $piece (@template) {
        foreach my $heading (@column_headings) {
	    if ($piece eq $heading) {
		#debug# print $heading."->".$column{$heading}."\n";
		$piece = $column{$heading};
	    }
	}
	push(@mss_pieces,$piece);
    }
    my $mss_path = join('/',@mss_pieces);
    #debbug# print $mss_path."\n";
    $self->path($mss_path);
}

# accessor method for TLnumber
sub TLnumber {
    my ($self, $TLnumber) = @_;
    $self->{_TLnumber} = $TLnumber if defined($TLnumber);
    return ($self->{_TLnumber});
}
# accessor method for pathTemplate
sub pathTemplate {
    my ($self, $pathTemplate) = @_;
    $self->{_pathTemplate} = $pathTemplate if defined($pathTemplate);
    return ($self->{_pathTemplate});
}
# accessor method for header
sub header {
    my ($self, $header) = @_;
    $self->{_header} = $header if defined($header);
    return ($self->{_header});
}
# accessor method for taplogRecord
sub taplogRecord {
    my ($self, $taplogRecord) = @_;
    $self->{_taplogRecord} = $taplogRecord if defined($taplogRecord);
    return ($self->{_taplogRecord});
}

sub print {
    my ($self) = @_;
    # call the print method of the parent class
    $self->SUPER::print;
    print $self->TLnumber."\n";
    print $self->pathTemplate;
}

1;
