#!/usr/bin/perl
#
# MSSfile object
# --------------
#
# Perl module to facilitate access to and manipulation of files on the UCAR/NCAR
# Mass Storage System (MSS). As of Version 1, this code only allows you to break
# a MSS file's path into directory and filename, and to perform mstouch and
# msretention on the file, but it could (should) be expanded in the future to do
# reecursive mstouch and msretention on a directory, to write files to the Mass
# Store, and pretty much anything else of use that is MSS file related.
#
# Note that the method find_taplog and the object variables _taplog and
# _searchfiles are specific to the RAF method of logging files on the mass 
# store, although they could be expanded to other logs, should they exist, in 
# the future.
#
# Version 1.0 (Original Version)
# 	Written by Janine Goldstein 4/11/2008
#
# Version 1.1	JAG 4/18/2008
#	Added block to confirm that the record from the taplog file in which we 
#	found the active filename begins with a flight number.  If it does not,
#	assume it is corrupt and warn the user.
#
# Version 1.2	JAG 7/9/2008
#	Commented out cos-blocked identification, because this was never implemented.
#	Just treat cos-blocked files like any other file and process them.
#
# Version 1.3   JAG 7/9/2008
#	Implemented cos-blocked identification so that if we get a purge notice
#	for a cos-blocked file, the code will try to update *ALL* files in the
#	taplog as cos-blocked files.
################################################################################
package MSSfile;

use strict;

# Constructor
sub new {

    # Use the $class variable to bless the hash reference, this making the
    # class inheritable. @_ contains the list of vars passed to the
    # subroutine. We will call this constructor with new MSSfile(), so
    # $_[0] will contain "MSSfile" which is the name of the class and thus
    # exactly what we need to bless our class
    my ($class) = @_;

    # Create a new object for our class. One can use any type of variable
    # as an object. I chose a hash.
    my $self = {
	_path		=> undef,
	_directory	=> undef,
	_filename	=> undef,
	_cosblocked	=> undef,
	_taplog		=> undef,
        # NOTE: searchfile does not have an accessor method b/c I only want it
        # accessible to this class and it's methods.
        _searchfiles 	=> "/scr/raf2/Prod_Data/archives/taplog/taplog.[0-9]*"
    };

    # The bless function indicates to the program that a variable belongs to 
    # a class. It takes two arguments: a reference to the variable to
    # be marked and a string containing the name of the class.
    bless $self, $class;

    return $self;
}

#Find the taplog file containing a listing for this MSSfile
sub find_taplog
{
    my ($self) = @_;
    my @lines;

    if ($self->cosblocked eq 'true')
        {
	if (`grep "MSS path name: *C*" $self->{_searchfiles}`)
	    {
            @lines = `grep $self->{_filename} $self->{_searchfiles}`;
	    $self->filename($self->filename.'C');
	    }
        }
    else
	{
        @lines = `grep $self->{_filename} $self->{_searchfiles}`;
        }

    # Check that the entry begins with a flight number. If not, it's either
    # not a record (maybe it's a comment or something), or it's an incomplete
    # or corrupt record, so warn user and remove it from @lines.
    my $index = 0;
    while ($index < scalar(@lines))
        {
	if ($lines[$index] !~ /: ?[RFT]F/) 
	    {
	    print "Line ".$lines[$index]." is corrupt or not a record line. Skip\n";
	    delete $lines[$index];
	    }
	$index++;
        }
	
    # If you don't find an entry at all, notify user
    if (@lines == 0) 
	{
	print "** WARNING: No logs found containing file ". +
	$self->filename." in path ".$self->{_searchfiles}."\n";
	print "Processing current file only.\n";
	$self->taplog("none");
	return;
	}

    # You should only find one entry, so warn user if you find more than one
    if (@lines > 1) 
	{
	print "WARNING: More than one entry found for file ".$self->filename."!\n";
	print "Only first taplog file is being processed!!!\n";
	foreach my $line (@lines)
	    {
	    print $line;
	    }
	}

    # If we get here, we found one and only one line containing our mss file
    # Break that line into subcomponents. Since this is output from a grep
    # command, the first component should be the name of the file containing
    # a listing for our mssfile.
    my $grepline = $lines[0];
    my @components = split(' ',$grepline);
    #debug# foreach $comp (@components) { print $comp."\n"; }

    # If the filename does not end in a colon, then there was no space at 
    # the beginning of the flight line, so we need to separate these and put
    # the flight number in the components array as it's own entry.
    if ($components[0] !~ /:$/)
	{
	# remove and return first element of @components
	my $first_comp = shift(@components); 

	# Split first element into two at the :
	my @sub_comp = split(':',$first_comp);

	# Prepend the two new elements to the components array
	# (actually append the components array to the two new
	# elements and then rename the new array to components)
	push(@sub_comp,@components);
	@components = @sub_comp;
	}

    # Otherwise, just remove the ':' from the end of the element
    else
	{
	chop $components[0];
	}

    #debug# foreach $comp (@components) { print $comp."\n"; }

    $self->taplog($components[0]);
    print "Entry ".$self->filename." found in file ".$self->taplog."\n";

}

#method to touch a mass store file, set the retention period to the max
#(32767 days) and check that changes were successful.
sub touch
{
    my ($self,$prod) = @_;

    # Touch the file
    my $command = "/net/local_lnx/bin/mstouch ".$self->path;
    print $command."\n";
    if ($prod) {system($command);}

    # Change the retention period to 32767 (the max)
    $command=
        "/net/local_lnx/bin/msretention -pe 32767 -wpwd RAFDMG ".$self->path;
    print $command."\n";
    if ($prod) {system($command);}

    # Now check to be sure changes took.
    if ($prod)
        {
        # List with the last access date rather than last modification date and
        # compare access date to today.  Should be the same.
        $command =  "/net/local_lnx/bin/msls -c ".$self->path;
        #debug# print $command."\n";
        my $today = substr(localtime()."",4,12);
    
	# Run command on system and capture STDOUT to $output.
        my $output = `$command`; 
	if ($output !~ /$today/)
	    {
	    print "ERROR: mstouch did not work: $output\n";
	    }
	else 
	    {
	    # do nothing - mstouch worked
	    }

        # List with the days until purge in place of the date and make sure
	# it is equal to 32767
        $command =  "/net/local_lnx/bin/msls -z ".$self->path;
        #debug# print $command."\n";
        $output = `$command`;
	if ($output !~ /32767/)
	    {
	    print "ERROR: msretention did not work: $output\n";
	    }
	else 
	    {
	    # do nothing - msretention worked
	    }
        }
}

#method to parse filename from path 
sub parse_filename_from_path 
{
    my ($self) = @_;

    # Don't process commented out lines.
    if ($self->path =~ /^\#/) {$self->filename("none"); return;}

    # Don't process blank lines.
    if ($self->path =~ /^$/) {$self->filename("none"); return;}

    print "\nProcessing MSS file: ".$self->path."\n";

    # Parse out the filename from the path
    my @components = split('/',$self->path);
    my $mssfile = @components[@components-1];

    # If the file to touch is a cos-blocked file (end in a C) then process
    # all cos-blocked files for the project found (by setting cos-blocked 
    # to true). 
    if ($mssfile =~ /C$/) 
        {
        $mssfile =~ s/C//;
        $self->cosblocked('true');
        print "File is cosblocked\n";
        }
    else
        {
        $self->cosblocked('false');
        }
    #debug# print $mssfile."\n"; 

    $self->filename($mssfile);

    # Parse out the directory from the path
    @components = split($mssfile,$self->path);
    $self->directory(@components[0]);
    #debug# $self->print();
}

#accessor method for MSSfile path (path+filename)
sub path 
{
    my ($self, $path) = @_;	# @_ contains the list of vars passed to
    				# this subroutine.
    $self->{_path} = $path if defined($path);
    return $self->{_path};
}

#accessor method for MSSfile directory.
sub directory {
    my ($self, $directory) = @_;# @_ contains the list of vars passed to
    				# this subroutine.
    $self->{_directory} = $directory if defined($directory);
    return $self->{_directory};
}
#accessor method for MSSfile filename.
sub filename {
    my ($self, $filename) = @_;	# @_ contains the list of vars passed to
    				# this subroutine.
    $self->{_filename} = $filename if defined($filename);
    return $self->{_filename};
}
#accessor method for MSSfile cosblocked.
sub cosblocked {
    my ($self, $cosblocked) = @_;# @_ contains the list of vars passed to
    				# this subroutine.
    $self->{_cosblocked} = $cosblocked if defined($cosblocked);
    return $self->{_cosblocked};
}
# accessor method for taplog
sub taplog {
    my ($self, $taplog) = @_;
    $self->{_taplog} = $taplog if defined($taplog);
    return ($self->{_taplog});
}

sub print {
   my ($self) = @_;

   # Print information about the MSS file
   printf( "%s\n", $self->path);
   printf( "%s%s %s\n", $self->directory, $self->filename,
       $self->cosblocked);

}  

1;
