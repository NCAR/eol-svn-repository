#! /usr/bin/perl -w
##Module------------------------------------------------------------------------
# Module to read in basic configuration files with lines like 
#
# key=value
# key=->configFile
# #comment
#
# where configFile allows inheritance of values in other files.
# 
# Janine Aquino 9/20/2012
##Module------------------------------------------------------------------------
package ReadConfig;
use strict;

##-----------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);

    return $self;
}
##-----------------------------------------------------------------------------
# Given a configuration file, read through it until find a given key and
# return the value. If given configuration file inherits from other files,
# will look into those as needed.
#
# @input attribute to retrieve value for
# @input file in which to look
# @output value
##-----------------------------------------------------------------------------
sub GetAttr {
        my $self = shift;
	my $attr = shift;
	my $filename = shift;
	my $temp = "";
	
	#print "$filename\n";
	open TFH, $filename;
	#print "$filename\n";
	while (<TFH>) {
		if ($_ =~ /(.*)=(.*)/) {
			if ($1 eq $attr) { 
			    $temp = $2;
			    close TFH;
			    if ($temp =~ /->(.*)/)
			    {
				my @split1 = split(/:/, $1);
				my $subfilename = $split1[0];
				my $subattr = $attr;
				if ($#split1 == 1)
				{
				    $subattr = $split1[1];
				}
				return $self->GetAttr($subattr, $subfilename);
				last;
			    }
			    else {
				return $temp;
			        last;	
			    }
			}
		}	
	}
	close TFH;
	return "";
}
##-----------------------------------------------------------------------------
sub ProcAttr
{
    my $self = shift;
    my $attr = shift;
    my $value = shift;

    my @split = split(/;/, $value);
    my $return = "";

    foreach my $command (@split)
    {
	if ($command =~ /^->(.*)$/)
	{
		my @arg = split(/:/, $1);
		if ($#arg == 1)
		{
		    $return .= $self->GetAttr($arg[1], $arg[0]);
		}
		else
		{
		    $return .= $self->GetAttr($attr, $arg[0]);
		}
	}
	elsif ($command =~ /^\/\.(.+)->(.+)$/)
	{
	    #Putting $1 and $2 in $s and $r is neccessary for reasons beyond my understanding 
	    # s/$1/$2/g gets a uninitialized error on $2..
	    my $s = $1;
 	    my $r = $2;
	    $return =~ s/$s/$r/g;
	}
	elsif ($command =~ /^\+(.*)$/)
	{
		$return += $1;
	}
	else
	{
		$return .= $command;
	}
    }

   return $return;
}
##-----------------------------------------------------------------------------
sub read_config_file {
    my $self = shift;
    my $filename = shift;

    open PFH, $filename;
    while (<PFH>) {
	my $line = $_;
	if ($line =~ /^#/) { next; }
	if ($line =~ /(.*)=(.*)/) {
                push @{ $_[0] }, $1;
		push @{ $_[1] }, $self->ProcAttr($1, $2);
	}
    }
    close PFH;
}
##------------------------------------------------------------------------------
1;
