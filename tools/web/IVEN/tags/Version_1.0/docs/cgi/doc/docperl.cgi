#! /usr/bin/perl -w

##Module--------------------------------------------------------------------
# <p>The docperl.cgi program is a cgi script used to create html documentation
# pages for perl.</p>
#
# @use http://www.joss.ucar.edu/cgi-bin/dpg/doc/docperl.cgi?file=FILENAME
# where FILENAME is the full path and name of the file to be documented.
#
# @author Joel Clawson
# @version 1.0 Original Creation
##Module--------------------------------------------------------------------

use strict;
use CGI qw(:standard :html3);

my $cgi = CGI->new();

main();

sub main() {
    # Create the header for the documentation
    print $cgi->header();
    print "<html><head><title>Conversions</title>\n";
    print "<link rel=\"STYLESHEET\" type=\"text/css\"";
    #print "href=\"http://www.joss.ucar.edu/~suldan/iven/doc/docperl.css\">\n";
    print "href=\"http://www.joss.ucar.edu/dpg/iven/doc/docperl.css\">\n";
    print "</head><body>\n";

    my $file = $cgi->param("file");
    open (FILE,$file) || die "There was a problem openning $file.\n";
    my $start = 0; # Marker to hold if a documented section has started
    my $mod_descript = 1; 
    my $lineCount = 0; # Used to keep track of file's line number for errors
    my %functions; # hash of functions in the file.
    my $currentFunction; # name of the current function
    while (<FILE>) {
	$lineCount++;
	if ($start) { # Check to see if a doc section has started
	    if ($_ =~ /^\#\#/) { # Check for end of doc section
		$start = 0; 
	    } else { # Contains documentation
		my $line = substr($_, 2); # remove the comment marker
		if ($line =~ /^[\@]/) { # see if the line is the 1st @ id
		    my $atLines = $line;
		    while (($line = <FILE>) !~ /^\#\#/) { # Get all the rest
			$atLines .= " ".substr($line, 2);
			$lineCount++;
		    }
		    $start = 0; # Mark documentation ended
		    # save the @id lines
		    $functions{$currentFunction}->{"at_lines"} = $atLines;
		} elsif ($line ne "") { # ignore blank lines in description
		    if (defined($functions{$currentFunction}->{"description"})) {
			$functions{$currentFunction}->{"description"} .= $line;
		    } else {
			$functions{$currentFunction}->{"description"} = $line;
		    }
		}
	    }
	} elsif ($_ =~ /^\#\#/ && !$start) { # Start of perldoc
	    $start = 1; # Starting a documentation block.
	    if ($mod_descript && $_ =~ /^\#\#Module/) { # Start Module def
		$currentFunction = "mod_descript";
		my $line = <FILE>; # Read the next line
		$lineCount++;
		while ($line !~ /^\#\#Module/) { # read to end of module def
		    $line = substr($line, 1); # Remove #
		    if (defined($functions{$currentFunction}->{"description"})) {
			$functions{$currentFunction}->{"description"} .= $line;
		    } else {
			$functions{$currentFunction}->{"description"} = $line;
		    }
		    
		    $line = <FILE>; # Read the next line
		    $lineCount++;
		}
		$mod_descript = 0;
		$start = 0;
	    } else { # Read function perldoc documentation
		$lineCount++;
		my $signatureLine = <FILE>;
		chomp $signatureLine;
		my @data = split('[\s()]', $signatureLine);
		
		# Make sure 1st line of doc is @signature line
		if ($data[1] ne "\@signature") {
		    print "At line $lineCount, the \@signature is required.";
		    die;
		}
		
		# parse the signature line
		my $nextIndex; # Used to index through signature list
		my $returnType = ""; # Stores return type
		if ($data[2] eq "") { # multiple return types
		    $returnType = "(";
		    $nextIndex = 3;
		    while ($data[$nextIndex] ne "") { # Builds return type
			$returnType .= $data[$nextIndex];
			if ($data[$nextIndex + 1] ne "") {
			    $returnType .= " ";
			}
			$nextIndex++;
		    }
		    $returnType .= ")";
		    $nextIndex++;
		} else { # Single value return type
		    $returnType = $data[2];
		    $nextIndex = 3;
		}
		
		my $function = $data[$nextIndex++];
		
		# Build the paramter list
		my $params = "(";
		while ($nextIndex < scalar(@data)) {
		    $params .= $data[$nextIndex];
		    if (defined($data[$nextIndex + 1])) {
			$params .= " ";
		    }
		    $nextIndex++;
		}
		$params .= ")";
		
		# Need both function and params in case of overloading.
		$currentFunction = $function.$params;
		$functions{$currentFunction}->{"return_type"} = $returnType;
		$functions{$currentFunction}->{"parameters"} = $params;
	    }
	}
    }
    close(FILE);

    #---------------------------------------------------------------------
    # Build the HTML document for the file.
    #---------------------------------------------------------------------
    $file =~ /(.+)[\/]([^\/]+)/;

    # Create the general documentation based on the file name
    print "<h1>".$2." Documentation</h1>\n";
    print "<p><b>Module: </b>".$2."</p>\n";

    # Build the module description
    my @mod_data = split("\@", $functions{"mod_descript"}->{"description"});
    my $count = 0;
    foreach my $data (@mod_data) {
	if ($count == 0) {
	    print $data."\n";
	} elsif ($data =~ /^use/) { # Recognize @use id
	    $data =~ s/\n/<br>/g;
	    print "<p><b>Use: </b>".substr($data, 5)."</p>\n";
	} elsif ($data =~ /^author/) { # Recognize @author id
	    print "<p><b>Author: </b>".substr($data, 7)."</p>\n";
	} elsif ($data =~ /^version/) { # Recognize @version id
	    print "<p><b>Version: </b>".substr($data, 7)."</p>\n";
	}
	$count++;
    }
    print "<b>Location: </b>".$1."<br>\n";
    print "<b>Link to this Page: </b><a href=http://www.joss.ucar.edu/cgi-bin/dpg/dev/iven/doc/docperl.cgi?file=$file>http://www.joss.ucar.edu/cgi-bin/dpg/dev/iven/doc/docperl.cgi?file=$file</a>\n";
    print "<hr><br>\n";
    delete $functions{"mod_descript"}; # Remove description from function list

    # Build the table containing the list of functions.
    print "<table border=2 width=100%><tr><th colspan=2>";
    print "Function List</th></tr>\n";
    foreach my $function (sort keys %functions) { # loop through funcs
	print "<tr><td>".$functions{$function}->{"return_type"}."</td>\n";
	$function =~ /^(.+)[(](.*)[)]$/;
	print "<td><a href=\#".$function."><b>".$1."</b>";
	print $functions{$function}->{"parameters"}."</a></td></tr>\n";
    }
    print "</table><br><hr>\n";

    # Build the individual descriptions for each function.
    foreach my $function (sort keys %functions) {
	print "<h3><a name=".$function.">".$function."</a></h3>\n";
	print $functions{$function}->{"description"}."\n";
	my @input;
	my @output;
	my @warning;
	my @limitation;
	my @link;
	my @atList = split("\@", $functions{$function}->{"at_lines"});
	foreach my $item (@atList) {
	    if ($item ne "") { # needed because split gives "" as first element
		my @itemList = split(' ', $item, 2); # Get id and description
		if ($itemList[0] eq "input") { # Recognize @input id
		    push @input, $itemList[1];
		} elsif ($itemList[0] eq "output") { # Recognize @output id
		    push @output, $itemList[1];
		} elsif ($itemList[0] eq "warning") { # Recognize @warning id
		    push @warning, $itemList[1];
		} elsif ($itemList[0] eq "limitation") { # Recognize @limitation id
		    push @limitation, $itemList[1];
		} elsif ($itemList[0] eq "link") { # Recognize @link id
		    push @link, $itemList[1];
		}
	    }
	}
	if (scalar(@input) > 0) { # Display input list if it exists
	    print "<dl><dt class=input>Inputs:</dt><dd>\n";
	    foreach my $in (@input) {
		my @items = split(' ', $in, 2);
		print "<code>".$items[0]."</code>- ".$items[1]."<br>\n";
	    }	
	    print "</dd></dl>\n";
	}
	if (scalar(@output) > 0) { # Display output list if it exists
	    print "<dl><dt class=output>Outputs:</dt><dd>\n";
	    foreach my $out (@output) {
		my @items = split(' ', $out, 2);
		print "<code>".$items[0]."</code>- ".$items[1]."<br>\n";
	    }
	    print "</dd></dl>\n";
	}
	if (scalar(@warning) > 0) { # Display warning list if it exists
	    print "<dl><dt class=warning>Warnings:</dt><dd>\n";
	    foreach my $warn (@warning) {
		print $warn."<br>";
	    }
	    print "</dd></dl>\n";
	}
	if (scalar(@limitation) > 0) { # Display limitation list if it exists
	    print "<dl><dt class=limitation>Limitations:</dt><dd>\n";
	    foreach my $limit (@limitation) {
		print $limit."<br>";
	    }
	    print "</dd></dl>\n";
	}
	if (scalar(@link) > 0) { # Display link list if it exists.
	    print "<dl><dt class=link>Links:</dt><dd>\n";
	    foreach my $lnk (@link) {
		print $lnk."<br>";
	    }
	    print "</dd></dl>\n";
	}
	print "<hr>";
    }
    print "<p>Created by perldoc.cgi</p>\n";
    print $cgi->end_html(); 
}
