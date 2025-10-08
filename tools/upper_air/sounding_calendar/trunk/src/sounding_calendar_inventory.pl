#! /usr/bin/perl -w

##Module------------------------------------------------------------------------------
# <p>The sounding_inventory.pl program is a script that is used to generated HTML
# calendar documents that contain a list of soundings.  Information that the script
# uses is defined in a inv-config file.  The file must be named inv-dir where dir is
# the name of the directory where the station calendar inventories will be stored.</p>
#
# @author Joel Clawson
# @version 1.1 Adapted from a script written by Nick Tower.  This creates HTML calendars
#    instead of text calendars.
##Module------------------------------------------------------------------------------
package SoundingInventory;
use strict;

&main();

##------------------------------------------------------------------------------------
# @signature void main(String file)
# <p>Run the script.</p>
#
# @input $file The inventory configuration file.
##------------------------------------------------------------------------------------
sub main {
    if (@ARGV != 1) {
	printf("Inventory needs the configuration file.\n");
	exit(1);
    }
    my ($file) = @ARGV;

    my $iven = SoundingInventory->new($file);
    $iven->create_inventory_pages();
}

##------------------------------------------------------------------------------------
# @signature void create_calendar(FileHandle HTML, int year, int month)
# <p>Create a calendar for the specified month and year for the currently loaded
# station and print it to the HTML file handle.</p>
#
# @input $HTML The FileHandle where the HTML code will be printed.
# @input $year The year of the calendar.
# @input $month The month of the calendar.
##------------------------------------------------------------------------------------
sub create_calendar {
    my ($self,$HTML,$year,$month) = @_;

    # Load the calendar from the cal command.
    my $calendar = sprintf("/usr/bin/cal %d %d",$month,$year);
    my @lines = split('\n',`$calendar`);

    # Create the header lines of the calendar.
    printf($HTML "\n\n<br><br>\n<a name=%04d%02d></a>\n",$year,$month);
    printf($HTML "<table align=center border=1>\n");
    printf($HTML "  <tr><th colspan=7><h2>%s %04d</h2></th></tr>\n",
	   (get_month_list())[$month-1],$year);
    print($HTML "  <tr><th width=10%>Sunday</th>\n");
    print($HTML "      <th width=10%>Monday</th>\n");
    print($HTML "      <th width=10%>Tuesday</th>\n");
    print($HTML "      <th width=10%>Wednesday</th>\n");
    print($HTML "      <th width=10%>Thursday</th>\n");
    print($HTML "      <th width=10%>Friday</th>\n");
    print($HTML "      <th width=10%>Saturday</th></tr>\n");

    # Loop through the weeks.
    for (my $week = 2; $week < @lines; $week++) {
	my @days = ();
	my @hours = ();

	# Loop through the days for the week.
	for (my $day = 0; $day < 7; $day++) {
	    my $value = $day * 3 < length($lines[$week]) ? trim(substr($lines[$week],$day*3,3)) : "";
	    push(@days,$value);

	    # Determine hours data is available.
	    unless ($value =~ /^\s*$/) {
		foreach my $hour (keys(%{ $self->{"data"}->{$year}->{sprintf("%02d",$month)}->{sprintf("%02d",$value)}})) {
		    $hours[$hour] = 1;
		}
	    }
	}

	# Print out the days for the week and their soundings.
	printf($HTML "  <tr>\n");
	foreach my $day (@days) {
	    if ($day =~ /\d+/) {
		printf($HTML "      <td><table width=100%s><tr><th align=right>%d</th></tr>\n","%",$day);
		for (my $hour = 0; $hour < @hours; $hour++) {
		    if ($hours[$hour]) {
			my $has_hour = $self->{"data"}->{$year}->{sprintf("%02d",$month)}->{sprintf("%02d",$day)}->{sprintf("%02d",$hour)};
			printf($HTML "          <tr><td align=center>%s</td></tr>\n",
			       defined($has_hour) ? sprintf("%02d %s",$hour,join(",",sort(@{$has_hour}))) 
			       : "&nbsp;");
		    }
		}
		printf($HTML "          </table></td>\n");
	    } else {
		printf($HTML "      <td>&nbsp;</td>\n");
	    }
	}
    }

    printf($HTML "</table>\n");
}

##------------------------------------------------------------------------------------
# @signature void create_header(FileHandle HTML, String station)
# <p>Create the header of the HTML file that contains the station information and the
# table of months/years available in the document.</p>
#
# @input $HTML The FileHandle where the HTML code will be printed.
# @input $station The station being processed.
##------------------------------------------------------------------------------------
sub create_header {
    my ($self,$HTML,$station) = @_;

    # Create the station part of the header.
    printf($HTML "<h1 align=center>%s: %s</h1>\n",$station,$self->{"cfg"}->{"title"});
    printf($HTML "<h2 align=center>%s</h2>\n",$self->{"cfg"}->{"desc"});

    # Create the month for the header table.
    printf($HTML "<table align=center border=1>\n");
    print($HTML "<tr><th width=5%>Year</th>");
    foreach my $month (get_month_list()) {
	printf($HTML "<th width=7%s>%s</th>","%",$month);
    }

    # Loop through the years and create a link for available months for the current year.
    foreach my $year (sort(keys(%{ $self->{"data"}}))) {
	printf($HTML "<tr><th>%d</th>",$year);
	for (my $i = 1; $i <= 12; $i++) {
	    printf($HTML "<td align=center>%s</td>",
		   defined($self->{"data"}->{$year}->{sprintf("%02d",$i)}) ?
		   sprintf("<a href=\#%04d%02d>View</a>",$year,$i) : "&nbsp;");
	}
    }

    printf($HTML "</tr>\n");
    printf($HTML "</table>\n");
}

##------------------------------------------------------------------------------------
# @signature void create_inventory_pages()
# <p>Generate a HTML page for each station in the config file.</p>
##------------------------------------------------------------------------------------
sub create_inventory_pages {
    my ($self) = @_;

    foreach my $station (sort(split(' ',$self->{"cfg"}->{"stations"}))) {
	printf("Processing Station: %s\n",$station);
	undef($self->{"data"});
	$self->load_data($station);
	$self->create_station_page($station);
    }
}

##------------------------------------------------------------------------------------
# @signature void create_station_page(String station)
# <p>Generate the station calendar page for the specified station.</p>
#
# @input $station The station to use to generate the calendars.
##------------------------------------------------------------------------------------
sub create_station_page {
    my ($self,$station) = @_;
    my $directory = sprintf("%s/%s",$self->{"cfg"}->{"dest"},$self->{"cfg"}->{"network"});

    mkdir($directory) unless (-e $directory);

    open(my $HTML,sprintf(">%s/%s.inv.html",$directory,$station))
	or die("Cannot create output file\n");

    printf($HTML "<html>\n<head>\n<title>%s: %s</title>\n</head>\n<body>\n",
	   $station,$self->{"cfg"}->{"title"});

    $self->create_header($HTML,$station);
    foreach my $year (sort(keys(%{ $self->{"data"}}))) {
	foreach my $month (sort(keys(%{ $self->{"data"}->{$year}}))) {
	    $self->create_calendar($HTML,$year,$month);
	}
    }

    printf($HTML "</body>\n</html>\n");

    close($HTML);
}

##------------------------------------------------------------------------------------
# @signature String[] get_month_list()
# <p>Get the list of months for a year.</p>
##------------------------------------------------------------------------------------
sub get_month_list {
    return ("January","February","March","April","May","June","July","August",
	    "September","October","November","December");
}

##------------------------------------------------------------------------------------
# @signature void load_data(String station)
# <p>Load the date information for the files for the specified station.</p>
# 
# @input $station The station to load the data for.
##------------------------------------------------------------------------------------
sub load_data {
    my ($self,$station) = @_;

    # Generate the pattern from the file name.
    my $file_pattern = $self->{"cfg"}->{"filename"};
    $file_pattern =~ s/[YDhm]/\\d/g;
    $file_pattern =~ s/[M]/[\\dabcABC]/g;
    $file_pattern =~ s/S\+/$station/;

    # Allow variable stations in the directory path
    my $src_dir = $self->{"cfg"}->{"source"};
    $src_dir =~ s/S\+/$station/;

    # Get the list of files for the station.
    opendir(my $DIR,$src_dir) or die("Can't read $src_dir.\n");
    my @files = grep(/$file_pattern/,readdir($DIR));
    closedir($DIR);

    foreach my $file (sort(@files)) {
	$self->parse_date($station,$file);
    }
}

##------------------------------------------------------------------------------------
# @signature SoundingInventory new(String file)
# <p>Create a new instance with the specified config file.</p>
#
# @input $file The name of the config file.
##------------------------------------------------------------------------------------
sub new {
    my ($invocant,$file) = @_;
    my $self = {};
    my $class = $invocant || ref($invocant);
    bless($self,$class);

    $self->read_config_file($file);

    return $self;
}

##------------------------------------------------------------------------------------
# @signature void parse_date(String station, String file)
# <p>Parse out the date from the file name.</p>
#
# @input $station The station the file is associated with.
# @input $file The name of the file to be parsed.
##------------------------------------------------------------------------------------
sub parse_date {
    my ($self,$station,$file) = @_;

    my $pattern = $self->{"cfg"}->{"filename"};
    my ($year,$month,$day,$hour,$min);
    my $index = 0;
    my $offset = $pattern =~ /S\+/ ? length($station) - 2 : 0;

    # Read in the date parts.
    while ($index < length($pattern)) {
	my $char = substr($pattern,$index,1);

	if ($char eq "Y") { $year .= substr($file,$index+$offset,1); }
	elsif ($char eq "M") { $month .= substr($file,$index+$offset,1); }
	elsif ($char eq "D") { $day .= substr($file,$index+$offset,1); }
	elsif ($char eq "h") { $hour .= substr($file,$index+$offset,1); }
	elsif ($char eq "m") { $min .= substr($file,$index+$offset,1); }

	$index++;
    }

    # Handle special cases
    $year = $self->{"cfg"}->{"year"} if (!defined($year));
    if ($month =~ /a/i) { $month = 10; }
    elsif ($month =~ /b/i) { $month = 11; }
    elsif ($month =~ /c/i) { $month = 12; }
    $year += $self->{"cfg"}->{"century"} if (length($year) < 4);
    $min = 0 if (!defined($min));

    # Add the file to the list of minutes for the specified time.
    push(@{ $self->{"data"}->{$year}->{sprintf("%02d",$month)}->{sprintf("%02d",$day)}->{sprintf("%02d",$hour)}},sprintf("%02d",$min));
}

##------------------------------------------------------------------------------------
# @signature void read_config_file(String file)
# <p>Read the data in the config file into the object.</p>
#
# @input $file The name of the config file.
##------------------------------------------------------------------------------------
sub read_config_file {
    my ($self,$file) = @_;
    
    open(my $FILE,$file) or die("Cannot open $file\n");
    my @lines = <$FILE>;
    close($FILE);

    foreach my $line (@lines) {
	chomp($line);
	my ($key,$value) = split(/:/,$line);
	$self->{"cfg"}->{$key} = trim($value);
    }

    $self->{"cfg"}->{"network"} = substr($file,4);
}

##------------------------------------------------------------------------------------
# @signature String trim(String line)
# <p>Remove the surrounding whitespace of a String.</p>
#
# @input $line The line to be trimmed.
# @output $lien The trimmed line.
##------------------------------------------------------------------------------------
sub trim {
    my $line = shift;
    $line =~ s/^\s+//g;
    $line =~ s/\s+$//g;
    return $line;
}
