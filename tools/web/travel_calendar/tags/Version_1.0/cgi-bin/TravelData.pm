#! /usr/bin/perl -w

##Module--------------------------------------------------------------------
# <p>The <code>travel.cgi</code> script is to generate HTML pages for 
# JOSS personel to create, edit and delete dates/times when they will be
# out of the office.  This is to replace the HTML travel page that needs to
# be manually editted.</p>
#
# @author Joel Clawson
# @version 0.01 Original Creation
##Module--------------------------------------------------------------------
package TravelData;
use strict;
#use lib ".";
use lib "/work/DPG_HTML/BEST_SW/conversion_modules/Version1";
use Conversions;
#use TravelData;
#use CGI qw(:standard :html3);
#our @ISA = ("CGI");

sub addTravel {
    my $self = shift;
    my $name = shift;
    my $start = shift;
    my $end = shift;
    my $why = shift;
    my $where = shift;
    my $line_no = scalar(keys %{ $self->{"data"}}) + 1;
    $self->{"data"}->{$line_no} = "$name,$start,$end,$why,$where";
    $self->save();
}

sub deleteTravel {
    my $self = shift;
    my $line_no = shift;
    delete $self->{"data"}->{$line_no};
    $self->save();
}

sub editTravel {
    my $self = shift;
    my $line_no = shift;
    my $name = shift;
    my $start = shift;
    my $end = shift;
    my $why = shift;
    my $where = shift;
    $self->{"data"}->{$line_no} = "$name,$start,$end,$why,$where";
    $self->save();
}

sub getData {
    my $self = shift;
    my $line = shift;
    return $self->{"data"}->{$line};
}

sub getEndDate {
    my $self = shift;
    return (split(",", $self->getData(shift)))[2];
}

sub getName {
    my $self = shift;
    my @data = split(",", $self->getData(shift));
    return $data[0];
}

sub getPeopleGone {
    my $self = shift;
    my $start_date = shift;
    my $end_date = shift;
    if (!defined($end_date)) { $end_date = $start_date; }
    my @people = ();

    foreach my $key (keys %{ $self->{"data"}}) {
	(my $name, my $start, my $end, my $why, my $where) =
	    split(",", $self->getData($key));

	if ((Conversions::compareDates($start_date, $start) <= 0 &&
	   Conversions::compareDates($start, $end_date) <= 0) ||
	    (Conversions::compareDates($start_date, $end) <= 0 &&
	   Conversions::compareDates($end, $end_date) <= 0) ||
	    (Conversions::compareDates($start, $end_date) <= 0 &&
	   Conversions::compareDates($start_date, $end) <= 0)) {
	    push(@people, $key);
	}
    }

    return @people;
}

sub getReason {
    my $self = shift;
    my @data = split(",", $self->getData(shift));
    return $data[3];
}

sub getStartDate {
    my $self = shift;
    my @data = split(",", $self->getData(shift));
    return $data[1];
}

sub getWhere {
    my $self = shift;
    return (split(",", $self->getData(shift)))[4];
}

sub loadData {
    my $self = shift;
    my $line_no = 1;
    open(INFILE, "<travel.dat") || die;
    while (<INFILE>) {
	my $line = $_;
	chomp($line);
	$self->{"data"}->{$line_no} = $line;
	$line_no++;
    }
    close(INFILE);
}

sub new {
    my $invocant = shift;
    my $station = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self, $class);
    $self->loadData();
    return $self;
}

sub save {
    my $self = shift;
    open(OUTFILE, ">travel.dat") || die "Cannot open file travel.dat";
    foreach my $key (sort keys %{ $self->{"data"}}) {
	print OUTFILE $self->{"data"}->{$key}."\n";
    }
    close(OUTFILE);
}
1;
