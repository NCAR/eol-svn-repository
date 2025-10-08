#!/usr/bin/perl -Tw

# DataStore
#  Author: Eric Dattore
#  Date: 2014-06-18
#
#-----------------------------------------------------------------------------#
#                                Subroutines                                  #
#-----------------------------------------------------------------------------#
# * new
# 	Creates a new instance of the DataStore module
#
# * getFormats
# 	Returns all the XML metadata formats that can be returned by the OAI-PMH
# 	repository from a given configuration file.
#
# * getIdentity
#	Returns a hash corresponding to all the indentity information for the
#	repository.
#
# * getIdentifiers
#	Returns an array of hashes containing information about each record
#
# * getSetFromIdentifier
#	Returns the set that the identifier belongs to
#
# * getSets
#	Returns an array of hashes containing the repository's sets
#
# * filterRecords
#	Most commonly used with the List* verbs. It takes in four parameters
#	and finds all records that match and returns them as an array of hashes
#
# * isValidMetadataFormat
#	Returns a Boolean integer depending on whether the given format is
#	supported by the repository
#
#-----------------------------------------------------------------------------#

use lib ".";
use XML::Simple;
use Data::Dumper;
use Date::Manip;

package DataStore;

my $parser;
my $config;
my $retrievalMethod;

sub new {
	my $class = shift;
	my $self = {};
	my $configPath = shift;

	bless($self, $class);

	$self->{parser} = new XML::Simple;
	$self->{config} = $self->{parser}->XMLin($configPath);
	$self->{retrievalMethod} = $self->{parser}->{driver}->{name};

	return $self;
}

sub getIdentity {
	my $self = shift;

	return $self->{config}->{identity};
}

sub getFormats {
	my $self = shift;
	my $config = shift;

	return $self->{config}->{metadataFormats}->{format};
}

sub getIdentifiers {
	my $self = shift;
	my $set = shift;
	my %files = %{$self->{config}->{files}->{set}};
	my @ident;

	foreach my $key (sort keys %files) {
		push (@ident, $files{$key}->{file});
	}

	return @ident;
}

sub getSetFromIdentifier {
	my $self = shift;
	my $ident = shift;
	my %sets = %{$self->{config}->{files}->{set}};

	foreach my $set (sort keys %sets) {
		foreach my $file (@{$sets{$set}->{file}}) {
			if ($file->{ident} eq $ident) {
				return $set;
			}
		}
	}

	return "";
}

sub getSets {
	my $self = shift;
	my %sets = %{$self->{config}->{files}->{set}};
	my @sets = ();

	foreach my $key (sort keys %sets) {
		push @sets, $key;
	}

	return @sets;
}

# For ListRecords and GetRecords, used to filter records based on the four URL parameter criteria
sub filterRecords {
	my $self = shift;
	my $prefix = shift;
	my $from = shift;
	my $until = shift;
	my $set = shift;
	my $f = undef;
	my $dtFrom;
	my $dtUntil;

	if ($from ne undef || $until ne undef) {
		# $f = new DateTime::Format::Strptime(pattern => "%FT%TZ");
		if ($from ne undef) {
			# $dtFrom = $f->parse_datetime($from);
			$dtFrom = new Date::Manip::Date;
			$dtFrom->parse($from);
		}

		if ($until ne undef) {
			# $dtUntil = $f->parse_datetime($until);
			$dtUntil = new Date::Manip::Date;
			$dtUntil->parse($until);
		}
	}

	my @files = ();
	my @filteredRecords = ();

	my %records = %{$self->{config}->{files}->{set}};

	# Sort on set first
	if ($set ne undef) {
		@files = @{$records{$set}->{file}};
	} else {
		foreach my $key (sort keys %records) {
			foreach (@{$records{$key}->{file}}) {
				$_->{set} = $key;
			}
			push @files, @{$records{$key}->{file}};
		}
	}

	# Sort on From and Until then Prefix
	foreach my $file (@files) {
		my $passes = 1;
		# Filter the date
		if ($from ne undef || $until ne undef) {
			# my $dtFile = $f->parse_datetime(POSIX::strftime("%FT%TZ", gmtime((stat $file->{content})[9])));
			my $dtFile = new Date::Manip::Date;
			$dtFile->parse(POSIX::strftime("%FT%TZ", gmtime((stat $file->{content})[9])));
			# Compare on $from
			if ($from ne undef) {
				# if ($dtFile->compare($dtFrom) < 0) {
				if ($dtFile->cmp($dtFrom) < 0) {
					# Set $passed to false
					$passes = 0;
				}
			}

			# Compare on $until
			if ($until ne undef) {
				# if ($dtFile->compare($dtUntil) > 0) {
				if ($dtFile->cmp($dtUntil) > 0) {
					# Set $passed to false
					$passes = 0;
				}
			}
		}

		# Filter on prefix
		if ($file->{metf} ne $prefix) {
			$passes = 0;
		}

		# Filter for previous record
		if (grep { $file->{content} eq $_->{content} } @filteredRecords) {
			$passes = 0;
			foreach (@filteredRecords) {
				if ($_->{content} eq $file->{content}) {
					if (ref($_->{set}) eq 'ARRAY') {
						push @{$_->{set}}, $file->{set};
					} else {
						$_->{set} = [$_->{set}, $file->{set}];
					}
				}
			}
		}

		if ($passes != 0) {
			$file->{set} = $set unless $set eq undef;
			push @filteredRecords, $file;
		}
	}

	return @filteredRecords;
}

sub isValidMetadataFormat {
	my $self = shift;
	my $format = shift;
	my $result = 0;

	my %formats = %{$self->getFormats()};

	$result = grep { $_->{shortname} eq $format } %formats;

	return $result;
}

1;
