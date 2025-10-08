#!/usr/bin/perl -wT

# OAIParser.pm
#  Author: Eric Dattore
#  Date: 2014-06-18
#
#-----------------------------------------------------------------------------#
#                                Subroutines                                  #
#-----------------------------------------------------------------------------#
# * new
# 	Creates a new instance of the OAIParser module
#
# * getVerb
# 	Fetches the request verb from the current URL
#
# * getOptions
# 	Fetches any URL options in the query string
#
# * getRecord
# 	Verb function that handles the GetRecord OAI verb
#
# * identify
# 	Verb function that handles the Identify OAI verb
#
# * listIdentifiers
#	Verb function that handles the ListIdentifiers OAI verb
#
# * listMetadataFormats
# 	Verb function that handles the ListMetadataFormats OAI verb
#
# * listRecords
# 	Verb function that handles the ListRecords OAI verb
#
# * listSets
# 	Verb function that handles the ListSets OAI verb
#
# * listBootstrap
#	Contains the core functionality for the
#	ListIdentifiers and ListRecords verbs
#
# * completeRequest
# 	Returns the XML output from the given OAI request
#
#-----------------------------------------------------------------------------#

package OAIParser;

# Custom Packages
use lib ".";
use URLparams;
use OAIXmlWriter;
use DataStore;

# CPAN Packages
use Data::Dumper;
use List::Flatten;

my $writer;
my $params;
my $verb;
my $dataStore;
my $error;
my @verbParams;
my @validVerbs = (
	'GetRecord',
	'Identify',
	'ListIdentifiers',
	'ListMetadataFormats',
	'ListRecords',
	'ListSets',
);

sub new {
	my $class = shift;
	my $self = {};
	bless($self, $class);

	$self->{params} = new URLparams;
	$self->{writer} = new OAIXmlWriter;
	$self->{writer}->writeResponseDate();
	$self->{dataStore} = new DataStore("lib/config.xml");
	$self->{error} = 0;

	return $self;
}

sub getVerb {
	my $self = shift;
	if (!$self->{params}->exists("verb") || $self->{params}->get("verb") eq "") {
		$self->{writer}->writeRequest(CGI::url());
		$self->{writer}->writeError("badVerb", "No verb specified!");
		$self->{error} = 1;
	} else {
		$self->{verb} = $self->{params}->get("verb");
	}

	return $self;
}

sub dispatchVerb {
	my $self = shift;

	if (grep($_ eq $self->{verb}, @validVerbs)) {
		my $verb = $self->{verb};
		$verb =~ s/(^[A-Z])(.*)/\l$1$2/;
		$verb->($self);
	} elsif ($self->{error} != 1) {
		$self->{writer}->writeRequest(CGI::url());
		$self->{writer}->writeError("badVerb", "The verb is invalid or unsupported");
		$self->{error} = 1;
	}

	return $self;
}

sub getOptions {
	my $self = shift;

	foreach my $key (sort $self->{params}->availKeys()) {
		if ($key eq 'verb') {
			next;
		}

		push(@{$self->{verbParams}}, $key);
	}

	return $self;
}

sub getRecord {
	my $self = shift;

	# Check for invalid arguments first
	if ($self->{verbParams} > 0) {
		foreach my $param (@{$self->{verbParams}}) {
			if ($param ne 'identifier' && $param ne 'metadataPrefix') {
				$self->{writer}->writeRequest(CGI::url());
				$self->{writer}->writeError("badArgument", "Parameter not allowed with this request");
				$error = 1;
				last;
			}
		}
	}

	if ($error == 1) {
		return $self;
	}

	my $identifier = $self->{params}->exists("identifier") ? $self->{params}->get("identifier") : undef;
	my $metadataPrefix = $self->{params}->exists("metadataPrefix") ? $self->{params}->get("metadataPrefix") : undef;

	if ($identifier eq undef || $metadataPrefix eq undef) {
		$self->{writer}->writeRequest(CGI::url());
		if ($identifier eq undef) {
			$self->{writer}->writeError("badArg", "'identifier' is missing");
			return $self;
		} else {
			$self->{writer}->writeError("badArg", "'metadataPrefix' required");
			return $self;
		}
	}

	$self->{writer}->writeRequest(CGI::url(), "verb", $self->{verb}, 'identifier', $identifier, 'metadataPrefix', $metadataPrefix);

	my @identifiers = flat $self->{dataStore}->getIdentifiers();

	if ($identifier !~ /oai:edu.ucar.eol:ds.*/) {
		$self->{writer}->writeError("badArg", "'$identifier' is not a valid oai identifier");
		return $self;
	}

	my $shortIdent;
	if ($identifier =~ /oai:edu.ucar.eol:ds(.*)/) {
		$shortIdent = $1;
	}

	# Check that the identifier exists

	my $identExist = grep { $_->{ident} eq $shortIdent } @identifiers;

	if ($identExist < 1) {
		$self->{writer}->writeError("idDoesNotExist");
		return $self;
	}

	# Check that the proper metadata format can be returned for the given valid identifier
	my @metfExists = grep { $_->{metf} eq $metadataPrefix && $_->{ident} eq $shortIdent } @identifiers;

	if (@metfExists < 1) {
		$self->{writer}->writeError("cannotDisseminateFormat");
		return $self;
	}

	# Print out metadata record
	$self->{writer}->openTag($self->{verb})->openTag("record");

	# Print header
	my $statTime = strftime("%FT%TZ", gmtime((stat $metfExists[0]{content})[9]));
	$self->{writer}->openTag("header")->openTag("identifier", $identifier)->closeTag()->openTag("datestamp", $statTime)
		->closeTag()->openTag("setSpec", $self->{dataStore}->getSetFromIdentifier($shortIdent))->closeTag()->closeTag();

	# Print metadata
	$self->{writer}->openTag("metadata")->concatXml($metfExists[0]{content})->closeTag();

	# Wrap everything up
	$self->{writer}->closeTag()->closeTag();

	return $self;
}

sub identify {
	my $self = shift;
	my $error = 0;

	$self->{writer}->writeRequest(CGI::url(), "verb", $self->{verb});

	if ($self->{verbParams} > 0) {
		$error = 1;
		foreach my $param (@{$self->{verbParams}}) {
			$self->{writer}->writeError("badArgument", "Parameter not allowed with this request");
		}
	}

	if ($error == 1) {
		return $self;
	}

	my %identity = %{$self->{dataStore}->getIdentity()};

	# Write out the proper XML response
	# With a fluent interface
	$self->{writer}->openTag($self->{verb})->openTag("repositoryName", $identity{repoName})->closeTag()
			->openTag("baseURL", CGI::url())->closeTag()->openTag("protocolVersion", $identity{protocolVersion})->closeTag()
			->openTag("adminEmail", $identity{adminEmail})->closeTag()
			->openTag("earliestDatestamp", $identity{earliestDatestamp})->closeTag()->openTag("deletedRecord", $identity{deletedRecord})
			->closeTag()->openTag("granularity", $identity{granularity})->closeTag()->openTag("description")
			->openTagWithAttributes("oai-identifier", undef, "xmlns",
				"http://www.openarchives.org/OAI/2.0/oai-identifier", "xmlns:xsi",
				"http://www.w3.org/2001/XMLSchema-instance", "xsi:schemaLocation",
				"http://www.openarchives.org/OAI/2.0/oai-identifier http://www.openarchives.org/OAI/2.0/oai-identifier.xsd")
			->openTag("scheme", "oai")->closeTag()->openTag("repositoryIdentifier", $identity{scheme}{repositoryIdentifier})->closeTag()
			->openTag("delimiter", ":")->closeTag()->openTag("sampleIdentifier", $identity{scheme}{sampleIdentifier})
			->closeTag()->closeTag()->closeTag()->openTag("description")->openTagWithAttributes("friends", undef, "xmlns", "http://www.openarchives.org/OAI/2.0/friends/", "xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance", "xsi:schemaLocation", "http://www.openarchives.org/OAI/2.0/friends/ http://www.openarchives.org/OAI/2.0/friends.xsd");

			if (@{$identity{friends}{friend}} > 0) {
				foreach my $friend (@{$identity{friends}{friend}}) {
					$self->{writer}->openTag("baseURL", $friend)->closeTag();
				}
			} else {
				$self->{writer}->openTag("baseURL", $identity{friends}{friend})->closeTag();
			}

			$self->{writer}->closeTag()->closeTag();

	# close verb tag
	$self->{writer}->closeTag();

	return $self;
}

sub listIdentifiers {
	my $self = shift;
	$self->listBootstrap('identifiers');

	return $self;
}

sub listMetadataFormats {
	my $self = shift;
	my $error = 0;
	my $identifier = undef;

	# $self->{writer}->writeRequest(CGI::url(), "verb", $self->{verb});

	if ($self->{verbParams} > 0) {
		foreach my $param (@{$self->{verbParams}}) {
			if ($param ne 'identifier') {
				$self->{writer}->writeError("badArgument", "Parameter not allowed with this request");
				$error = 1;
			} else {
				$identifier = $self->{params}->get("identifier");
			}
		}
	}

	if ($error == 1) {
		return $self;
	}


	# Search for available metadata formats for a given identifier
	if ($identifier ne undef) {
		$self->{writer}->writeRequest(CGI::url(), "verb", $self->{verb}, "identifier", $identifier);
		if ($identifier !~ /oai:edu.ucar.eol:ds.*/) {
			$self->{writer}->writeError("idDoesNotExist");
			return $self;
		}

		my $shortIdent;
		if ($identifier =~ /oai:edu.ucar.eol:ds(.*)/) {
			$shortIdent = $1;
		}

		# Check that the ident exists
		my @identifiers = flat $self->{dataStore}->getIdentifiers();
		my @identExist = grep { $_->{ident} eq $shortIdent } @identifiers;

		if (@identifiers < 1) {
			$self->{writer}->writeError("idDoesNotExist");
			return $self;
		}

		$self->{writer}->openTag($self->{verb});
		# Fetch metadata formats for the given identifier
		my @myFormats;
		foreach my $format (@identExist) {
			push @myFormats, $format->{metf};
		}

		my %hash = %{$self->{dataStore}->getFormats()};

		foreach my $key (sort keys %hash) {
			if (grep {$_ eq $hash{$key}->{shortname}} @myFormats) {
				$self->{writer}->openTag("metadataFormat")->openTag("metadataPrefix", $hash{$key}->{shortname})->closeTag();
				$self->{writer}->openTag("schema", $hash{$key}->{schema})->closeTag() unless $hash{$key}->{schema} eq "null";
				$self->{writer}->openTag("metadataNamespace", $hash{$key}->{namespace})->closeTag() unless $hash{$key}->{namespace} eq "null";

				$self->{writer}->closeTag();
			}
		}
	} else {
		$self->{writer}->writeRequest(CGI::url(), "verb", $self->{verb});
		$self->{writer}->openTag($self->{verb});
		my %hash = %{$self->{dataStore}->getFormats()};

		foreach my $key (sort keys %hash) {
			$self->{writer}->openTag("metadataFormat")->openTag("metadataPrefix", $hash{$key}->{shortname})->closeTag();

			if ($hash{$key}->{schema} > 1) {
				foreach (@{$hash{$key}->{schema}}) {
					$self->{writer}->openTag("schema", $_)->closeTag() unless $_ eq "null";
				}
			} else {
				$self->{writer}->openTag("schema", $hash{$key}->{schema})->closeTag() unless $hash{$key}->{schema} eq "null";
			}

			if ($hash{$key}->{namespace} > 1) {
				foreach (@{$hash{$key}->{namespace}}) {
					$self->{writer}->openTag("metadataNamespace", $_)->closeTag() unless $_ eq "null";
				}
			} else {
				$self->{writer}->openTag("metadataNamespace", $hash{$key}->{namespace})->closeTag() unless $hash{$key}->{namespace} eq "null";
			}

			$self->{writer}->closeTag();
		}
	}

	$self->{writer}->closeTag();

	return $self;
}

sub listRecords {
	my $self = shift;
	$self->listBootstrap('records');

	return $self;
}

sub listSets {
	my $self = shift;
	my $error = 0;

	$self->{writer}->writeRequest(CGI::url(), "verb", $self->{verb});

	if ($self->{params}->get("resumptionToken") ne "") {
		$self->{writer}->writeError("badResumptionToken", "resumptionToken not supported by this repository");
		return $self;
	}

	if ($self->{verbParams} > 0) {
		$error = 1;
		$self->{writer}->writeError("badArgument", "Paramter not allowed with this request.");
		return $self;
	}

	# Get all sets
	my @sets = $self->{dataStore}->getSets();

	if (@sets < 1) {
		$self->{writer}->writeError("noSetHierarchy", "Sets not supported");
		return $self;
	}

	# Get associated descriptions for the sets


	$self->{writer}->openTag($self->{verb});
	# Print out listing of all sets
	foreach my $set (@sets) {
		$self->{writer}->openTag("set")->openTag("setSpec", $set)->closeTag()->openTag("setName", $set)->closeTag()->closeTag();
	}

	$self->{writer}->closeTag();

	return $self;
}

sub listBootstrap {
	my $self = shift;
	my $type = shift;

	my ($metadataPrefix, $from, $until, $set);

	if ($self->{params}->get("resumptionToken") ne "") {
		$self->{writer}->writeRequest(CGI::url(), "verb", $self->{verb});
		$self->{writer}->writeError("badResumptionToken", "The resumption token isn't supported by this repository");
		return $self;
	}

	if ($self->{verbParams} > 0) {
		foreach my $param (@{$self->{verbParams}}) {
			if ($param ne 'from' && $param ne 'until' && $param ne 'metadataPrefix' && $param ne 'set') {
				$self->{writer}->writeRequest(CGI::url(), "verb", $self->{verb});
				$self->{writer}->writeError("badArgument", "Parameter not allowed with this request");
				return $self;
			}
		}
	}

	if ($self->{params}->exists("metadataPrefix")) {
		$metadataPrefix = $self->{params}->get('metadataPrefix');
		if ($self->{dataStore}->isValidMetadataFormat($metadataPrefix) != 1) {
			$self->{writer}->writeRequest(CGI::url(), "verb", $self->{verb});
			$self->{writer}->writeError("cannotDisseminateFormat");
			return $self;
		}
	} else {
		$self->{writer}->writeRequest(CGI::url(), "verb", $self->{verb});
		$self->{writer}->writeError("badArgument", "metadataPrefix required with this request");
		return $self;
	}

	if ($self->{params}->exists("from")) {
		$from = $self->{params}->get("from");
	}

	if ($self->{params}->exists("until")) {
		$until = $self->{params}->get("until");
	}

	if ($self->{params}->exists("set")) {
		$set = $self->{params}->get("set");
	}

	if ($from ne "" && $from !~ /[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z/) {
		$self->{writer}->writeRequest(CGI::url(), "verb", $self->{verb});
		$self->{writer}->writeError("badArgument", "'from' is not a valid UTCDatetime");
		return $self;
	}

	if ($until ne "" && $until !~ /[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z/) {
		$self->{writer}->writeRequest(CGI::url(), "verb", $self->{verb});
		$self->{writer}->writeError("badArgument", "'from' is not a valid UTCdatetime");
		return $self;
	}

	my @attributes = ("metadataPrefix", $metadataPrefix);

	if ($from ne "") {
		push @attributes, "from";
		push @attributes, $from;
	}

	if ($until ne "") {
		push @attributes, "until";
		push @attributes, $from;
	}

	if ($set ne "") {
		push @attributes, "set";
		push @attributes, $from;
	}

	$self->{writer}->writeRequest(CGI::url(), "verb", $self->{verb}, @attributes);

	# Filter all records to match
	my @identifiers = $self->{dataStore}->filterRecords($metadataPrefix, $from, $until, $set);

	if (@identifiers < 1) {
		$self->{writer}->writeError("noRecordsMatch");
		return $self;
	}

	$self->{writer}->openTag($self->{verb});

	if ($type eq 'records') {
		foreach my $record (@identifiers) {
			$self->{writer}->openTag("record");
			# Header
			$self->{writer}->openTag("header")->openTag("identifier", "oai:edu.ucar.eol:ds" . $record->{ident})->closeTag()
				->openTag("datestamp", strftime("%FT%TZ", gmtime((stat $record->{content})[9])))->closeTag();

				if (ref($record->{set}) eq 'ARRAY') {
					foreach (@{$record->{set}}) {
						$self->{writer}->openTag("setSpec", $_)->closeTag();
					}
				} else {
					$self->{writer}->openTag("setSpec", $record->{set})->closeTag();
				}

				$self->{writer}->closeTag();

			# Metadata
			$self->{writer}->openTag("metadata")->concatXml($record->{content})->closeTag()->closeTag();
		}

		$self->{writer}->closeTag();

	} elsif ($type eq 'identifiers') {
		foreach my $record (@identifiers) {
			$self->{writer}->openTag("header")->openTag("identifier", "oai:edu.ucar.eol:ds" . $record->{ident})->closeTag()
				->openTag("datestamp", strftime("%FT%TZ", gmtime((stat $record->{content})[9])))->closeTag();

				if (ref($record->{set}) eq 'ARRAY') {
					foreach (@{$record->{set}}) {
						$self->{writer}->openTag("setSpec", $_)->closeTag();
					}
				} else {
					$self->{writer}->openTag("setSpec", $record->{set})->closeTag();
				}

				$self->{writer}->closeTag();
		}

		$self->{writer}->closeTag();
	}

	return $self;
}

sub completeRequest {
	my $self = shift;

	return $self->{writer}->getOutput();
}

1;
