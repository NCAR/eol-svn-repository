#!/usr/bin/perl -wT

# OAIXmlWriter
#  Author: Eric Dattore
#  Date: 2014-06-18
#

#-----------------------------------------------------------------------------#
#                                Subroutines                                  #
#-----------------------------------------------------------------------------#
# * new
# 	Creates a new instance of the OAIXmlWriter module
#
# * getOutput
# 	Returns the XML output from XML::Writer
#
# * writeError
# 	Helper function to write error tags when errors are thrown
#
# * openTag
# 	Opens a new tag, without attributes and inserts any text inside the tag
#
# * openTagWithAttributes
# 	Opens a new tag with attributes
#
# * closeTag
# 	Closes an open tag
#
# * writeResponseDate
# 	Writes the response date on the XML document according to OAI-PMH standards
#
# * writeRequest
# 	Writes the request tag on the XML document for the given OAI-PMH request
#
#-----------------------------------------------------------------------------#

use lib ".";
use XML::Writer;
use Data::Dumper;
use POSIX qw(strftime);

package OAIXmlWriter;

my $writer;
my $output;
my $schema;

sub new {
	my $class = shift;
	my $self = {};
	bless($self, $class);

	$self->{writer} = new XML::Writer(OUTPUT => $self->{output}, UNSAFE => 1, DATA_MODE => 1, DATA_INDENT => 2);
	$self->{writer}->xmlDecl("UTF-8");
	$self->{writer}->startTag("OAI-PMH", xmlns => "http://www.openarchives.org/OAI/2.0/", "xmlns:xsi" => "http://www.w3.org/2001/XMLSchema-instance", "xsi:schemaLocation" => "http://www.openarchives.org/OAI/2.0/ http://www.openarchives.org/OAI/2.0/OAI-PMH.xsd");

	return $self;
}

sub getOutput() {
	my $self = shift;

	$self->{writer}->endTag();
	$self->{writer}->end();

	return $self->{output};
}

sub writeError {
	my $self = shift;
	my $code = shift;
	my $message = shift;

	$self->{writer}->startTag("error", "code" => $code);
	$self->{writer}->characters($message) unless !defined $message;
	$self->{writer}->endTag();

	return $self;
}

sub openTag {
	my $self = shift;
	my $tagName = shift;
	my $text = shift;

	$self->{writer}->startTag($tagName);
	$self->{writer}->characters($text) unless $text eq undef;

	return $self;
}

sub openTagWithAttributes {
	my $self = shift;
	my $tagName = shift;
	my $text = shift;
	my @attr = @_;

	$self->{writer}->startTag($tagName, @attr);
	$self->{writer}->characters($text) unless !defined $text;

	return $self;
}

sub closeTag {
	my $self = shift;

	$self->{writer}->endTag();

	return $self;
}

sub writeResponseDate {
	my $self = shift;
	my $dateString = POSIX::strftime("%FT%H:%M:%SZ", gmtime);

	$self->{writer}->startTag("responseDate");
	$self->{writer}->characters($dateString);
	$self->{writer}->endTag();

	return $self;
}

sub writeRequest {
	my $self = shift;
	my $request = shift;
	my @attr = @_;

	openTagWithAttributes($self, "request", $request, @attr);
	$self->{writer}->endTag();

	return $self;
}

sub concatXml {
	my $self = shift;
	my $filename = shift;

	open(FILE, " < $filename");
	my $lines = join("\n", <FILE>);
	$lines =~ s/(<?.*\?>)//;

	$self->{writer}->raw($lines);

	return $self;
}

1;
