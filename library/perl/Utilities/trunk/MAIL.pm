#! /usr/bin/perl -w

##Module----------------------------------------------------------------------
# <p>The HPSS.pm module is a class that controls the interaction
# between a Perl script and the HPSS.</p>
#
# @author Sean Stroble
#
# @version 1.1 changed default $receipients to be eol-cds-ingest to 
# match what other ingest scripts are doing (LEH 29 Oct 2018)
##Module----------------------------------------------------------------------
package MAIL;
use strict;
use Email::MIME;
use Email::MIME::Creator;

##-----------------------------------------------------------------------------
# @signature void send_mail(String subject, String body)
# <p>Send an email to the group of receipients with the specified subject and
# text.</p>
#
# @input $subject The subject of the email.
# @input $body The message of the email.
##-----------------------------------------------------------------------------
sub send_mail {
    my ($subject,$body,$receipients) = @_;

    my $sender = "joss\@ucar.edu";
    # my $reply_to = "eol-archive\@ucar.edu";
    my $reply_to = "eol-cds-ingest\@ucar.edu";
    if (!defined $receipients || $receipients eq "") {$receipients = "eol-cds-ingest\@ucar.edu"; }
    
    my @parts = (Email::MIME->create(attributes => { content_type => "text/plain" },
                                     body => $body));

    my $email = Email::MIME->create(parts => [ @parts ]);

    $email->header_set("From" => $sender);
    $email->header_set("Reply-To" => $reply_to);
    $email->header_set("To" => $receipients);
    $email->header_set("Subject" => $subject);

    open(my $SENDMAIL,"|/usr/lib/sendmail -t") || die("Unable to open sendmail\n");
    printf($SENDMAIL $email->as_string());
    close($SENDMAIL);
}

1;
