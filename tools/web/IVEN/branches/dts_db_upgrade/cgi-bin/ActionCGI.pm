#! /usr/bin/perl -w

package ActionCGI;
use strict;
use lib ".";
use IvenCGI;
our @ISA = ("IvenCGI");

sub buildPage {
    my ($self,$title) = @_;

    $self->processParams();

    print ($self->header());
    printf("<html>\n");
    printf("<head>\n");
    printf("   <title>Iven: %s</title>\n",$title);
    printf("   <link rel=stylesheet type=text/css href=%s>\n",$self->{"CSS"});
    printf("   <script language=javascript type=text/javascript src=%s></script>\n",
	   $self->{"JAVASCRIPT"});
    printf("</head>\n");
    printf("<body onLoad=\"javascript: closeSuccess();\">\n");
    $self->buildPageBody();
    printf("</body>\n");
    printf("</html>\n");

    $self->{"DB"}->disconnect();

}

sub delete {
    my ($self,$entry) = @_;
    $self->{"DB"}->delete($entry);
}

sub insert {
    my ($self,$entry) = @_;
    $self->{"DB"}->insert($entry);
}

sub processParams {}

sub update {
    my ($self,$entry) = @_;
    $self->{"DB"}->update($entry);
}

1;
