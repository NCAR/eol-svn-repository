#! /usr/bin/perl -w

package IvenCGI;
use strict;
use lib "lib";
use CGI;
use IvenDB;
use Status;
use URI;
use User;
our @ISA = ("CGI");

sub buildPage {
    my ($self,$title) = @_;

    print ($self->header());
    printf("<html>\n");
    printf("<head>\n");
    printf("   <title>Iven: %s</title>\n",$title);
    printf("   <link rel=stylesheet type=text/css href=%s>\n",$self->{"CSS"});
    printf("   <script language=javascript type=text/javascript src=%s></script>\n",
	   $self->{"JAVASCRIPT"});
    printf("</head>\n");
    printf("<body>\n");
    $self->buildPageBody();
    printf("</body>\n");
    printf("</html>\n");

    $self->{"DB"}->disconnect();
}

sub buildPageBody {}

sub getDataset {
    my ($self,$id) = @_;
    unless (defined($self->{"datasetEntry"})) {
	$self->{"datasetEntry"} = $self->{"DB"}->getDataset(defined($id) ? $id : $self->param("dataset"));
    }
    return $self->{"datasetEntry"};
}

sub getGroup {
    my ($self) = @_;
    unless (defined($self->{"groupEntry"})) {
	$self->{"groupEntry"} = $self->{"DB"}->getGroup($self->param("id"));
    }
    return $self->{"groupEntry"};
}

sub getProductList {
    my ($self,$project,$sort) = @_;
    $sort = "" unless(defined($sort));
    unless (defined($self->{"productList$sort"})) {
	push(@{ $self->{"productList$sort"}},$self->{"DB"}->getProductList($project,$sort));
    }
    return @{ $self->{"productList$sort"}};
}

sub getProject {
    my ($self) = @_;
    unless (defined($self->{"projectEntry"})) {
	$self->{"projectEntry"} = $self->{"DB"}->getProject($self->param("project"));
    }
    return $self->{"projectEntry"};
}

sub getProjectList {
    my ($self) = @_;
    unless (defined($self->{"projects"})) {
	push(@{ $self->{"projects"}},$self->{"DB"}->getProjectList());
    }
    return @{ $self->{"projects"}};
}

sub getStatusList {
    my ($self) = @_;
    unless (defined($self->{"statusList"})) {
	push(@{ $self->{"statusList"}},$self->{"DB"}->getStatusList());
    }
    return @{ $self->{"statusList"}};
}

sub getStatusMap {
    my ($self) = @_;
    unless (defined($self->{"statusMap"})) {
	foreach my $status ($self->getStatusList()) {
	    $self->{"statusMap"}->{$status->getId()} = $status;
	}
	$self->{"statusMap"}->{"-1"} = Status->new();
    }
    return $self->{"statusMap"};
}

sub getUserList {
    my ($self) = @_;
    unless (defined($self->{"userList"})) {
	push(@{ $self->{"userList"}},$self->{"DB"}->getUserList());
    }
    return @{ $self->{"userList"}};
}

sub getUserMap {
    my ($self) = @_;
    unless (defined($self->{"userMap"})) {
	foreach my $user ($self->getUserList()) {
	    $self->{"userMap"}->{$user->getId()} = $user;
	}
	$self->{"userMap"}->{"-1"} = User->new();
    }
    return $self->{"userMap"};
}

sub new {
    my $invocant = shift;
    my $class = $invocant || ref($invocant);
    my $self = CGI->new();
    bless($self,$class);

    $self->{"DB"} = IvenDB->new();

    $self->{"CSS"} = "/dpg/iven/dev/iven.css";
    $self->{"IMAGES"} = "/dpg/iven/dev/images";
    $self->{"JAVASCRIPT"} = "/dpg/iven/dev/iven.js";

    return $self;
}

sub uriEscape {
    my ($self,$param) = @_;
    return URI::Escape::uri_escape($param);
}
