#! /usr/bin/perl -w

package EditorCGI;
use strict;
use lib ".";
use IvenCGI;
our @ISA = ("IvenCGI");

sub printButtonRow {
    my ($self,$mode,$cols) = @_;

    printf("      <tr>\n");
    printf("         <td class=buttons colspan=%d>\n",$cols);
    printf("            <input type=submit name=action value=Add />\n") if ($mode eq "Add");
    printf("            <input type=submit name=action value=Update />\n") if ($mode ne "Add");
    printf("            <input type=submit name=action onclick=\"javascript: return confirmDelete(this);\" value=Delete />\n") if ($mode ne "Add");
    printf("            <input type=button onclick=\"javascript: closeWindow();\" value=Cancel />\n");
    printf("         </td>\n");
    printf("      </tr>\n");
}

sub printStatusOptions {
    my ($self,$selected) = @_;
    $selected = "-1" unless(defined($selected));

    printf("<option value=\"-1\" %s>unassigned</option>\n",$selected == -1 ? "selected" : "");
    foreach my $status ($self->getStatusList()) {
	printf("<option value=%s %s>%s</option>\n",$status->getId(),
	       $selected == $status->getId() ? "selected" : "",$status->getName());
    }
}

sub printUserOptions {
    my ($self,$selected) = @_;
    $selected = -1 unless(defined($selected));

    printf("<option value=\"-1\" %s>unassigned</option>\n",$selected == -1 ? "selected" : "");
    foreach my $user ($self->getUserList()) {
	printf("<option value=%s %s>%s</option>\n",$user->getId(),
	       $selected == $user->getId() ? "selected" : "",$user->getName());
    }
}

1;
