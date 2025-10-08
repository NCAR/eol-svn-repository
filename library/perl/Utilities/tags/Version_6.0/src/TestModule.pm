#! /usr/bin/perl -w

package TestModule;
use strict;

sub assertFormatString {
  $_[0]->assertString(sprintf("%7.2f", $_[1]), sprintf("%7.2f", $_[2]), $_[3]);
}

sub assertString {
  if ($_[1] eq $_[2]) {
  } else { printf("Expected %s, but was %s for %s\n", $_[1], $_[2], $_[3]); }
}

sub assertUndef {
  if (defined($_[1])) {
    printf("Value was not NULL for %s\n", $_[2]);
  }
}

sub assertValue {
  if (sprintf("%f",$_[1]) ne sprintf("%f",$_[2])) {
    printf("Expected %f, but was %f for %s\n", $_[1], $_[2], $_[3]);
  }
}

sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self, $class);
    return $self;
}
1;
