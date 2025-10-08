

package Note;
use strict;
use IvenDB;

sub getAuthor {
    my ($self) = @_;
    return $self->{"author"};
}

sub getEntryDate {
    my ($self) = @_;
    return $self->{"entry_date"};
}

sub getNoteId {
    my ($self) = @_;
    return $self->{"note_id"};
}

sub getNoteText {
    my ($self) = @_;
    return $self->{"note_text"};
}

sub getRowReviseTime {
    my ($self) = @_;
    return $self->{"row_revise_time"};
}

sub new {
    my $invocant = shift;
    my $class = $invocant || ref($invocant);
    my $self = {};
    bless($self, $class);
    
    return $self;
}

sub setAuthor {
    my ($self, $author) = @_;
    $self->{"author"} = $author;
}

sub setEntryDate {
    my ($self, $date) = @_;
    $self->{"entry_date"} = $date;
}

sub setNoteId {
    my ($self, $id) = @_;
    $self->{"note_id"} = $id;
}

sub setNoteText {
    my ($self, $text) = @_;
    $self->{"note_text"} = $text;
}

sub setRowReviseTime {
    my ($self, $timestamp) = @_;
    $self->{"row_revise_time"} = $timestamp;
}

1;
