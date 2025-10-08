

package Status;
use strict;
use IvenDB;

sub new {
    my $invocant = shift;
    my $class = $invocant || ref($invocant);
    my $self = {};
    bless($self, $class);
    return $self;
}

sub getName {
    my ($self) = @_;
    return $self->{"name"};
}

sub getSortIndex {
    my ($self) = @_;
    return $self->{"sort_index"};
}

sub getStatusMap {
    my $map = {};
    
    my $dbh = IvenDB::getConnection();
    my $stmt = $dbh->prepare("SELECT status_id, name, css_style, sort_index, is_resolved FROM status");
    $stmt->execute();

    while ((my @row = $stmt->fetchrow())) {
        my $status = Status->new();
        $status->setStatusId($row[0]);
        $status->setName($row[1]);
        $status->setStyle($row[2]);
        $status->setSortIndex($row[3]);
        $status->setResolved($row[4]);
        $map->{$status->getStatusId()} = $status;
    }

    return $map;
}

sub getStatusId {
    my ($self) = @_;
    return $self->{"status_id"};
}

sub getStyle {
    my ($self) = @_;
    return $self->{"style"};
}

sub isResolved {
    my ($self) = @_;
    return $self->{"resolved"};
}

sub setName {
    my ($self, $name) = @_;
    $self->{"name"} = $name;
}

sub setResolved {
    my ($self, $resolved) = @_;
    $self->{"resolved"} = $resolved;
}

sub setSortIndex {
    my ($self, $index) = @_;
    $self->{"sort_index"} = $index;
}

sub setStatusId {
    my ($self, $id) = @_;
    $self->{"status_id"} = $id;
}

sub setStyle {
    my ($self, $style) = @_;
    $self->{"style"} = $style;
}

1;
