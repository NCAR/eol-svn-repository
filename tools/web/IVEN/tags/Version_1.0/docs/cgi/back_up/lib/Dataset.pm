package Dataset;

use strict;

sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	$self->checkDefined();

	return $self;
}

sub checkDefined
{
	my $self = shift;

	foreach my $param ("dsname", "pdname", "pjname", "storm_id", "user_processing",
                     "raw_data", "final_data", "station_info", "software", "plots", "id_type", "platform_type",
                     "status_notes", "admin_notes", "status" )
	{
		$self->{$param} = defined( $self->{$param} )? $self->{$param} : "";
	}

	foreach my $param ("exclude", "questions" )
	{
		$self->{$param} = defined( $self->{$param} )? $self->{$param} : 0;
	}
}

sub set
{
	my $self = shift;
	my $row = shift;

	$self->{dsname} = shift( @$row );
	$self->{pdname} = shift( @$row );
	$self->{pjname} = shift( @$row );
	$self->{storm_id} = shift( @$row );
	$self->{user_processing} = shift( @$row );
	$self->{raw_data} = shift( @$row );
	$self->{final_data} = shift( @$row );
	$self->{station_info} = shift( @$row );
	$self->{software} = shift( @$row );
	$self->{plots} = shift( @$row );
	$self->{id_type} = shift( @$row );
	$self->{platform_type} = shift( @$row );
	$self->{status_notes} = shift( @$row );
	$self->{admin_notes} = shift( @$row );
	$self->{status} = shift( @$row );
	$self->{exclude} = shift( @$row );
	$self->{questions} = shift( @$row );

	$self->checkDefined();
}

sub dbPrepare
{
	my $self = shift;

	foreach my $param ("storm_id", "user_processing",
                     "raw_data", "final_data", "station_info", "software", "plots", "id_type", "platform_type",
                     "status_notes", "admin_notes", "status" )
	{
		$self->{$param} = defined( $self->{$param} ) && $self->{$param} ne "" ? $self->{$param} : undef();
	}

	foreach my $param ("exclude", "questions" )
	{
		$self->{$param} = defined( $self->{$param} )? $self->{$param} : 0;
	}
}

1;
