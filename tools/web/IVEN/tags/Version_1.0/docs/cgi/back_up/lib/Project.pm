package Project;

use strict;

sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	$self->checkDefined();

	$self->{products} = undef();

	return $self;
}

sub checkDefined
{
	my $self = shift;

	$self->{pjname} = ( defined( $self->{pjname} ) )? $self->{pjname} : "";
	$self->{full_name} = ( defined( $self->{full_name} ) )? $self->{full_name} : "";
	$self->{storm_id_prefix} = ( defined( $self->{storm_id_prefix} ) )? $self->{storm_id_prefix} : "";
	$self->{begin_date} = ( defined( $self->{begin_date} ) )? $self->{begin_date} : "";
	$self->{end_date} = ( defined( $self->{end_date} ) )? $self->{end_date} : "";
	$self->{minlat} = ( defined( $self->{minlat} ) )? $self->{minlat} : "";
	$self->{maxlat} = ( defined( $self->{maxlat} ) )? $self->{maxlat} : "";
	$self->{minlon} = ( defined( $self->{minlon} ) )? $self->{minlon} : "";
	$self->{maxlon} = ( defined( $self->{maxlon} ) )? $self->{maxlon} : "";
	$self->{admin_notes} = ( defined( $self->{admin_notes} ) )? $self->{admin_notes} : "";
	$self->{sym_link} = ( defined( $self->{sym_link} ) )? $self->{sym_link} : "";
	$self->{charge_num} = ( defined( $self->{charge_num} ) )? $self->{charge_num} : "";
}

sub set
{
	my $self = shift;
	my $row = shift;

	$self->{pjname} = shift( @$row );
	$self->{full_name} = shift( @$row );
	$self->{storm_id_prefix} = shift( @$row );
	$self->{begin_date} = shift( @$row );
	$self->{end_date} = shift( @$row );
	$self->{minlat} = shift( @$row );
	$self->{maxlat} = shift( @$row );
	$self->{minlon} = shift( @$row );
	$self->{maxlon} = shift( @$row );
	$self->{admin_notes} = shift( @$row );
	$self->{sym_link} = shift( @$row );
	$self->{charge_num} = shift( @$row );

	$self->checkDefined();
}

sub dbPrepare
{
	my $self = shift;

	$self->{full_name} = ( $self->{full_name} && $self->{full_name} ne "" )? $self->{full_name} : undef();
	$self->{storm_id_prefix} = ( $self->{storm_id_prefix} && $self->{storm_id_prefix} ne "" )? $self->{storm_id_prefix} : undef();
	$self->{begin_date} = ( $self->{begin_date} && $self->{begin_date} ne "" )? $self->{begin_date} : undef();
	$self->{end_date} = ( $self->{end_date} && $self->{end_date} ne "" )? $self->{end_date} : undef();
	$self->{minlat} = ( $self->{minlat} && $self->{minlat} ne "" )? $self->{minlat} : undef();
	$self->{maxlat} = ( $self->{maxlat} && $self->{maxlat} ne "" )? $self->{maxlat} : undef();
	$self->{minlon} = ( $self->{minlon} && $self->{minlon} ne "" )? $self->{minlon} : undef();
	$self->{maxlon} = ( $self->{maxlon} && $self->{maxlon} ne "" )? $self->{maxlon} : undef();
	$self->{admin_notes} = ( $self->{admin_notes} && $self->{admin_notes} ne "" )? $self->{admin_notes} : undef();
	$self->{sym_link} = ( $self->{sym_link} && $self->{sym_link} ne "" )? $self->{sym_link} : undef();
	$self->{charge_num} = ( $self->{charge_num} && $self->{charge_num} ne "" )? $self->{charge_num} : undef();
}
1;
