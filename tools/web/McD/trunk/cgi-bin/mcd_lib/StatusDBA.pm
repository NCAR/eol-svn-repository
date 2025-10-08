package StatusDBA;

use lib ".";
use Status;
use McDDB;

sub new
{
	my $class = shift;	
	my $self = {};
	bless( $self, $class );

	return $self;
}

sub insertDB
{
	my $self = shift;
	my $status = shift;

	my $dbh = McDDB::connect();

	my $sql= "INSERT INTO status VALUES( " . 
						$status->{dataset} . ", " .	
						$status->{documented} . ", " .	
						$status->{checked} . ", " .	
						$status->{loaded} . ", " .	
						$status->{in_progress} . ", " .	
						$status->{new} . ", " .	
						$status->{updated} . 
						")";

	$dbh->do( $sql ) || die( "Unable to insert status\n" );

	$dbh->disconnect();
}

sub updateDB
{
	my $self = shift;
	my $status = shift;
	my @fields = @_;

	if( scalar(@_) == 0 )
	{ @fields = ( "documented", "checked", "loaded", "in_progress", "new", "updated" ); }

	my $sql = "UPDATE status SET ";
	my $size = @fields;

	for( my $x = 0; $x < $size; $x++ )
	{
		$sql = $sql . "$fields[$x] = $status->{$fields[$x]}";
		if( $x != $size - 1 )
		{
			$sql = $sql . ", ";
		}
		else	
		{
			$sql = $sql . " ";
		}	
	}

	$sql = $sql . "WHERE dataset=$status->{dataset}";

	my $dbh = McDDB::connect();
	$dbh->do( $sql ) || die( "Unable to update status\n" );

	$dbh->disconnect();
}

1;
