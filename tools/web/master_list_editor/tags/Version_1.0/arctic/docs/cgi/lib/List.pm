package List;

# List.pm: containst the List class to hold the values in the List table
# This is a simple container class used the the master_query script

sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	# Items in the List table
	$self->{id} = 0;
	$self->{storm_id} = "99.999";
	$self->{title} = "";
	$self->{date} = "0000-00-00";
	$self->{author} = "";
	$self->{doc_url} = "";
	$self->{data_type_id} = 0;
	$self->{discipline_id} = 0;
	$self->{platform_id} = "";
	$self->{site_id} = 0;
	$self->{proj_id} = 0;
	$self->{commnets} = "";
	$self->{url} = "";
	$self->{phase} = "";
	$self->{start_year} = "";
	$self->{end_year} = "";
	$self->{spat_res_id} = 0;
	$self->{method_of_obs} = "";

	# The names of data_type, discipline...
	$self->{data_type} = "";
	$self->{discipline} = "";
	$self->{platform} = "";
	$self->{site} = "";
	$self->{spat_res} = "";

	return $self;
}

sub set
{
	my $self = shift;
	
	$self->{id} = shift;
	$self->{storm_id} = shift;
	$self->{title} = shift;
	$self->{date} = shift;
	$self->{author} = shift;
	$self->{doc_url} = shift;
	$self->{data_type_id} = shift;
	$self->{discipline_id} = shift;
	$self->{platform_id} = shift;
	$self->{site_id} = shift;
	$self->{proj_id} = shift;
	$self->{comments} = shift;
	$self->{url} = shift;
	$self->{phase} = shift;
	$self->{start_year} = shift;
	$self->{end_year} = shift;
	$self->{spat_res_id} = shift;
	$self->{method_of_obs} = shift;


	$self->{data_type} = shift;
	$self->{discipline} = shift;
	$self->{platform} = shift;
	$self->{site} = shift;
	$self->{spat_res} = shift;
}

1;
