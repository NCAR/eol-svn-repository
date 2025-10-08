package State;

use lib "lib";
use Search;

# Dataset view or list view
$LIST = 1;
$DATASET = 2;

# List view types
$PROJECT = 3;
$LOADER = 4;
$CHECKER = 5;
$SEARCH = 6;

# Datasets per page
$DSS_PER_PAGE = 30;

my %sort_def = ( 	$PROJECT => "dataset.date", 
									$LOADER => "dataset.date",
									$CHECKER => "dataset.date",
									$SEARCH => "dataset.date" 
							 );
my %hide_def = ( $PROJECT => [],
									$LOADER => ["loaded"],
									$CHECKER => ["checked"], 
									$SEARCH => [] 
							);
							
sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );
	
	$self->{view} = undef();  					# Either $LIST or $DATASET
	$self->{list_view_type} = undef(); 	# When view=$LIST, $PROJECT, $LOADER, or $CHECKER or $SEARCH
	$self->{list_view_id} = undef(); 		# id number when view=$LIST
	$self->{hide} = (); 
	$self->{sort} = undef();

	$self->{page} = undef();
	$self->{dataset_num} = undef();

	$self->{edit} = undef();
	$self->{proc_edit} = undef();

	# An istance of Seach if searching!!
	$self->{search} = undef();

	return $self;
}

sub setDefaults
{
	my $self = shift;

	$self->{hide} = $hide_def{$self->{list_view_type}};
	$self->{sort} = $sort_def{$self->{list_view_type}};

	if( $self->{view} eq $DATASET )
	{ $self->{dataset_num} = 0; }
	else
	{ $self->{dataset_num} = undef(); }

	$self->{page} = 1;

	$self->{edit} = undef();
	$self->{proc_edit} = undef();

	$self->{search} = undef();
}

sub getUrlString
{
	my $self = shift;
	my %args = (
							"view" => undef(),
							"list_view_type" => undef(),
							"list_view_id" => undef(),
							"hide" => undef(),
							"sort" => undef(),
							"dataset_num" => undef(),
							"edit" => undef(),
							"proc_edit" => undef(),
							"page" => undef(),
							@_
							);
	foreach my $arg (keys %args)
	{
		$args{$arg} = $self->{$arg} if( !defined( $args{$arg} ) );
	}

	my $str = "view=$args{view}&" .
						"list_view_type=$args{list_view_type}&" .
						"list_view_id=$args{list_view_id}&" .
						"sort=$args{sort}";

	$str = $str . "&dataset_num=$args{dataset_num}" if( defined( $args{dataset_num} ) ); 
	$str = $str . "&page=$args{page}" if( defined( $args{page} ) ); 
	$str = $str . "&edit=1" if( defined( $args{edit} ) );
	$str = $str . "&proc_edit=1" if( defined( $args{proc_edit} ) );
	
	foreach my $h (@{$args{hide}})
	{
		$str = $str . "&hide=$h";
	}

	if( defined( $self->{search} ) )
	{
		$str = $str . "&" . $self->{search}->getUrlString();
	}

	return $str;
}

sub getHiddenFieldString
{
	my $self = shift;
	my $hidden =	"<input type=hidden name=\"view\" value=\"$self->{view}\">\n" . 
								"<input type=hidden name=\"list_view_type\" value=\"$self->{list_view_type}\">\n".
								"<input type=hidden name=\"list_view_id\" value=\"$self->{list_view_id}\">\n".
								"<input type=hidden name=\"sort\" value=\"$self->{sort}\">\n";

	foreach my $h (@{$self->{hide}})
	{
		$hidden = $hidden . "<input type=hidden name=\"hide\" value=\"$h\">\n";	
	}

	$hidden = $hidden . "<input type=hidden name=\"dataset_num\" value=\"$self->{dataset_num}\">\n" if( defined( $self->{dataset_num} ) );
	$hidden = $hidden . "<input type=hidden name=\"page\" value=\"$self->{page}\">\n" if( defined( $self->{page} ) );

	if( defined( $self->{search} ) )
	{
		$hidden = $hidden . $self->{search}->getHiddenFieldString();
	}				
	
	return $hidden;				
}

sub setFromParams
{
	my $self = shift;
	my $cgi = shift;

	$self->{view} = $cgi->param( "view" );
	die( "View not defined!\n" ) if( !defined( $self->{view}) );

	$self->{list_view_type} = $cgi->param( "list_view_type" );
	die( "List view type not defined!\n" ) if( !defined( $self->{list_view_type}) );

	$self->{list_view_id} = $cgi->param( "list_view_id" );
	die( "List view id not defined!\n" ) if( !defined( $self->{list_view_id}) );

	$self->setDefaults();

	$self->{sort} = $cgi->param( "sort" ) if( defined( $cgi->param( "sort" ) ) );

	$self->{hide} = [];	
	my @hide = $cgi->param( "hide" );
	if( @hide && scalar( @hide ) != 0 )
	{
		$self->{hide} = [];
		my $c = 0;
		foreach my $h (@hide)
		{
			$self->{hide}->[$c++] = $h;	
		}
	}
	
	$self->{dataset_num} = $cgi->param( "dataset_num" ) if( defined( $cgi->param( "dataset_num" ) ) );
	$self->{page} = $cgi->param( "page" ) if( defined( $cgi->param( "page" ) ) );
	$self->{edit} = $cgi->param( "edit" );
	$self->{proc_edit} = $cgi->param( "proc_edit" );

	# Get the search if searching 
	if( $self->{list_view_type} == $SEARCH )
	{
		$self->{search} = Search->new();
		$self->{search}->setFromParams( $cgi );
	}
}
1;
