package State;

use lib ".";
use Utils;

require Exporter;
use vars qw(@ISA @EXPORT $VERSION );
@ISA = qw(Exporter);
@EXPORT = qw( $PROJECT $PRODUCT $USER $THREAD $DATASET);

# View types
$PROJECT = 1;
$PRODUCT = 2;
$USER = 3;
$THREAD = 4;
$DATASET = 5;

sub new
{
	my $class = shift;
	my $self = {};

	$self->{view} = undef(); # $PROJECT, $PRODUCT, $USER, $THREAD
	$self->{project} = undef(); # Name of project viewing

	$self->{product} = undef(); # Name of product viewing, only in $PRODUCT view
	$self->{dataset} = undef();  # Name of the dataset viewing, only in $DATASET view
	$self->{user} = undef(); # Name of user viewing, only in $USER view

	$self->{edit} = undef();
	$self->{proc_edit} = undef();

	bless( $self, $class );
}

sub setFromParams
{
	my $self = shift;
	my $cgi = shift;

	$self->{view} = $cgi->param( "view" );
	$self->{project} = $cgi->param( "project" );
	$self->{product} = $cgi->param( "product" );
	$self->{dataset} = $cgi->param( "dataset" );
	$self->{user} = $cgi->param( "user" );
	$self->{edit} = $cgi->param( "edit" );
	$self->{proc_edit} = $cgi->param( "proc_edit" );
}

sub getUrlString
{
	my $self = shift;
	my %args = (
							"view" => undef(),
							"project" => undef(),
							"product" => undef(),
							"user" => undef(),
							"dataset" => undef(),
							"edit" => undef(),
							"proc_edit" => undef(),
							@_
						 );

	foreach my $a (keys %args)
	{
		$args{$a} = $self->{$a} if( !defined( $args{$a} ) );
	}

	my $url = "controller?";

	if( !$args{edit} )
	{
		if( $args{view} == $PROJECT )
		{
			$args{product} = undef();
			$args{dataset} = undef();
			$args{user} = undef();
		}
		elsif( $args{view} == $PRODUCT )
		{
			$args{dataset} = undef();
			$args{user} = undef();
		}
		elsif( $args{view} == $DATASET )
		{
			$args{user} = undef();
		}
		elsif( $args{view} == $USER )
		{
			$args{product} = undef();
			$args{dataset} = undef();
		}
	}

	foreach my $a (keys %args)
	{
		$url = $url . "$a=" . uri_escape($args{$a}) . "&" if( defined( $args{$a} ) );
	}

	return $url;
}

sub getHiddenFieldString
{
	my $self = shift;
	my %args = (
							"view" => undef(),
							"project" => undef(),
							"product" => undef(),
							"user" => undef(),
							"dataset" => undef(),
							"edit" => undef(),
							"proc_edit" => undef(),
							@_
						 );

	foreach my $a (keys %args)
	{
		$args{$a} = $self->{$a} if( !defined( $args{$a} ) );
	}

	my $hidden = "";

	if( $args{view} == $PROJECT )
	{
		$args{product} = undef();
		$args{dataset} = undef();
		$args{user} = undef();
	}
	elsif( $args{view} == $PRODUCT )
	{
		$args{dataset} = undef();
		$args{user} = undef();
	}
	elsif( $args{view} == $DATASET )
	{
		$args{user} = undef();
	}
	elsif( $args{view} == $USER )
	{
		$args{product} = undef();
		$args{dataset} = undef();
	}

	foreach my $a (keys %args)
	{
		$hidden = $hidden . "<input type=hidden value=\"" . uri_escape($args{$a}) . "\">" if( defined( $args{$a} ) );
	}

	return $hidden;
}

sub setDefaults
{
	my $self = shift;

	$self->{view} = $PROJECT;
	$self->{project} = undef();
}

1;
