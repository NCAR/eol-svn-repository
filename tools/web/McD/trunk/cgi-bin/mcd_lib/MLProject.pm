package MLProject;

use lib ".";
use Project;
@ISA = ("Project");  # Inherit the Project class

# --------------------------------------------------------------
# MLProject.pm:
#  Contains the MLProject class used to perform operations on the
#  database.  A single instance of the MLProject class corresponds
#  to an entry in the mlprojview table.
# 
# Author: Dan Sullivan
# Date: July, 2003
#                      -  -  -  -  - 
# Subroutines:
#
# sub new() - returns a new instance of the MLProject class
#
# sub set( @vals ) - assigns all of the fields in this MLProject
#  with the given values.
#
# sub checkDefined() - set default values to fields with undef()
#
# sub dbPrepare() - prepare the Project to be added/updated to 
#  database
# --------------------------------------------------------------

sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	$self->{logo_src} = "";
	$self->{left_css_src} = "";
	$self->{body_css_src} = "";

	return $self;
}

sub set
{
	my $self = shift;
	my $row = shift;

	$self->SUPER::set( $row );

	shift( @$row );
	$self->{logo_src} = shift( @$row );
	$self->{left_css_src} = shift( @$row );
	$self->{body_css_src} = shift( @$row );

	$self->checkDefined();
}

sub checkDefined
{
	my $self = shift;

	$self->{logo_src} = ( defined( $self->{logo_src} ) )? $self->{logo_src}: "";
	$self->{left_css_src} = ( defined( $self->{left_css_src} ) )? $self->{left_css_src}: "";
	$self->{body_css_src} = ( defined( $self->{body_css_src} ) )? $self->{body_css_src}: "";
} 

sub dbPrepare
{
	my $self = shift;

	$self->SUPER::dbPrepare();

	$self->{logo_src} = ( $self->{logo_src} ne "" )? $self->{logo_src}: undef();
	$self->{left_css_src} = ( $self->{left_css_src} ne "" )? $self->{left_css_src}: undef();
	$self->{body_css_src} = (  $self->{body_css_src} ne "" )? $self->{body_css_src}: undef();
}
1;
