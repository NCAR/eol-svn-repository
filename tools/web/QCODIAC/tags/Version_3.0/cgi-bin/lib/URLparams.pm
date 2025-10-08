#
# URLparams
# Date: (6/25/03)
#

# This package obtains all of the available parameters listed in the URL, and
# makes lists of their values avaible as output through and instantiated obj.

package URLparams;

use CGI;

$cgi = new CGI;
my %params = ();

# Grabs the parameters and their values from the URL and stores info.
# into a class hash table
sub new {
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	# Get a full listing of all the parameters in the URL
	my @parameters = $cgi->param();

	# Store the parameters and their values in a hash for this class
	for( my $x = 0; $x < @parameters; $x++ ) {
		my @temp = $cgi->param( $parameters[$x] );
		$params{ $parameters[$x] } = \@temp;
	}

	return $self;
}

# Returns the value associated with a queried id
sub get {
	return wantarray ? @{ $params{ $_[1] } } : $params{ $_[1] }->[0];
}

# returns a list of all available keys
sub availKeys {
	return keys %params;
}

# determines if a key exists or not
sub exists {
	return exists $params{ $_[1] };
}


1;

