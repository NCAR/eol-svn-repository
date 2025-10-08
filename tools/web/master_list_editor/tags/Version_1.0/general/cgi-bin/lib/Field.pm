#!/usr/bin/perl -w

package Field;

#----------------------------------------------------
# Field.pm:  This module contains the Field class.
#		A Field is a generic object to handle all of the
#		fields of the data notes pages.  A field is given
#		one of several types and then the subs called are
#		dynamically determined by the given type.
#
#	Possible types: 
#			text - a simple text box
#			checkbox - a checkbox
#			select - a drop down menu
#			textarea - a large textarea 
#			date - a date consisiting of three fields: 
#					month, day, and year
#
# Subroutines:
#		new - the constructor, intializes a new Field
#		write - writes the value of the field
#		read - reads the value of the field
#		htm_tag - returns a sting containing the necessary
#			html code for the entry form
#		read_param - reads in the value for the field using
#			the CGI modules param() subroutine.
#		print_val - returns the value of the field with the
#			needed html formatting	
#
# Properties - general		 
#		name - the name of field
#		type - the type of field (text, select...)
#		value - the value of field
#
#	Properties - html formating - used by htm_tag
#		size - the size of the input field
#		tabindex - the tabindex of the input field
#		onClick - the javascript to perform when field
#			is clicked
#		onChange - the javascript to perform when the
#			field is changed
#		maxlength - the max length of the text in the 
#			field
#		othertagmods - a reference to an array that contains 
#			other html tag modifiers, if the type is select this
#			must reference the array that contains the options 
#			for the drop down box.
#----------------------------------------------------

use strict;
use CGI qw(:standard :html3 );

# new( name, type, value )
sub new
{
	my $classname = shift;
	my $name = shift;
	my $type = shift;
	my $value = shift;
	my $self = {};
	bless( $self, $classname );

	# Function pointers
#	$self->{write} = \&write;
#	$self->{read} = \&read;

	# Properties - general
	$self->{name} = $name;
	$self->{type} = $type;
	$self->{value} = $value;

	# Properties - html
	$self->{size} = undef;
	$self->{tabindex} = undef;
	$self->{onClick} = undef;
	$self->{onChange} = undef;
	$self->{maxlength} = undef;
	$self->{othertagmods} = undef;
	$self->{link} = undef;
	return $self;
}

sub write
{
	my $self = shift;
	*OUT = shift;

	print( OUT $self->{value}, "\n" );

}

sub read
{
	my $self = shift;
	*INPUT = shift;

	$self->{value} = <INPUT>;
}

sub htm_tag
{
	my $self = shift;
	my $meth = "htm_tag_$self->{type}";

	return $self->$meth;
}
sub htm_tag_text
{
	my $self = shift;
	my $string = "<INPUT TYPE=text NAME=\"$self->{name}\" VALUE=\"$self->{value}\"";

	if( $self->{size} ) { $string = $string . " SIZE=$self->{size}"; }
	if( $self->{tabindex} ) { $string = $string . " TABINDEX=$self->{tabindex}"; }
	if( $self->{onClick} ) { $string = $string . " onClick=\"$self->{onClick}\""; }
	if( $self->{onChange} ) { $string = $string . " onChange=\"$self->{onChange}\""; }
	if( $self->{maxlength} ) { $string = $string . " MAXLENGTH=$self->{maxlength}"; }  
	if( $self->{othertagmods} ) 
	{
		my @arr = @{$self->{othertagmods}}; 
		for( my $x = 0; $x < @arr; $x++ )
		{
			$string = $string . " $arr[$x]"; 
		}
	}
	
	$string = $string . "></input>";

	return $string; 

}
sub htm_tag_checkbox
{
	my $self = shift;
	my $string = "<INPUT TYPE=checkbox NAME=\"$self->{name}\"";

	if( $self->{tabindex} ) { $string = $string . " TABINDEX=$self->{tabindex}"; }
	if( $self->{onClick} ) { $string = $string . " onClick=\"$self->{onClick}\""; }
	if( $self->{onChange} ) { $string = $string . " onChange=\"$self->{onChange}\""; }

	if( $self->{othertagmods} ) 
	{
		my @arr = @{$self->{othertagmods}}; 
		for( my $x = 0; $x < @arr; $x++ )
		{
			$string = $string . " $arr[$x]"; 
		}
	}
	if( $self->{value} eq "Y" ) { $string = $string . " checked"; }

	
	$string = $string . "></input>";

	return $string; 
}
sub htm_tag_date
{
	my $self = shift;
	my $string;
	my $date = $self->{value};
	my $m = substr( $date, 5, 2 );
	my $d = substr( $date, 8, 2 );
	my $y = substr( $date, 0, 4 );

	$string = "<INPUT TYPE=text NAME=\"$self->{name}_year\" VALUE=\"$y\" SIZE=4, MAXLENGTH=4"; 
	if( $self->{tabindex} ) { $string = $string . " TABINDEX=$self->{tabindex}"; }
	if( $self->{onChange} ) { $string = $string . " onChange=\"$self->{onChange}\""; }
	if( $self->{othertagmods} ) 
	{
		my @arr = @{$self->{othertagmods}}; 
		for( my $x = 0; $x < @arr; $x++ )
		{
			$string = $string . " $arr[$x]"; 
		}
	}
	$string = $string . "></input> - ";

	$string = $string . "<INPUT TYPE=text NAME=\"$self->{name}_month\" VALUE=\"$m\" SIZE=2, MAXLENGTH=2"; 
	if( $self->{tabindex} ) { $string = $string . " TABINDEX=$self->{tabindex}"; }
	if( $self->{othertagmods} ) 
	{
		my @arr = @{$self->{othertagmods}}; 
		for( my $x = 0; $x < @arr; $x++ )
		{
			$string = $string . " $arr[$x]"; 
		}
	}
	$string = $string . "></input> - ";

	$string = $string . "<INPUT TYPE=text NAME=\"$self->{name}_day\" VALUE=\"$d\" SIZE=2, MAXLENGTH=2"; 
	if( $self->{tabindex} ) { $string = $string . " TABINDEX=$self->{tabindex}"; }
	if( $self->{othertagmods} ) 
	{
		my @arr = @{$self->{othertagmods}}; 
		for( my $x = 0; $x < @arr; $x++ )
		{
			$string = $string . " $arr[$x]"; 
		}
	}
	$string = $string . "></input>";
	
}
sub htm_tag_textarea
{
	my $self = shift;
	my $string = "<TEXTAREA NAME=\"$self->{name}\""; 

	if( $self->{tabindex} ) { $string = $string . " TABINDEX=$self->{tabindex}"; }
	if( $self->{onClick} ) { $string = $string . " onClick=\"$self->{onClick}\""; }
	if( $self->{onChange} ) { $string = $string . " onChange=\"$self->{onChange}\""; }
	if( $self->{othertagmods} ) 
	{
		my @arr = @{$self->{othertagmods}}; 
		for( my $x = 0; $x < @arr; $x++ )
		{
			$string = $string . " $arr[$x]"; 
		}
	}

	$string = $string . " >" . $self->{value} . "</TEXTAREA>";

	return $string;
	
}
sub htm_tag_select
{
	my $self = shift;
	my $string = "<SELECT NAME=\"$self->{name}\"";

	if( $self->{size} ) { $string = $string . " SIZE=$self->{size}"; }
	if( $self->{tabindex} ) { $string = $string . " TABINDEX=$self->{tabindex}"; }
	if( $self->{onClick} ) { $string = $string . " onClick=\"$self->{onClick}\""; }
	if( $self->{onChange} ) { $string = $string . " onChange=\"$self->{onChange}\""; }
	$string = $string . " >";
	if( $self->{othertagmods} ) 
	{
		my @arr = @{$self->{othertagmods}}; 
		for( my $x = 0; $x < @arr; $x+=2 )
		{
			if( $arr[$x] eq $self->{value} )
			{
				$string = $string . "<option selected value=$arr[$x]>$arr[$x+1]</option>\n";  
			}
			else
			{
				$string = $string . "<option value=$arr[$x]>$arr[$x+1]</option>\n";  
			}
		}
	}
	else
	{
		$string = $string . "<option>$self->{value}</option>\n";  
	}

	$string = $string . "</select>";	
}

sub read_param
{
	my $self = shift;
	my $meth = "read_param_$self->{type}";

	return $self->$meth();
}
sub read_param_text
{
	my $self = shift;
	
	$self->{value} = param( $self->{name} );
}
sub read_param_checkbox
{
	my $self = shift;
	my $ch = param( $self->{name} );

	if( defined( $ch ) )
	{
		$self->{value} = "Y";
	}
	else
	{
		$self->{value} = "N";
	}
}
sub read_param_date
{
	my $self = shift;
	my $m = param( "$self->{name}_month" );
	for( my $x = length( $m ); $x < 2; $x++ )
	{ $m = "0" . $m; }

	my $d = param( "$self->{name}_day" );
	for( my $x = length( $d ); $x < 2; $x++ )
	{ $d = "0" . $d; }

	my $y = param( "$self->{name}_year" );
	if( length( $y ) == 2 ) { $y = "20" . $y; }
	$self->{value} = "$y-$m-$d";
}

sub read_param_textarea
{
	my $self = shift;
	my $str = param( $self->{name} );
	if( substr( $str, length($str) - 1, 1 ) ne "\n" )
	{ $str = $str . "\n"; }
	$self->{value} = $str; 
}
sub read_param_select
{
	my $self = shift;
	$self->{value} = param( $self->{name} );
}

sub print_val
{
	my $self = shift;
	my $meth = "print_$self->{type}";

	return $self->$meth();
}

sub print_text
{
	my $self = shift;
	my $ret;
	if( defined( $self->{link} ) )
	{
		$ret = "<a href=$self->{link}>$self->{value}</a>";
	}
	else
	{
		$ret = $self->{value};
	}
	return $ret;
}
sub print_checkbox
{
	my $self = shift;
	my $ret = "";

	if( defined( $self->{link} ) )
	{
		$ret = "<a href=$self->{link}>";
	}

	if( $self->{value} eq "Y" )
	{
		$ret = $ret . "<font color=blue>Y</font>";
	}
	else
	{
		$ret = $ret . "<b><font color=red>N</font>";
	}

	if( defined( $self->{link} ) )
	{
		$ret = $ret . "</a>";
	}
	return $ret;
}
sub print_date
{
	my $self = shift;
	my $ret;
	if( defined( $self->{link} ) )
	{
		$ret = "<a href=$self->{link}>$self->{value}</a>";
	}
	else
	{
		$ret = $self->{value};
	}
	return $ret;
}
sub print_textarea
{
	my $self = shift;
	return $self->{value};
}
sub print_select
{
	my $self = shift;
	my $ret;
	if( defined( $self->{link} ) )
	{
		$ret = "<a href=$self->{link}>$self->{value}</a>";
	}
	else
	{
		$ret = $self->{value};
	}
	return $ret; 
}

1;
