#!/usr/bin/perl -w

#---------------------------------------------------------------------
# This stores the data needed for a table within the Ivotuk CD
# It pulls out the different tables - Discipline, Site, etc.
# For use with mk_tables2.pl
#
# Author: Dan Sullivan
# Date:   Summer, 2002
#---------------------------------------------------------------------

package Table;

use strict;
use lib ".";
use Row;
use Util;

sub new
{
	my $classname = shift;
	my $self = {};
	bless( $self, $classname );

	$self->{rows} = [];
	$self->{count} = 0;
	return $self;
}

sub addRow_full
{
	my $self = shift;
	my $row = shift;
	$self->{rows}[$self->{count}] = $row;
	$self->{count}++;
}

sub addRow
{
	my $self = shift;
	my $row = shift;
	my $found = 0;

	for( my $x = 0; $x < $self->{count}; $x++ )
	{
		if( $row->{dataset} eq $self->{rows}[$x]->{dataset} )
		{
			$found = 1;
			$self->{rows}[$x]->addFields( $row );
		}
	}

	if( !$found )
	{
		$self->{rows}[$self->{count}] = $row;
		$self->{count}++;
	}	
}

sub sort
{
	my $self = shift;
	my $fields = shift;
	my $size = $self->{count};

	for( my $x = 0; $x < $size; $x++ )
	{
		$self->{rows}[$x]->sortAllFields();
	}
	
	$self->sortTable( $fields );
}

sub sortTable
{
	my $self = shift;
	my $fields = shift;
	my $size = $self->{count};

	for( my $a = 0; $a < $size - 1; $a++ )
	{
		for( my $b = $a + 1; $b < $size; $b++ )
		{
			if( checkSwap( $self->{rows}[$a], $self->{rows}[$b], $fields ) )
			{
				my $tmp = $self->{rows}[$a];
				$self->{rows}[$a] = $self->{rows}[$b];
				$self->{rows}[$b] = $tmp;
			}
		}
	}
}	

sub checkSwap
{
	my $row1 = shift;
	my $row2 = shift;
	my $fields = shift;
	my @f_arr = @$fields;

	my $field1 = $f_arr[0];
	my $field2 = $f_arr[1];
	my $field3 = $f_arr[2];

	my $arr1 = \@{$row1->{$field1}};
	my $arr2 = \@{$row2->{$field1}};

	if( same( $arr1, $arr2 ) )
	{
		$arr1 = \@{$row1->{$field2}};
		$arr2 = \@{$row2->{$field2}};

		if( same( $arr1, $arr2 ) )
		{
			$arr1 = \@{$row1->{$field3}};
			$arr2 = \@{$row2->{$field3}};

			if( $field3 eq "site" )
			{	
				return checkFieldSwap_site( $arr1, $arr2 );		#field3
			}
			else
			{
				return checkFieldSwap( $arr1, $arr2 );
			}
		}

		if( $field2 eq "site" )
		{	
			return checkFieldSwap_site( $arr1, $arr2 );
		}
		else
		{
			return checkFieldSwap( $arr1, $arr2 );		#field2
		}
	}

	if( $field1 eq "site" )
	{	
		return checkFieldSwap_site( $arr1, $arr2 );
	}
	else
	{
		return checkFieldSwap( $arr1, $arr2 );		#field1
	}
}

sub checkFieldSwap_site 
{
	my ( $a1, $a2 ) = @_;

	my @arr1 = @$a1;	
	my @arr2 = @$a2;	

	my $size1 = @arr1;
	my $size2 = @arr2;
	my $c = 0;
	my $swap = -1;

	while( $c < $size1 && $c < $size2 )
	{
		if( $arr1[$c] gt $arr2[$c] )
		{	$swap = 1; last; }

		if( $arr1[$c] ne $arr2[$c] )
		{	$swap = 0; last; }

		$c++;
	}

	if( $swap == -1 )
	{	
		if( $size1 > $size2 )
		{
			$swap = 1; 
		}
		else
		{
			$swap = 0;
		}
	} 

	if( $size2 == 1 && $size1 != 1 )
	{	
		$swap = 1;
	}	
	return $swap;	
}

sub checkFieldSwap 
{
	my ( $a1, $a2 ) = @_;

	my @arr1 = @$a1;	
	my @arr2 = @$a2;	

	my $size1 = @arr1;
	my $size2 = @arr2;
	my $c = 0;
	my $swap = -1;

	while( $c < $size1 && $c < $size2 )
	{
		if( $arr1[$c] gt $arr2[$c] )
		{	$swap = 1; last; }

		if( $arr1[$c] ne $arr2[$c] )
		{	$swap = 0; last; }

		$c++;
	}

	if( $swap == -1 )
	{	
		if( $size1 > $size2 )
		{
			$swap = 1; 
		}
		else
		{
			$swap = 0;
		}
	} 
	
	return $swap;	
}

sub showTable
{
	my $self = shift;
	*OUT = shift;
	my $size = $self->{count};

	println( *OUT, "\t<tbody>" );
	for( my $x = 0; $x < $size; $x++ )
	{
		$self->{rows}[$x]->showRow( *OUT );
	}
	
}

sub same
{
	my ( $ar1, $ar2 ) = @_;

	my @a1 = @$ar1;
	my @a2 = @$ar2;
	my $same = 1;
	my $s1 = @a1;
	my $s2 = @a2;

	if( $s1 != $s2 )
	{
		$same = 0;
	}	
	else
	{
		for( my $x = 0; $x < $s1; $x++ )
		{
			if( $a1[$x] ne $a2[$x] )
			{
				$same = 0; 
				last;
			}
		}
	}

	return $same;
}

sub getDisciplines
{
	my $self = shift;
	my @rows = @{$self->{rows}};

	my %keys = ("Active Layer/Permafrost", "permafrost",
		    "Flux", "flux",
		    "Hydrology", "hydrology",
		    "Meteorology", "meteorology",
		    "Snow Properties", "snow",
		    "Soil", "soil",
		    "Vegetation", "vegetation" );	
	
	my %disc = ("Hydrology" => Table->new(),
		    "Meteorology" => Table->new(),
		    "Active Layer/Permafrost" => Table->new(),
		    "Snow Properties" => Table->new(),
		    "Soil" => Table->new(),
		    "Vegetation" => Table->new(),
		    "Flux" => Table->new() );	


	foreach my $row (@rows)
	{
		if( $row->{disc}[0] ne "All" )
		{
			my $ndisc = @{$row->{disc}};
			if( $ndisc == 1 )
			{
				print( $row->{disc}[0], "\n" );
				my $tmp = $row->{disc}[0];
				$disc{$tmp}->addRow( $row );	
			}
			else
			{
				my $row2 = Row->new();
				for( my $x = 0; $x < $ndisc; $x++ )
				{
					$row2->addField( "group", $row->{group}[$x] );
					$row2->addField( "members", $row->{members}[$x] );
					$row2->addField( "disc", $row->{disc}[$x] );
					$row2->addField( "sitegroup", $row->{sitegroup}[$x] );
					@{$row2->{date}} = @{$row->{date}};
					@{$row2->{site}} = @{$row->{site}};
					$row2->{dataset} = $row->{dataset};
					$row2->{readme} = $row->{readme};
					$disc{ $row->{disc}[$x] }->addRow( $row2 );
				}
			}
		}
	}

	return %disc;
}

sub getSites
{
	my $self = shift;
	my @rows = @{$self->{rows}};

	# These are used so multiple ids can point to the same table.
	my $spruce = Table->new();
	my $tundra = Table->new();
	my $shrub = Table->new();
	my $woodland = Table->new();
	my $lowshrub = Table->new();
	my $burned = Table->new();

	my %sites = ("Barren Acidic", Table->new(),
		     "Barren Non-Acidic", Table->new(),
		     "Bear Creek", Table->new(),
		     "Blueberry Hill", Table->new(),
		     "Burned Tundra", $burned,
		     "C1", $spruce,
		     "C2", $tundra,
		     "C3", $shrub,
		     "C4", $woodland,
		     "C5", Table->new(),
		     "C6", $lowshrub,
		     "C8", $burned,
		     "Clyde's Gulch", Table->new(),
		     "Council", Table->new(),
		     "Council Grid", Table->new(),
		     "Fox River", Table->new(),
		     "Grasshopper Hill", Table->new(),
			 "Guy Rowe", Table->new(),
		     "Heath", Table->new(),
		     "ISS", Table->new(),
		     "K1", Table->new(),
		     "K2", Table->new(),
		     "K3", Table->new(),
		     "Kougarok", Table->new(),
		     "Low Shrub", $lowshrub,
		     "MAT", Table->new(),
		     "Mauze Gulch", Table->new(),
		     "Melsing Creek", Table->new(),
		     "Multiple", Table->new(),
		     "Mystery Creek",  Table->new(),
		  	 "Niagara Creek", Table->new(),
		     "Ophir", Table->new(),
		     "Otuk Creek", Table->new(),
		     "Seward", Table->new(),
		     "Shallow Lake", Table->new(),
		     "Shrub", $shrub,
		     "Spruce Forest",  $spruce,
			 "Tall Shrub", Table->new(),
		     "Tundra", $tundra,
		     "Woodland", $woodland);
	
	
	foreach my $row (@rows)
	{
		if( $row->{site}[0] ne "" && $row->{site}[0] ne "." )
		{
			my $nsite = @{$row->{site}};
			if( $nsite == 1 )
			{
				print( $row->{site}[0], "\n" );
				my $tmp = $row->{site}[0];
				$sites{$tmp}->addRow( $row );	
			}
			else
			{
				for( my $x = 0; $x < $nsite; $x++ )
				{
					my $row2 = Row->new();
					@{$row2->{group}} = @{$row->{group}};
					@{$row2->{members}} = @{$row->{members}};
					@{$row2->{date}} = @{$row->{date}};
					@{$row2->{disc}} = @{$row->{disc}};
					@{$row2->{sitegroup}} = @{$row->{sitegroup}};
					$row2->addField( "site", $row->{site}[$x] );
					$row2->{dataset} = $row->{dataset};
					$row2->{readme} = $row->{readme};
					$sites{ $row->{site}[$x] }->addRow( $row2 );
				}
			}
		}
	}

	return %sites;
}


sub getSiteGroups
{
	my $self = shift;
	my @rows = @{$self->{rows}};


	my %sitegroups = ("Burn", Table->new(),
			  "Council", Table->new(),
			  "Council North", Table->new(),
			  "Council South", Table->new(),
			  "Kougarok", Table->new(),
			  "S. Quartz Creek", Table->new(),
			  "Seward Peninsula", Table->new());

	foreach my $row (@rows)
	{
		if( $row->{sitegroup}[0] ne "" && $row->{sitegroup}[0] ne "." )
		{
			my $ngroup = @{$row->{sitegroup}};
			if( $ngroup == 1 )
			{
				print( $row->{sitegroup}[0], "\n" );
				my $tmp = $row->{sitegroup}[0];

				$sitegroups{$tmp}->addRow( $row );	
			}
			else
			{
				for( my $x = 0; $x < $ngroup; $x++ )
				{
					my $row2 = Row->new();
					@{$row2->{site}} = @{$row->{site}};
					@{$row2->{date}} = @{$row->{date}};
					$row2->addField( "disc", $row->{disc}[$x] );
					$row2->addField( "group", $row->{group}[$x] );
					$row2->addField( "members", $row->{members}[$x] );
					$row2->addField( "sitegroup", $row->{sitegroup}[$x] );
					$row2->{dataset} = $row->{dataset};
					$row2->{readme} = $row->{readme};
					$sitegroups{ $row->{sitegroup}[$x] }->addRow( $row2 );
				}
			}
		}
	}

	return %sitegroups;
}

sub getGroups
{
	my $self = shift;
	my @rows = @{$self->{rows}};


	my %groups = ("ALP", Table->new(),
		      "ETGF", Table->new(),
		      "HR", Table->new(),
		      "PS", Table->new(),
		      "SSW", Table->new(),
		      "VEG", Table->new() );


	foreach my $row (@rows)
	{
		if( $row->{group}[0] ne "" && $row->{group}[0] ne "." )
		{
			my $ngroup = @{$row->{group}};
			if( $ngroup == 1 )
			{
				print( $row->{group}[0], "\n" );
				my $tmp = $row->{group}[0];

				$groups{$tmp}->addRow( $row );	
			}
			else
			{
				for( my $x = 0; $x < $ngroup; $x++ )
				{
					my $row2 = Row->new();
					@{$row2->{site}} = @{$row->{site}};
					@{$row2->{date}} = @{$row->{date}};
					$row2->addField( "disc", $row->{disc}[$x] );
					$row2->addField( "group", $row->{group}[$x] );
					$row2->addField( "members", $row->{members}[$x] );
					$row2->addField( "sitegroup", $row->{sitegroup}[$x] );
					$row2->{dataset} = $row->{dataset};
					$row2->{readme} = $row->{readme};
					$groups{ $row->{group}[$x] }->addRow( $row2 );
				}
			}
		}
	}

	return %groups;
}

sub getDates
{
	my $self = shift;
	my @rows = @{$self->{rows}};


	my %dates = ("1999", Table->new(),
		     "2000", Table->new(),
		     "2001", Table->new(),
		     "2002", Table->new(),
		     "2003", Table->new(),
		     "multi", Table->new(),);


	foreach my $row (@rows)
	{
	    my $ndate = @{$row->{date}};
	    if( $ndate == 1 )
	    {
		print( $row->{date}[0], "\n" );
		my $tmp = $row->{date}[0];
		if ($tmp =~ /\d+-\d+/) {
		    $dates{multi}->addRow($row);
		} else{ 
		    $dates{$tmp}->addRow( $row );	
		}
	    }
	    else
	    {
		for( my $x = 0; $x < $ndate; $x++ )
		{
		    my $row2 = Row->new();
		    @{$row2->{site}} = @{$row->{site}};
		    @{$row2->{disc}} = @{$row->{disc}};
		    @{$row2->{members}} = @{$row->{members}};
		    @{$row2->{sitegroup}} = @{$row->{sitegroup}};
		    @{$row2->{group}} = @{$row->{group}};
		    $row2->addField( "date", $row->{date}[$x] );
		    $row2->{dataset} = $row->{dataset};
		    $row2->{readme} = $row->{readme};
		    if ($row->{date}[$x] =~ /\d+-\d+/) {
			$dates{multi}->addRow($row2);
		    } else {
			$dates{ $row->{date}[$x] }->addRow( $row2 );
		    }
		}
	    }
	}

	return %dates;
}
1;













