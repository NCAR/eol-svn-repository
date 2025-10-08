#!/usr/bin/perl -w

#---------------------------------------------------------------------
# Some utility functions and hash tables
# For use with mk_tables2.pl
#
# Author: Dan Sullivan
# Date:   Summer, 2002
#---------------------------------------------------------------------

package Util;

use CGI qw(:standard :html3 );
require Exporter;
use vars qw(@ISA @EXPORT $VERSION );


@ISA = qw(Exporter);
@EXPORT = qw( &println %sites %keys_disc %keys_site %keys_group %keys_date %sort_fields %sort_fields_orig %titles %titles_big %keys_sitegroup);


sub println
{
    *OUT = shift;
    my $val = shift;
    #if( !defined( $_[0] ) ) { @_ = qw( A REALLY BIG STRING I CAN HOPEFULLY SEE ); }
    print( OUT $val, "\n" );
}


%keys_disc = (  "Active Layer/Permafrost", "permafrost",
                "Flux", "flux",
                "Hydrology", "hydrology",
                "Meteorology", "meteorology",
                "Snow Properties", "snow",
                "Soil", "soil",
                "Vegetation", "vegetation");   

%keys_site = ("Barren Acidic", "acidic",
	      "Barren Non-Acidic", "nonacidic",
	      "Bear Creek", "bear",
	      "Blueberry Hill", "blueberry",
	      "Burned Tundra", "burned",
	      "C1", "spruce",
	      "C2", "tundra",
	      "C3", "shrub",
	      "C4", "woodland",
	      "C5", "c5",
	      "C6", "lowshrub",
	      "C8", "burned",
	      "Clyde's Gulch", "clyde",
	      "Council", "council",
	      "Council Grid", "councilgrid",
	      "Fox River", "fox",
	      "Grasshopper Hill", "grasshopper",
		  "Guy Rowe", "guyrowe",
	      "Heath", "heath",
	      "ISS", "iss",
	      "K1", "k1",
	      "K2", "k2",
	      "K3", "k3",
	      "Kougarok", "kougarok",
	      "Low Shrub", "lowshrub",
	      "MAT", "mat",
	      "Mauze Gulch", "mauze",
	      "Melsing Creek", "melsing",
	      "Multiple", "multiple",
	      "Mystery Creek", "mystery",
		  "Niagara Creek", "niagara",
	      "Ophir", "ophir",
	      "Otuk Creek", "otuk",
	      "Seward", "seward",
	      "Shallow Lake", "shallow",
	      "Shrub", "shrub",
	      "Spruce Forest", "spruce",
		  "Tall Shrub", "tallshrub",
	      "Tundra", "tundra",
	      "Woodland", "woodland");

%keys_sitegroup = ("Burn", "burn",
		   "Council", "council",
		   "Council North", "cnorth",
		   "Council South", "csouth",
		   "Kougarok", "kougarok",
		   "S. Quartz Creek", "quartz",
		   "Seward Peninsula", "seward");

%keys_group = ( "ALP", "alp",
                "ETGF", "etgf",
                "HR", "hr",
                "PS", "ps",
                "SSW", "ssw",
                "VEG", "veg" );

%keys_date = (  "1999", "1999",
                "2000", "2000",
		        "2001", "2001",
		        "2002", "2002",
		        "2003", "2003",
	    	    "multi", "multi");


@disc = ( "group", "site", "date" );
@site = ( "disc", "date", "group" );
@group = ( "disc", "site", "date" );
@date = ( "date", "disc", "site" );
@sitegroup = ("site", "group", "date");

@disc_orig = ( "disc", "site", "date" );
@site_orig = ( "site", "disc", "date" );
@group_orig = ( "group", "site", "date" );
@date_orig = ( "date", "site", "group" );
@sitegroup_orig = ("sitegroup", "site", "group");

%sort_fields = (    "disc" => \@disc,
                    "site" => \@site,
                    "group" => \@group,
                    "date" => \@date,
				    "sitegroup" => \@sitegroup); 

%sort_fields_orig = (   "disc" => \@disc_orig,
                        "site" => \@site_orig,
                        "group" => \@group_orig,
                        "date" => \@date_orig,
				        "sitegroup" => \@sitegroup_orig); 

%titles_big = ( "disc", "All Disciplines",
                "site", "All Sites",
                "group", "All Groups",
                "date", "All Years",
		        "sitegroup", "All Site Groups");

%titles = ("Active Layer/Permafrost", "Active Layer/Permafrost",
	   "Flux", "Flux",
	   "Hydrology", "Hydrology", 
	   "Meteorology", "Meteorology",
	   "Snow Properties", "Snow Properties",
	   "Soil", "Soil",
	   "Vegetation", "Vegetation", 

	   "Barren Acidic", "Barren Acidic site",
	   "Barren Non-Acidic", "Barren Non-Acidic site",
	   "Bear Creek", "Bear Creek site",
	   "Blueberry Hill", "Blueberry Hill site",
	   "Burned Tundra", "Burned Tundra (C8) site",
	   "C1", "Spruce Forest (C1) site",
	   "C2", "Tundra (C2) site",
	   "C3", "Shrub (C3) site",
	   "C4", "Woodland (C4) site",
	   "C5", "C5 site",
	   "C6", "Low Shrub (C6) site",
	   "C8", "Burned Tundra (C8) site",
	   "Clyde's Gulch", "Clyde's Gulch site",
	   "Council", "Council (general)",
	   "Council Grid", "Council Grid site",
	   "Fox River", "Fox River site",
	   "Grasshopper Hill", "Grasshopper Hill site",
	   "Guy Rowe", "Guy Rowe site",
	   "Heath", "Heath site",
	   "ISS", "ISS site",
	   "K1", "K1 site",
	   "K2", "K2 site",
	   "K3", "K3 site",
	   "Kougarok", "Kougarok (general)",
	   "Low Shrub", "Low Shrub (C6) site",
	   "MAT", "MAT site",
	   "Mauze Gulch", "Mauze Gulch site",
	   "Melsing Creek", "Melsing Creek site",
	   "Multiple", "Multiple sites",
	   "Mystery Creek", "Mystery Creek site",
	   "Niagara Creek", "Niagara Creek site",
	   "Ophir", "Ophir site",
	   "Otuk Creek", "Otuk Creek site",
	   "Seward", "Seward (general)",
	   "Shallow Lake", "Shallow Lake site",
	   "Shrub", "Shrub (C3) site",
	   "Spruce Forest", "Spruce Forest (C1) site",
	   "Tall Shrub", "Tall Shrub site",
	   "Tundra", "Tundra (C2) site",
	   "Woodland", "Woodland (C4) site",

	   "Burn", "Burn site group",
	   "Council North", "Council North site group",
	   "Council South", "Council South site group",
	   "S. Quartz Creek", "S. Quartz Creek site group",
	   "Seward Peninsula", "Seward Peninsula (general) group",
	   
	   "AVG", "Arctic Climate Change: Substrate and Vegetation group",
	   "ALP", "Active Layer and Permafrost Geophysics group",
	   "ETGF", "Energy and Trace Gas Flux group",
	   "HR", "Hydrological Response group",
	   "PS", "Permafrost Soils group",
	   "SSW", "Snow, Shrubs and Weather group",
	   "VEG", "Substrate and Vegetation group",

	   "1999", "1999",
	   "2000", "2000",
	   "2001", "2001",
	   "2002", "2002",
	   "multi", "Multiple Years");

















