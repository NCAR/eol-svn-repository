#! /usr/bin/perl -wT

#-----------------------------------------------------------------------------#
# ml_class_list: This script generates a list of data sets for classifications,
#                categories, or platforms in the Master List or Codiac database
#                on Riesling or Sferic.
#
# Author: Amanda Orin
# Date: October, 2013
#
# Adopted from code taken from the dataset_contacts script.
#-----------------------------------------------------------------------------#
use strict;
use CGI qw(:standard :html3 );
use DBI;
use Switch;
#---------------------Initialize some Variables-------------------------------#

# Connect to the databases
my $zdb = connectToZincDB();
my $mldb = connectToMLDB();

my $id = param( "id" );
my $tableType = param( "type" );	# cat, plat, or class?
my $display = param( "display" );	# json or html list?

switch (lc($tableType)) {
	case ("category") { $tableType = 1; }
	case ("platform") { $tableType = 2; }
	case ("classification") { $tableType = 0; }
	else { $tableType = 0; }
}

switch (lc($display)) {
	case ("list") { $display = 0; }
	case ("json") { $display = 1; }
	else { $display = 0; }
}

if ($id && $id !~ /^[0-9]+$/) { exit; }	# Return now if ID is invalid.


# type_1 = Category
# type_2 = Event
# type_3 = Site
my @types = ("Classification", "Category", "Event", "Site", "Platform");
my @datasets = getDataset2DArray($tableType, $id);


# JSON FORMATTING FOR RESPONSE
#
# var classes = [
#   { "name": "",
#     "type": "",
#     "children": [
#       {
#         "name": "",
#         "type": ""
#       },
#       {
#         "name": "",
#         "type": ""
#       }
#     ]
#   },
#   { "name": "",
#     "type": "",
#     "children": [
#       {
#         "name": "",
#         "type": ""
#       },
#       {
#         "name": "",
#         "type": ""
#       }
#     ]
#   }
# ]
#   
# classes[i].name
# classes[i].children[i]
# classes[i].children[i].name
# classes[i].children[i].type


#-----------------------The Main Program--------------------------
my $indents = 2;
if ($display == 0) {
	print header('text/html');
	
	print "<table id=\"dataset-list\">\n";

	for (my $i = 0; $i < scalar(@datasets); $i++) {
		print printTabs($indents-1) . "<tr class=\"" . $types[$tableType] . "\">\n";
		
		print printTabs($indents) . "<td>" . $datasets[$i][0] . "</td>\n";
		print printTabs($indents) . "<td>" . $datasets[$i][1] . "</td>\n";
		print printTabs($indents) . "<td>" . $datasets[$i][2] . "</td>\n";
		
		print printTabs($indents-1) . "</tr>\n";
	}
	
	print "</table>\n";
} else {
	print header('application/json');

	print "[\n";
	
	for (my $i = 0; $i < scalar(@datasets); $i++) {
		print "\t{\n";
		
		print printTabs($indents) . "\"project\": \"" . $datasets[$i][0] . "\",\n";
		print printTabs($indents) . "\"archiveId\": \"" . $datasets[$i][1] . "\",\n";
		print printTabs($indents) . "\"name\": \"" . $datasets[$i][2] . "\"\n";
		
		print "\t}";
		
		if ($i < scalar(@datasets)-1) {
			print ",";
		}
		
		print "\n";
	}
	
	print "]\n";
}
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# Get a hash of data set results
#-----------------------------------------------------------------------------#
sub getDataset2DArray
{
  my $tableType = shift;
  my $id = shift;
  my @results = ();
  my @ds2dArray = ();
  my @sql = ("select dp.project_id, d.dataset_id, d.name from dataset d left join dataset_project dp on dp.dataset_id=d.dataset_id left join dataset_classification dc on dc.dataset_id=d.dataset_id where dc.class_id = ? order by dp.project_id, d.dataset_id",
  "select p.name, d.archive_ident, d.title from dataset d left join dataset_project dp on d.id=dp.dataset_id left join project p on p.id=dp.project_id left join dataset_category c on d.id=c.dataset_id where c.category_id = ? order by p.name, d.archive_ident",
  "select p.name, d.archive_ident, d.title from dataset d left join dataset_project dp on d.id=dp.dataset_id left join project p on p.id=dp.project_id left join dataset_platform plat on d.id=plat.dataset_id where plat.platform_id = ? order by p.name, d.archive_ident");
  
  my $sth;
  
  if ($tableType > 0) { $sth = $zdb->prepare( $sql[$tableType] ); }
  else { $sth = $mldb->prepare( $sql[$tableType] ); }
  
  $sth->execute( $id );
    
  my $i = 0;
  while( @results = $sth->fetchrow() ) {
  	if (!$results[0]) { $results[0] = "-NULL-"; }
  	
  	for (my $j = 0; $j < scalar(@results); $j++) {
  		push(@{ $ds2dArray[$i] }, $results[$j]);
  	}
  	
  	$i++;
  }
  
  return @ds2dArray;
}

#-----------------------------------------------------------------------------#
# Print tabs as a string
#-----------------------------------------------------------------------------#
sub printTabs
{
	my $count = shift;
	my $tabString = "";
	
	for (my $i = 0; $i < $count; $i++) {
		$tabString .= "\t";
	}
	
	return $tabString;
}




#-----------------------------------------------------------------------------#
# Connect to the Zinc database 
#-----------------------------------------------------------------------------#
sub connectToZincDB
{
  # Uncomment this section if using the test database on Sferic-Dev. #
#  return DBI->connect( "DBI:mysql:database=zith9;host=sferic-dev.eol.ucar.edu",
#                       "zithview", "look-999", { RaiseError=>1} ) ||
#                  die( "Unable to connect to database" );
  # ------------------------------------------------------------- #

  # Uncomment this section if using the live database on Farskol. #
  return DBI->connect( "DBI:mysql:database=zith9;host=farskol.eol.ucar.edu",
                       "zithview", "look-999", { RaiseError=>1} ) ||
                  die( "Unable to connect to database" );
  # ------------------------------------------------------------- #
}

#-----------------------------------------------------------------------------#
# Connect to the ML database 
#-----------------------------------------------------------------------------#
sub connectToMLDB
{
  return DBI->connect( "DBI:mysql:database=dmg_merged_ml;host=riesling.eol.ucar.edu",
           "dts-full", "l\@micbc", { RaiseError=>1} ) || 
         die( "Unable to connect to database" );
}
