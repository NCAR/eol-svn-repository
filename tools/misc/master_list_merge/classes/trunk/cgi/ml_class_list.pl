#! /usr/bin/perl -wT

#-----------------------------------------------------------------------------#
# ml_class_list: This script generates JSON output of the classifications in 
#                the Master List database on Riesling.
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

my $tableType = param( "type" );

switch ($tableType) {
	case ("category") { $tableType = 1; }
	case ("platform") { $tableType = 2; }
	case ("classification") { $tableType = 0; }
	else { $tableType = 0; }
}


# type_1 = Category
# type_2 = Event
# type_3 = Site
my @types = ("null", "category", "event", "site", "platform");
my %classes = getClasses($tableType);
my @roots = getRoots($tableType);


# HTML FORMATTING FOR RESPONSE
#
# <ul>
#	<li class="$types[$i]"></li>
# 	<li class="$types[$i]">
#		<ul>
#			<li class="$types[$i]"></li>
#		</ul>
#	</li>
# </ul>


#-----------------------The Main Program--------------------------
print header('text/html');

print "<ul id=\"accordion\">\n";

for (my $i = 0; $i < scalar(@roots); $i++) {
	printClass($tableType, \%classes, \@types, $roots[$i], 2);
}

print "</ul>\n";
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# Get child classifications
#-----------------------------------------------------------------------------#
sub getChildClasses
{
  my $tableType = shift;
  my $id = shift;
  my @results = ();
  my @kids = ();
  my @sql = ("select class_id from classification_parent where parent_class_id = ? order by class_id",
  "select id from category where parent_category_id = ? order by id",
  "select ? from platform where id < 0 order by id"); # Change when schema supports nested platforms
  
  my $sth;
  
  if ($tableType > 0) { $sth = $zdb->prepare( $sql[$tableType] ); }
  else { $sth = $mldb->prepare( $sql[$tableType] ); }
  
  $sth->execute( $id );
    
  while( @results = $sth->fetchrow() ) {
  	push(@kids, $results[0]);
  }
  
  return @kids;
}

#-----------------------------------------------------------------------------#
# Get all ML classifications 
#-----------------------------------------------------------------------------#
sub getClasses
{
  my $tableType = shift;
  my @results = ();
  my %class = ();
  my @sql = ("select class_id, name, type_id from classification",
  "select id, name, 1 from category", 
  "select id, name, 4 from platform");
  # type_id can only be 1, 2, or 3 (or 4 for platform)
  
  my $sth;
  
  if ($tableType > 0) { $sth = $zdb->prepare( $sql[$tableType] ); }
  else { $sth = $mldb->prepare( $sql[$tableType] ); }
  
  $sth->execute();
    
  while( @results = $sth->fetchrow() ) {
  	$class{ $results[0] } = $results[2]."_".$results[1];
  }
  
  return %class;
}

#-----------------------------------------------------------------------------#
# Get root classifications (be they parent or not)
#-----------------------------------------------------------------------------#
sub getRoots
{
  my $tableType = shift;
  my @results = ();
  my @roots = ();
  my @sql = ("select r.class_id as roots from classification r where r.class_id not in (select cc.class_id from classification_parent cc group by cc.class_id) order by r.type_id, r.class_id",
  "select r.id as roots from category r where r.parent_category_id is null order by r.id",
  "select r.id as roots from platform r order by r.id"); # Change when schema supports nested platforms
  
  my $sth;
  
  if ($tableType > 0) { $sth = $zdb->prepare( $sql[$tableType] ); }
  else { $sth = $mldb->prepare( $sql[$tableType] ); }
  
  $sth->execute();
    
  while( @results = $sth->fetchrow() ) {
  	push(@roots, $results[0]);
  }
  
  return @roots;
}

#-----------------------------------------------------------------------------#
# Print classification (recursive)
#  printClass($tableType, \%classes, \@types, $i, $indents);
#-----------------------------------------------------------------------------#
sub printClass
{
	my $tableType = shift;
	my $classesRef = shift;
	my %classes = %$classesRef;
	my $typeRef = shift;
	my @types = @$typeRef;
	my @ttypes = ("Classification", "Category", "Platform");
	my $id = shift;
	my $class = $classes{$id};
	my $indents = shift;
	my @children = ();
	
	@children = getChildClasses($tableType, $id);
	
	print printTabs($indents-1) . "<li class=\"" . $types[substr($class, 0, 1)];
	
	if (scalar(@children) > 0) {
		print " parent";
	}
	
	print "\">\n";
	
	print printTabs($indents) . substr($class, 2);
	print "\n" . printTabs($indents) . "<span class=\"right-span\" onClick=\"showDsList(this, '".$ttypes[$tableType]."', '".stripQuotes(substr($class, 2))."', ".$id.");\"><img alt=\"View Data Set List\" title=\"View Data Set List\" src=\"list.png\" /></span>";
	
	if (scalar(@children) > 0) {
		print "\n" . printTabs($indents) . "<br />\n";
		
		print printTabs($indents) . "<ul>\n";
		
		for (my $i = 0; $i < scalar(@children); $i++) {
			printClass($tableType, $classesRef, $typeRef, $children[$i], $indents+2);
		}
		
		print printTabs($indents) . "</ul>\n";
	} else {
		print "\n";
	}
	
	print printTabs($indents-1) . "</li>\n";
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
# Strips string of quotes
#-----------------------------------------------------------------------------#
sub stripQuotes
{
	my $strippedString = shift;
	
	$strippedString =~ s/"/&#34;/g;
	$strippedString =~ s/'/&#39;/g;
	
	return $strippedString;
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
