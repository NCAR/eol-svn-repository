#! /usr/bin/perl -wT

#-----------------------------------------------------------------------------#
# dataset_contacts: This script generates JSON output of the contacts and 
#                   contact type list for data sets from Codiac.
#
# Author: Amanda Orin
# Date: June, 2013
#
# Adopted from code taken from the acadis_manifest script and
# PacMARS status script.
#-----------------------------------------------------------------------------#
use strict;
use CGI qw(:standard :html3 );
use DBI;
#---------------------Initialize some Variables-------------------------------#

# Connect to the databases
my $zdb = connectToZincDB();
my $mldb = connectToMLDB();

# All of the query parameters to read in
#my %query = ( "archiveId", []);
#@{$query{archiveId}} = param( "archiveId" );
my @archiveId = param( "id" );
my $authors = param ( "authors" );
my $project = param( "project" );
my $prjFlag = param( "all" );

# JSON FORMATTING FOR RESPONSE
#
# var datasets = [
#   { "archiveId": "", 
#     "title": "",
#     "visible": "",
#     "contacts": [
#       {
#         "name": "",
#         "org": "",
#         "iso_role": ""
#       },
#       {
#         "name": "",
#         "org": "",
#         "iso_role": ""
#       }
#     ]
#   },
#   { "archiveId": "", 
#     "title": "",
#     "contacts": [
#       {
#         "name": "",
#         "org": "",
#         "iso_role": ""
#       },
#       {
#         "name": "",
#         "org": "",
#         "iso_role": ""
#       }
#     ]
#   }
# ]
#   
# datasets[i].archiveId
# datasets[i].title
# datasets[i].visible
# datasets[i].contacts[i]
# datasets[i].contacts[i].name
# datasets[i].contacts[i].org
# datasets[i].contacts[i].iso_role


#-----------------------The Main Program--------------------------
print header('application/json');

# If archiveId was passed in but no actual value was given, reset it.
if (scalar(@archiveId) == 1 && $archiveId[0] eq "") {
	@archiveId = ();
}

if (defined($prjFlag) && $prjFlag eq "true") {

	my $prjQuery = "select name from project order by name asc";
	my @projects = ();
	
	my $pqh = $zdb->prepare( $prjQuery );
	$pqh->execute();
	
	print "\t{\n";
		
	print "\t\t" . "\"projects\": [\n";

	while ( my @row = $pqh->fetchrow() ) {
		push(@projects, $row[0]);
	}
	
	my $i = 0;
	for my $prj (@projects) {
	  print "\t\t\t{\n";

	  print "\t\t\t\t" . "\"name\": \"" . $prj . "\"\n";
	  print "\t\t\t}";

	  if ($i < scalar(@projects)-1) {
		print ",";
	  }
	  print "\n";
	  $i += 1;
	}

	print "\t\t" . "]\n";
	
	print "\t}";
	
	print "\n";

} else {

	if (defined($project) && $project ne "") {
		my $queryDs = "select archive_ident from dataset left join dataset_project on dataset.id=dataset_project.dataset_id left join project on dataset_project.project_id=project.id where project.name = ? order by archive_ident asc";
		my @rowResults = ();
		my @prjArchiveId = ();
		my $sqh = $zdb->prepare( $queryDs );
		$sqh->execute($project);
		
		while( @rowResults = $sqh->fetchrow() ) {
			# Unless the archive ID is already in the list, add it!
			unless ($rowResults[0] ~~ @archiveId) {
				push(@archiveId, $rowResults[0]);
				push(@prjArchiveId, $rowResults[0]);
			}
		}
		
		# If the zith9 project has no data sets, check the master list project for data sets.
		if (scalar(@prjArchiveId) == 0) {
			$queryDs = "select d.dataset_id from dataset d left join dataset_project dp on d.dataset_id=dp.dataset_id where dp.project_id = ? and d.dataset_id not like 'ML.%' order by d.dataset_id";
			@rowResults = ();
			
			$sqh = $mldb->prepare( $queryDs );
			$sqh->execute($project);
		
			while( @rowResults = $sqh->fetchrow() ) {
				# Unless the archive ID is already in the list, add it!
				unless ($rowResults[0] ~~ @archiveId) {
					push(@archiveId, $rowResults[0]);
				}
			}
		}
	}

	if (scalar(@archiveId) > 0) {
	  print "[\n";

	  for (my $i = 0; $i < scalar(@archiveId); $i++) {

                my $queryDs = "select title, visible from dataset where archive_ident = '${archiveId[$i]}'";
                my $sqh = $zdb->prepare($queryDs);
                $sqh->execute();
                my @rowResults = $sqh->fetchrow();
                my $dtitle = $rowResults[0];
                my $dvisible = $rowResults[1];

		my @cntRow = getZincContact($archiveId[$i]); # This returns a 2-dimensional array!
		
		print "\t{\n";
		
		print "\t\t" . "\"archiveId\": \"" . $archiveId[$i] . "\",\n";
		print "\t\t" . "\"title\": \"" . $dtitle . "\",\n";
		print "\t\t" . "\"visible\": \"" . $dvisible . "\",\n";
		print "\t\t" . "\"authors\": \"" . getMlAuthors($archiveId[$i]) . "\",\n";
		print "\t\t" . "\"contacts\": [\n";
		
		for (my $j = 0; $j < scalar(@cntRow); $j++) {
		  print "\t\t\t{\n";

		  # If any names or organizations have the apostrophe in them, replace it with an HTML single quote.
		  if ($cntRow[$j][0] =~ m/\\[']/) {
			$cntRow[$j][0] =~ s/\\[']/&rsquo;/g;
		  }
		  if ($cntRow[$j][1] =~ m/\\[']/) {
			$cntRow[$j][1] =~ s/\\[']/&rsquo;/g;
		  }
		  
		  print "\t\t\t\t" . "\"name\": \"" . $cntRow[$j][0] . "\",\n";
		  print "\t\t\t\t" . "\"org\": \"" . $cntRow[$j][1] . "\",\n";
		  print "\t\t\t\t" . "\"iso_role\": \"" . $cntRow[$j][2] . "\"\n";

		  print "\t\t\t}";

		  if ($j < scalar(@cntRow)-1) {
			print ",";
		  }
		  print "\n";
		}

		print "\t\t" . "]\n";
		
		print "\t}";
		
		if ($i < scalar(@archiveId)-1) {
		  print ",";
		}
		
		print "\n";
	  }
	  
	  print "]\n";
	} else {
	  print "[ { \"archiveId\": \"\", \"title\": \:\", \"contacts\": [{\"name\": \"\", \"org\": \"\", \"iso_role\": \"\"}] } ]\n";
	}

}
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# Get dataset contacts from Zinc's database
#-----------------------------------------------------------------------------#
sub getZincContact
{
  my $id = shift;
  my @results = ();
  my @contacts = ();
  my $c = 0;
  my @sql = (
            "select c.person_name, c.organization_name, \"pointOfContact\" from dataset d left join contact c on c.id=d.point_of_contact_id where d.archive_ident = '${id}'", 
            "select c.person_name, c.organization_name, \"grantContact\" from dataset d left join contact c on c.id=d.grant_contact_id where d.archive_ident = '${id}'", 
            "select c.person_name, c.organization_name, \"EOL_internal\" from dataset d left join contact c on c.id=d.internal_contact_id where d.archive_ident = '${id}'",
            "select c.person_name, c.organization_name, dc.iso_citation_role from dataset d left join dataset_contact dc on d.id=dc.dataset_id left join contact c on c.id=dc.contact_id where d.archive_ident = '${id}' order by dc.sort_key, dc.row_create_time"
  );
  
  if ($authors eq "true") {
	@sql = (
				"select c.person_name, c.organization_name, dc.iso_citation_role from dataset d left join dataset_contact dc on d.id=dc.dataset_id left join contact c on c.id=dc.contact_id where d.archive_ident = '${id}' and dc.iso_citation_role = 'author' order by dc.sort_key, dc.row_create_time"
	  );
  }
  
  foreach my $s (@sql) {

    my $sth = $zdb->prepare( $s );
    $sth->execute();
      
    while( @results = $sth->fetchrow() ) {
      
      if (scalar(@results) == 0) {
        push(@results, "");
        push(@results, "N/A");
        push(@results, "N/A");
        push(@results, "N/A");
      }
        
      foreach my $i (@results) {
        if (!defined($i)) {
          $i = "N/A";
        }
        $i =~ s/"/\\"/g;
        $i =~ s/'/\\'/g;
        
        # Add these results to the contacts array.
        push(@{ $contacts[$c] }, $i);
      }

      # Increment the contact array index for the next row retrieval.
      $c += 1;

    }

  }
  
  return @contacts;
}

#-----------------------------------------------------------------------------#
# Get dataset ML authors 
#-----------------------------------------------------------------------------#
sub getMlAuthors
{
  my $id = shift;
  my @results = ();
  my $sql = "select author_pi from dataset where dataset_id = '${id}'";
  my $str = "";
  
  my $sth = $mldb->prepare( $sql );
    $sth->execute();
    
    @results = $sth->fetchrow();
    
    if (scalar(@results) > 0) {
      $str = ($results[0]);
      
      $str =~ s/"/&quot;/g;
      $str =~ s/'/&rsquo;/g;
    }
  
  return $str;
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

  # Uncomment this section if using the live database on Sferic. #
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
  return DBI->connect( "DBI:mysql:database=dmg_merged_ml;host=farskol.eol.ucar.edu",
           "mlview", "st00p1d", { RaiseError=>1} ) || 
         die( "Unable to connect to database" );
}

