#!/usr/bin/perl
# copies metadata files contained in XML_LOCATION to the
# dataset directories located in STAGING_AREA_LOCATION/[project]/[dataset_id]
# This script may take a little longer to run than you might expect, since
# there are some special cases it must handle, namely metacharacters and
# checking if the dataset has been versioned (must manage this metadata manually)
use strict;

# change these if the XML or staging locations change
my $XML_LOCATION = '/net/work/acadis_xml';
my $STAGING_AREA_LOCATION = '/scr/ctm/cloud/s3_staging';
my @metadata = <$XML_LOCATION/*>;

foreach (@metadata) {
	if (/((AMTS)|(ARC-MIP)|(ARCSS)|(ATLAS)|(BASE)|(BEST)|(BOREAS)|(BSIERP)|(ITEX)|(PacMARS)|(SBI)|(SHEBA))/) {
		my $project_name = $1;
		if ($project_name =~ /(BEST)|(BSIERP)/) {
			$project_name = 'BeringSea/' . $project_name;
		}
		foreach(<$_/*>) {
			if (/((\d{2,3}\.(ARCSS)?(B\d\d-)?\d{3,4}).+\.xml)/) {
				my $staging_dest = "$STAGING_AREA_LOCATION/$project_name/$2";
				if (-d $staging_dest) {
					# check if the dataset is versioned. If it does, ask users to handle manually, as
					# there can be a number of complicated edge-cases.
					my @versions = <$staging_dest/v_*>;
					if (scalar @versions > 0) {
						print "$2 is a versioned dataset. You will need to copy $_ to the latest version directory at $staging_dest and move any existing metadata in the base directory to the first version directory, if this has not been done.\n";
					}
					else {
						my $src_path = $_;
						my $dest_file = $1;

						# This is where we handle the unix metacharacters that may be in the filenames.
						# Currently handled: ', ;, &, and space. If further issues present themselves -
						# i.e. if the program prints "error copying..." messages - it may be an issue
						# with metacharacters. This is where to put further metacharacter handling if
						# necessary

						# escape metacharacters commonly found in the metadata filenames
						$src_path =~ s/(\'|\;|\&| )/\\$1/g;
						# replace these metacharacters in the destination filename so there
						# won't be further issues.
						$dest_file =~ s/\'|\;| +//g;
						$dest_file =~ s/\&amp|\&/and/g;
						`cp -v $src_path $staging_dest/$dest_file\n`;
						if ($?) {
							print "error copying $src_path to $staging_dest/$dest_file\n";
						}
					}
				}
			}	
		}
	}
	
}
exit;

