#!/usr/bin/perl -w

# This is a nice example of how the querying of mass store is done.

open( OUT, ">goes_stat.txt" ) || die( "cannot open file" );

$m = `date +%H:%M:%S`;
print( OUT "Start: $m" );
print( $m );

$topdir = "/JOSS/DATA/RAW/SATELLITE/GOES/G10/4KM/2002";

@daily_dir = `msls -1 $topdir`;
#$size = @daily_dir;
#print( OUT "All Files in $topdir...\n" );
print( "All Files in $topdir...\n" );
#print( OUT @daily_dir );
print( @daily_dir );
print( OUT "\n" );
print( "\n" );

#print( "\nSize: $size\n" );

foreach $dir ( @daily_dir )
{
	chop( $dir );
	print( "Directory Path: $topdir/$dir\n" );
	print( OUT "Directory Path: $topdir/$dir\n" );

	$dir2 = $topdir . "/" . $dir;

	#$usage = `msdu -s $dir2`;
	#chop($usage);
	#$usage = trim( $usage );
	#$bytes = substr( $usage, 0, index( $usage, " " ) ) * 512;	
	#print( "Usage: $bytes Bytes\n" );

	@files = `msls -1 $dir2`;
	$num_files = @files;
	print( "Files: $num_files total files in directory.\n" );
	print( OUT "Files: $num_files total files in directory.\n" );
	showFiles( @files ); 	

	print( OUT "\n---------------------------------------\n" );
	print( "\n---------------------------------------\n" );
}

$m = `date +%H:%M:%S`;
print( OUT "End: $m" );

sub showFiles
{
	$track = 0;

	foreach $file (@files )
	{
		chop( $file );
			
		if( $track == 2)
		{
			print( OUT "$file\n" );
			print( "$file\n" );
		}
		else
		{
			print( OUT "$file   " );
			print( "$file   " );
		}
		$track = ( $track == 2 )? 0 : $track + 1;
	}
}

sub trim
{
	local $str = $_[0];
	chop( $str );

	$str =~ s/^\s+//;
	$str =~ s/\s+$//;
	return $str;
}
