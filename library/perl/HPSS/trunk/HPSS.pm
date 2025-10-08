##Module----------------------------------------------------------------------
# <p>The HPSS.pm module is a class that controls the interaction
# between a Perl script and the HPSS.</p>
#
# @author Sean Stroble
##Module----------------------------------------------------------------------
use Cwd 'abs_path';
package HPSS;
use strict;

my $hsi = "/opt/local/bin/hsi";

##-----------------------------------------------------------------------------
# @signature get(ref hpss_files, ref dest, string flags)
# <p>Download the specified files from the HPSS to dest</p>
#
# @input hpss_files: A file or files to be downloaded from the HPSS (may be scalar or array ref)
# @input dest: local destination filenames or directory (may be array or scalar ref)
# 	If dest is an array ref is MUST have the same number of entries as src_files
# 	If dest is a scalar ref it is assumed to be a target directory
# @input flags: Flags to pass to hsi get. (-R = recursive)
##-----------------------------------------------------------------------------
sub get {
    
    my $msg = "";

    my ($src_ref, $dest_ref, $flags) = @_;
    unless (defined $flags) { $flags = ""; }

    if (ref $src_ref eq 'ARRAY') {
	my @src_files = @$src_ref;

	for (my $i = 0; $i <= $#src_files; $i++) {
	    my $file = $src_files[$i];
	    
	    #determine desination
	    my $dest = "";
	    if (ref $dest_ref eq "ARRAY") {
		if ($#$dest_ref == $#src_files) { $dest = $dest_ref->[$i]; }
		else { $msg .= "hpss_put error: Invalid dest array length!\n"; return $msg}
	    }
	    elsif (ref $dest_ref eq "SCALAR") { $dest = Cwd::abs_path($$dest_ref) . "/" . (split(/\//,$file))[-1] ; }

	    #Check to see if the dest file already exists
	    if (-e $dest) {
		$msg .= "Local files already exists: $dest\n";
		next;
	    }

	    #upload
	    #print "Downloading: $dest <- $file\n";
	    #print "hsi get \"$dest\" : \"$file\"\n";
	    my $output = `$hsi $flags get "$dest" : "$file" 2>&1`;
	    my $err = $?>>8;
	    if ($err == 1) {
	        print  "\nYou must have a valid Kerberos ticket to use hsi commands.\nUse 'kinit USERNAME' to generate a Kerberos ticket.\n"; 
	        exit($err);
	    }elsif ($err != 0) {
	        $msg .= "Could not get $file, Error code: " . $err . ".\n$output\n";
	    }
	}
    }
    elsif (ref $src_ref eq "SCALAR" && ref $dest_ref eq "SCALAR") {
	my $file = $$src_ref;
	my $dest = $$dest_ref;

	#Check to see if the dest file already exists
        if (-e $dest) {
	    $msg .= "Local files already exists: $dest\n";
	    return $msg;
        }

	#upload
	#print "Downloading: $dest <- $file\n";
	my $output = `$hsi $flags get "$dest" : "$file" 2>&1`;
	my $err = $?>>8;
	if ($err == 1) {
	    print "\nYou must have a valid Kerberos ticket to use hsi commands.\nUse 'kinit USERNAME' to generate a Kerberos ticket.\n"; 
	    exit($err);
	}elsif ($err != 0) {
	    $msg .= "Could not get $file, Error code: " . $err . ".\n$output\n";
	}
	
    }
    else { $msg .= "hpss_put error: Invalid arguments!\n"; }
    
    return $msg;
}



##-----------------------------------------------------------------------------
# @signature put(ref local_files, ref dest, string flags)
# <p>Copy the specified files to dest on the HPSS</p>
#
# @input local_files: A file or files to be uploaded to the HPSS (may be scalar or array ref)
# @input dest: HPSS filenames or HPSS directory (may be array or scalar ref)
# 	If dest is an array ref is MUST have the same number of entries as the src_files array
# 	If dest is a scalar ref it is assumed to be a target directory
# @input flags: flags to give to hsi put command (-d = Remove local files, -R = recursive, -P = create path if needed)
##-----------------------------------------------------------------------------
sub put {

    my $msg = "";

    my ($src_ref, $dest_ref ,$flags) = @_;
    unless (defined $flags) { $flags = "-P"; }

    $flags =~ s/-d//g;

    if (ref $src_ref eq 'ARRAY') {
	my @src_files = @$src_ref;


	for (my $i = 0; $i <= $#src_files; $i++) {
	    my $file = $src_files[$i];

	    #hsi will expand relative paths to be represent the HPSS side..
	    #use Cwd::abs_path to expand relative paths before hsi gets to see them
	    if (-e $file) {$file = Cwd::abs_path($file); }
	    else { $msg .= "Local file: $file DOES NOT EXIST\n"; }
	    
	    #determine desination
	    my $dest = "";
	    if (ref $dest_ref eq "ARRAY") {
		if ($#$dest_ref == $#src_files) { $dest = $dest_ref->[$i]; }
		else { $msg .= "hpss_put error: Invalid dest array length!\n"; return $msg}
	    }
	    elsif (ref $dest_ref eq "SCALAR") { $dest = $$dest_ref . "/" . (split(/\//,$file))[-1]; }

	    #Check to see if file already exists
	    if (HPSS::exists($dest)) {
		#Do not overwrite existing files
		$msg .= "HPSS file already exists: $dest\n";
		next;
	    }

	    #upload
	    #print "Uploading: $file -> $dest\n";
	    my $output = `$hsi put $flags "$file" : "$dest" 2>&1`;
	    my $err = $?>>8;
	    if ($err == 1) {
		print "\nYou must have a valid Kerberos ticket to use hsi commands.\nUse 'kinit USERNAME' to generate a Kerberos ticket.\n"; 
		exit($err);
	    }elsif ($err != 0) {
		$msg .= "Could not put $file, Error code: " . $err . ".\n$output\n";
	    }
	}
    }
    elsif (ref $src_ref eq "SCALAR" && ref $dest_ref eq "SCALAR") {
	my $file = $$src_ref;
	my $dest = $$dest_ref;

	#hsi will expand relative paths to be represent the HPSS side..
	#use Cwd::abs_path to expand relative paths before hsi gets to see them
	if (-e $file) {$file = Cwd::abs_path($file); }
	else { $msg .= "Local file: $file DOES NOT EXIST\n"; }

	#Check to see if file already exists
	if (HPSS::exists($dest)) {
	    #Do not overwrite existing files
	    $msg .= "HPSS file already exists: $dest\n";
	    return $msg;
	}

	#upload
	#print "Uploading: $file -> $dest\n";
	my $output = `$hsi put $flags "$file" : "$dest" 2>&1`;
	my $err = $?>>8;
        if ($err == 1) {
 	    print "\nYou must have a valid Kerberos ticket to use hsi commands.\nUse 'kinit USERNAME' to generate a Kerberos ticket.\n"; 
	    exit($err);
        }elsif ($err != 0) {
	    $msg .= "Could not put $file, Error code: " . $err . ".\n$output\n";
        }
	
    }
    else { $msg .= "hpss_put error: Invalid arguments!\n"; }
    
    #print "$msg\n";
    return $msg;
}

##-----------------------------------------------------------------------------
# @signature @files = ls(string path, string flags)
# <p>retrieves a list of files on the HPSS</p>
#
# @input path: Full HPSS path
# @output fiels: Array of files at the specified path
##-----------------------------------------------------------------------------
sub ls {
    my ($path, $flags) = @_;
    unless (defined $flags) { $flags = ""; }
    unless (defined $path) { print "HPSS:ls called with no arguments!\n"; exit(2); }

    my @output =  `$hsi ls $flags $path 2>&1`;
    chomp(@output);
    my $err = $?>>8;
    if ($err == 1) {
        print "\nYou must have a valid Kerberos ticket to use hsi commands.\nUse 'kinit USERNAME' to generate a Kerberos ticket.\n";
	exit($err);
    }
    else { return @output; }
}


##-----------------------------------------------------------------------------
# @signature $result = exists(string path)
# <p>Checks to see if the specified file exists on the HPSS</p>
#
# @input path: The full HPSS path to the file or directory
# @output result: 0 = No, 1 = Yes
##-----------------------------------------------------------------------------
sub exists {
    my ($path) = @_;
    my $output = `$hsi ls $path 2>&1`;
    my $err = $?>>8;

    if ($err == 0) { return 1; }
    elsif ($err == 1) {
        print "\nYou must have a valid Kerberos ticket to use hsi commands.\nUse 'kinit USERNAME' to generate a Kerberos ticket.\n";
	exit($err);
    }
    else { return 0; }
}

1;
