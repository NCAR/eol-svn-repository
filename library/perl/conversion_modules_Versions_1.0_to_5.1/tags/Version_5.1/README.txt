The conversion modules were developed by Joel Clawson as a Perl library
to use during data conversions.  There are three main directories.

modules - The library of modules and scripts that can be used for converting
          data.  It also contains testing scripts and modules for the library.

cgi-bin - A set of Perl CGI scripts that provide a web interface for the
          conversion modules.

html - A set of documents including how tos and information on the modules
       themselves.

The modules were originally deployed in the /work/software/conversion_modules
with a subdirectory for each version.  The cgi-bin files should be deployed in
the /web/cgi-bin/dpg/conversions directory.  The html files should be deployed
in the /work/DPG_HTML/TOOLS/conversions directory.

None of the CGI or HTML files were updated to Version 2.

- Joel Clawson
