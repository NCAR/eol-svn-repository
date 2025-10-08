The docperl tool was developed by Joel Clawson.  It is a tool that parses Perl scripts 
or modules and dynamically creates HTML pages.  The pages show a javadoc like presentation
of the information in the file if the comments are formatted in a specific way that the
tool can parse.

The cgi-bin directory contains the script.  The html directory contains the stylesheet
and the how to on how to format the Perl files for the tool to read.

The cgi-bin files should be deployed in the /web/cgi-bin/dpg/doc directory.  The files
in the html directory should be deployed in the /work/DPG_HTML/TOOLS/doc directory.

- Joel Clawson
