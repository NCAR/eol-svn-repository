The travel calendar is a web based tool for storing and displaying travel information
for people in the office.  It was developed by Joel Clawson and was later updated by
Joel and some classmates for a school project.

There are currently 2 versions of the tool.  The first version was originally completely
deployed in the /web/cgi-bin/dpg/travel directory.  Version 2 was deployed in the 
/web/cgi-bin/travel (cgi-bin) and the /web/docs/travel (html) directories.  All further
deployments should be done in the non dpg directories.

NOTE:  The html directory contains a .htaccess file that provides the security to the
site.  It is a . file, so is not visible through the standard ls command.  Use ls -f to
force the view of the . files.

- Joel Clawson
