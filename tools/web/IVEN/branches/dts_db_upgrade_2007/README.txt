The IVEN tool was created by Dan Sullivan to create a better way of maintaining
processing information for conversions than the static HTML pages.  It has since
been taken over by Joel Clawson.

The software has gone through two known revisions since its creation that were
not documented well before this repository was created.  The first was changing
the username/password combination for the iven database.  This was changed for
better database error tracking and to remove the DBA username/password combination
from the script.

The second change was adding in more statuses.  The No Proc and In Progress statuses
were added to better represent the state of a processed dataset.  With this upgrade,
it also had some of the labels changed to be more clear for their meaning.

The files in the cgi-bin directory should be deployed in the /web/cgi-bin/dpg/iven
directory.  The files in the html directory should be deployed in the 
/work/DPG_HTML/iven directory.  The files in the docs directory are Dan's original
documentation on the tool that he created before he left.  The development was
originally done in the /web/cgi-bin/dpg/dev/iven directory.

- Joel Clawson
