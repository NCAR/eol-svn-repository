Notes for HPCNfiles.pl

What needs to be finished:
- add to cron tab

HPCNstations.txt is needed to run the cron job so that the script can use it to check for missing files and update the stations that are sending us data.	
In addition, HPCNstations.txt contains the city and state for all stations and
this info is used for the HTML inventory pages so that station ID, city, and
state are all displayed even if a station has no readings for a month.

The cron job can currently be run by inputting this on the command line:
	./HPCNfiles.pl
This option will check the date automatically and run the script for the
previous three months. In order to debug, the script can also be run as:
	./HPCNfiles.pl MM YYYY

Where MM is the two-digit month, and YYYY is the four-digit year. 

This script is intended to be run on sferic as joss. It has been successfully tested on
tsunami as joss.

The HTML inventory webpages are stored in the following directory:
/net/web/dmg/html/projects/operational/surface/hpcn/docs/inventory

