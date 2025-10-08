QCODIAC is a web tool developed by Dan Sullivan and Phil Dressen to view information
in the CODIAC database through a web browser.  It has since been taken over by Joel
Clawson and Don Stott. J. Scannell now maintains this software.

To deploy QCODIAC, you can use Ant to manage deployment. The 'deploy-test' target
will deploy QCODIAC to the test location, while the 'deploy' target will deploy
QCODIAC to the production location. The directories for the test and production
areas are:

Test:
cgi-bin: /net/web/dmg/cgi-bin/qcodiac-test
html: /net/web/dmg/html/software/tools/web/qcodiac-test

Production:
cgi-bin: /net/web/dmg/cgi-bin/qcodiac
html: /net/web/dmg/html/software/tools/web/qcodiac

There are a number of places in the code that specify qcodiac that need to be changed
to qcodiac-test for the test area.  These are:

html/index.html (2 places)
cgi-bin/query (1 place)
cgi-bin/lib/Order_Statistics.pm (3 places)
cgi-bin/lib/Utils.pm (12 places)

There is a required perl module that needs to be added when switching to a new server:
Date/Calc.pm

