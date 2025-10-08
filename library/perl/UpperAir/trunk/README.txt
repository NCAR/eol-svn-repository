Once you have made the change in the library code, it must be tested.  For
ClassHeader.pm changes, run TestClassHeader.pl (at the command line type
"TestClassHeader.pl").  For ClassRecord.pl changes, run TestClassRecord.pl.
Running these tests will create an output file that is diffed with
TEST_CLASS_SOUNDING.txt, and if there are no diffs, the output file is removed
(so you will probably not even see this file if everything is OK, and there
will be no output to screen since there are no diffs).  If there are no diffs
and everything looks good, go up a level and run deploy.pl.  This will deploy
the changed code to the proper directory.  Then you should commit this new
version to subversion.

- Linda Echo-Hawk - September 2012





Previous to Version 6.0, the conversion module libraries and web interface were a part of a singular package called the conversion_modules.  They are located in the subversion repository at dmg/library/perl/conversion_modules.  Version 6.0 split the libraries and interface into seperate packages to allow individual upgrades to packages without being forced to upgrade the entire package.  This coincides with a change in the library deployment going to a single best version deployed and not a series of different version directories lying around.  This will require upgrades to any of these packages to be backward compatable.

- Joel Clawson - Sept 2007
