Taylor Thomas created the gcmd*.txt file. This is true in all
of the Taylor directories.

See where I tried testing the software on surface data at
/net/work/tmp/cully/SVN_work/GCMD_keywords/trunk/NSF_NCAR_C130/LowRate/input_test_sfc .

Beware that the SQL commands generated may have the archIdent listed more than
once in the command but that is not an issue and the command will execute. But,
many times an additional comma is also added to the end of the SQL command line
and that extra comma does cause a problem and must be removed before executing
the SQL command.  The code was not updated to fix the extra comma problem since
cutting off the extra comma by hand was easy. The code could be updated to handle
this.  Try sed 's/\,/\n/g' input.txt > output.txt

LEC
