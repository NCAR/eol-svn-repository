#
# $Id: fix_missing.sh,v 1.1 1992/06/22 17:03:35 wayne Exp $
#
#
# $Log: fix_missing.sh,v $
# Revision 1.1  1992/06/22  17:03:35  wayne
# Initial Version
#
# 13 Jan 94 lec
#   Specified file locations.

sed -f sed_script ../out/hplains.spatial > ../out/hplains.clean
