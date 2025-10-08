#!/bin/csh

# A shell script to cut the name/id 
# from the original and the revised
# HPCN final files. Once cut, the
# rest of the file should be the same
# in both versions.  Run diff on the
# outputs to find out.
#
# 7 Aug 96, ds

echo "Will process the $1.0qc and $1.pqc files..."
cut -c1-30 $1.0qc > $1.0qc.a
cut -c60-256 $1.0qc > $1.0qc.b
paste $1.0qc.a $1.0qc.b > $1.0qc.paste
cut -c1-30 $1.pqc > $1.pqc.a
cut -c60-256 $1.pqc > $1.pqc.b
paste $1.pqc.a $1.pqc.b > $1.pqc.paste
del *.a *.b
