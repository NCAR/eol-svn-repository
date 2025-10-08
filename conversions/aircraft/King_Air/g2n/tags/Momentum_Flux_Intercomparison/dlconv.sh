#! /bin/csh -f

echo $argv

set file=$argv
set dir='/RAF/1993/226/HRT'

msrcp mss:$dir/$file .

cossplit $file
cosconvert -b f001
cosconvert -b f002
cosconvert -b f003
cat f001 f002 f003 > temp
rm -f f001
rm -f f002
rm -f f003
g2n temp $file.nc
rm -f temp

