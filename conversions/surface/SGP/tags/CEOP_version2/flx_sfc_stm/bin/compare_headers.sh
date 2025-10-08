#!/bin/tcsh

cd EBBR/raw
ncdump -h *20021101* > new.header
diff -w *.header > header.diffs
vile header.diffs
cd -

cd IRT/raw
ncdump -h *20021101* > new.header
diff -w *.header > header.diffs
vile header.diffs
cd -

cd SIRS/raw
ncdump -h *20021101* > new.header
diff -w *.header > header.diffs
vile header.diffs
cd -

cd SMOS/raw
ncdump -h *20021101* > new.header
diff -w *.header > header.diffs
vile header.diffs
cd -

cd SWATS/raw
ncdump -h *20021101* > new.header
diff -w *.header > header.diffs
vile header.diffs
cd -

cd TWR10x/raw
ncdump -h *20021101* > new.header
diff -w *.header > header.diffs
vile header.diffs
cd -

