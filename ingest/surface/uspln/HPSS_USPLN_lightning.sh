#!/bin/csh
cd /net/work
if -f /tmp/df.out /bin/rm /tmp/df.out
df -h > /tmp/df.out
/h/eol/dmg/HPSS_cronjobs/ingest/surface/uspln/HPSS_USPLN_lightning.pl
