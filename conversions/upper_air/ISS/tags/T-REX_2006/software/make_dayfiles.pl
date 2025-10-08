#! /bin/csh

# Create the output directory if it doesn't already exist
if (!(-e ../database)) then
    mkdir ../database
endif

# Create the day files using the QC files.
/net/work/dev/make_esc_dayfiles/make_esc_dayfiles ../final/*.qc ../database/ISS_
