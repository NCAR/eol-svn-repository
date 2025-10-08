#! /bin/csh

if (!(-e ../codiac)) then
    mkdir ../codiac
endif

/net/work/dev/make_esc_dayfiles/make_esc_dayfiles ../final/N49RF_*.cls ../codiac/N49RF_
