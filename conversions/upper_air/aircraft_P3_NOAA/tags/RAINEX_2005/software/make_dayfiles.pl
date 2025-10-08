#! /bin/csh

if (!(-e ../codiac)) then
    mkdir ../codiac
endif

/net/work/dev/make_esc_dayfiles/make_esc_dayfiles ../final/N42RF_*.cls ../codiac/N42RF_
/net/work/dev/make_esc_dayfiles/make_esc_dayfiles ../final/N43RF_*.cls ../codiac/N43RF_
