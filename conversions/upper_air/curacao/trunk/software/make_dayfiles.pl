#! /bin/csh

#if (!(-e ../codiac)) then
#    mkdir ../codiac
#endif

#/net/work/dev/make_esc_dayfiles/make_esc_dayfiles ../final/*.cls.qc ../codiac/Curacao_

java -cp /net/work/bin/dayfiles/upper_air_dayfile.jar:/net/work/lib/java/upper_air.jar:/net/work/lib/java/utilities.jar dmg.ua.sounding.dayfile.ESCDayFileCreator -Z Curacao_ ../final ../codiac \.cls\.qc
