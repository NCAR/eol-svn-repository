#! /bin/csh

#if (!(-e ../codiac)) then
#    mkdir ../codiac
#endif

#/net/work/dev/make_esc_dayfiles/make_esc_dayfiles ../final/*.cls.qc ../codiac/Trinidad_

java -cp /net/work/bin/dayfiles/upper_air_dayfile.jar:/net/work/lib/java/upper_air.jar:/net/work/lib/java/utilities.jar dmg.ua.sounding.dayfile.ESCDayFileCreator -Z Trinidad_ ../final ../codiac \.cls\.qc
