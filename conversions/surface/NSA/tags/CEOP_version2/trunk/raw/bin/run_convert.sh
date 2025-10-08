#!/bin/tcsh

./convert_datetime.pl NSA_GNDRAD_flagging.txt gndrad_DQR_flag.file

./convert_datetime.pl NSA_METTWR_flagging.txt mettwr_DQR_flag.file

#./convert_datetime.pl NSA_ORG_flagging.txt org_DQR_flag.file

./convert_datetime.pl NSA_SKYRAD_flagging.txt skyrad_DQR_flag.file
