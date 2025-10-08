#!/bin/tcsh

#------------------------------------------------------------------------
# SFC:
#------------------------------------------------------------------------

cp -r /ingest/CEOP/EOP4/GAPP/SGP/SMOS/2003*/*.cdf* SMOS/raw
cp -r /ingest/CEOP/EOP4/GAPP/SGP/SMOS/2004*/*.cdf* SMOS/raw
cp -r /ingest/CEOP/EOP4/GAPP/SGP/flagging/ARM_SGP_SMOS_flagging.txt SMOS

cp -r /ingest/CEOP/EOP4/GAPP/SGP/SIRS/2003*/*.cdf* SIRS/raw
cp -r /ingest/CEOP/EOP4/GAPP/SGP/SIRS/2004*/*.cdf* SIRS/raw
cp -r /ingest/CEOP/EOP4/GAPP/SGP/flagging/ARM_SGP_SIRS_flagging.txt SIRS

cp -r /ingest/CEOP/EOP4/GAPP/SGP/IRT/2003*/*.cdf* IRT/raw
cp -r /ingest/CEOP/EOP4/GAPP/SGP/IRT/2004*/*.cdf* IRT/raw

#------------------------------------------------------------------------
# STM:
#------------------------------------------------------------------------

cp -r /ingest/CEOP/EOP4/GAPP/SGP/SWATS/2003*/*.cdf* SWATS/raw
cp -r /ingest/CEOP/EOP4/GAPP/SGP/SWATS/2004*/*.cdf* SWATS/raw
cp -r /ingest/CEOP/EOP4/GAPP/SGP/flagging/ARM_SGP_SWATS_flagging.txt SWATS

#------------------------------------------------------------------------
# TWR:
#------------------------------------------------------------------------

# cp -r /ingest/CEOP/EOP4/GAPP/SGP/TWR/*.cdf* TWR10x/raw
# cp -r /ingest/CEOP/EOP4/GAPP/SGP/flagging/ARM_SGP_TWR_flagging.txt TWR10x

#------------------------------------------------------------------------
# FLX:
#------------------------------------------------------------------------

cp -r /ingest/CEOP/EOP4/GAPP/SGP/EBBR/2003*/*.cdf* EBBR/raw
cp -r /ingest/CEOP/EOP4/GAPP/SGP/EBBR/2004*/*.cdf* EBBR/raw
cp -r /ingest/CEOP/EOP4/GAPP/SGP/flagging/ARM_SGP_EBBR_flagging.txt EBBR

cp -r /ingest/CEOP/EOP4/GAPP/SGP/ECOR/2003*/*.cdf* ECOR/raw
cp -r /ingest/CEOP/EOP4/GAPP/SGP/ECOR/2004*/*.cdf* ECOR/raw
cp -r /ingest/CEOP/EOP4/GAPP/SGP/flagging/ARM_SGP_ECOR_flagging.txt ECOR
