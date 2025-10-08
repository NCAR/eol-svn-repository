#!/bin/tcsh

#------------------------------------------------------------------------
# SFC:
#------------------------------------------------------------------------

cp -r /net/ingest/CEOP/v2/ARM/SGP/SMOS/2005*/*.cdf* SMOS/raw
cp -r /net/ingest/CEOP/v2/ARM/SGP/SMOS/2006*/*.cdf* SMOS/raw
cp -r /net/ingest/CEOP/v2/ARM/SGP/SMOS/2007*/*.cdf* SMOS/raw
cp -r /net/ingest/CEOP/v2/ARM/SGP/SMOS/2008*/*.cdf* SMOS/raw
cp -r /net/ingest/CEOP/v2/ARM/SGP/SMOS/2009*/*.cdf* SMOS/raw
cp -r /net/ingest/CEOP/v2/ARM/SGP/SMOS/flagging/SGP_SMOS_flagging_2005_2009.txt SMOS

cp -r /net/ingest/CEOP/v2/ARM/SGP/SIRS/2005*/*.cdf* SIRS/raw
cp -r /net/ingest/CEOP/v2/ARM/SGP/SIRS/2006*/*.cdf* SIRS/raw
cp -r /net/ingest/CEOP/v2/ARM/SGP/SIRS/2007*/*.cdf* SIRS/raw
cp -r /net/ingest/CEOP/v2/ARM/SGP/SIRS/2008*/*.cdf* SIRS/raw
cp -r /net/ingest/CEOP/v2/ARM/SGP/SIRS/2009*/*.cdf* SIRS/raw
cp -r /net/ingest/CEOP/v2/ARM/SGP/SMOS/flagging/SGP_SIRS_flagging_2005_2009.txt SIRS

cp -r /net/ingest/CEOP/v2/ARM/SGP/IRT/2005*/*.cdf* IRT/raw
cp -r /net/ingest/CEOP/v2/ARM/SGP/IRT/2006*/*.cdf* IRT/raw
cp -r /net/ingest/CEOP/v2/ARM/SGP/IRT/2007*/*.cdf* IRT/raw
cp -r /net/ingest/CEOP/v2/ARM/SGP/IRT/2008*/*.cdf* IRT/raw
cp -r /net/ingest/CEOP/v2/ARM/SGP/IRT/2009*/*.cdf* IRT/raw

cp -r /net/ingest/CEOP/v2/ARM/SGP/MET/2005*/*.cdf* MET/raw
cp -r /net/ingest/CEOP/v2/ARM/SGP/MET/2006*/*.cdf* MET/raw
cp -r /net/ingest/CEOP/v2/ARM/SGP/MET/2007*/*.cdf* MET/raw
cp -r /net/ingest/CEOP/v2/ARM/SGP/MET/2008*/*.cdf* MET/raw
cp -r /net/ingest/CEOP/v2/ARM/SGP/MET/2009*/*.cdf* MET/raw

#------------------------------------------------------------------------
# STM:
#------------------------------------------------------------------------

cp -r /net/ingest/CEOP/v2/ARM/SGP/SWATS/2005*/*.cdf* SWATS/raw
cp -r /net/ingest/CEOP/v2/ARM/SGP/SWATS/2006*/*.cdf* SWATS/raw
cp -r /net/ingest/CEOP/v2/ARM/SGP/SWATS/2007*/*.cdf* SWATS/raw
cp -r /net/ingest/CEOP/v2/ARM/SGP/SWATS/2008*/*.cdf* SWATS/raw
cp -r /net/ingest/CEOP/v2/ARM/SGP/SWATS/2009*/*.cdf* SWATS/raw
cp -r /net/ingest/CEOP/v2/ARM/SGP/SWATS/flagging/SGP_SWATS_flagging_2005_2009.txt SWATS

#------------------------------------------------------------------------
# TWR: Done separately by Susan 2010
#------------------------------------------------------------------------

# cp -r /net/ingest/CEOP/v2/ARM/SGP/TWR/*.cdf* TWR10x/raw
# cp -r /net/ingest/CEOP/v2/ARM/SGP/flagging/ARM_SGP_TWR_flagging.txt TWR10x

#------------------------------------------------------------------------
# FLX:
#------------------------------------------------------------------------

cp -r /net/ingest/CEOP/v2/ARM/SGP/EBBR/2005*/*.cdf* EBBR/raw
cp -r /net/ingest/CEOP/v2/ARM/SGP/EBBR/2006*/*.cdf* EBBR/raw
cp -r /net/ingest/CEOP/v2/ARM/SGP/EBBR/2007*/*.cdf* EBBR/raw
cp -r /net/ingest/CEOP/v2/ARM/SGP/EBBR/2008*/*.cdf* EBBR/raw
cp -r /net/ingest/CEOP/v2/ARM/SGP/EBBR/2009*/*.cdf* EBBR/raw
cp -r /net/ingest/CEOP/v2/ARM/SGP/EBBR/flagging/SGP_EBBR_flagging_2005_2009.txt EBBR

cp -r /net/ingest/CEOP/v2/ARM/SGP/ECOR/2005*/*.cdf* ECOR/raw
cp -r /net/ingest/CEOP/v2/ARM/SGP/ECOR/2006*/*.cdf* ECOR/raw
cp -r /net/ingest/CEOP/v2/ARM/SGP/ECOR/2007*/*.cdf* ECOR/raw
cp -r /net/ingest/CEOP/v2/ARM/SGP/ECOR/2008*/*.cdf* ECOR/raw
cp -r /net/ingest/CEOP/v2/ARM/SGP/ECOR/2009*/*.cdf* ECOR/raw
cp -r /net/ingest/CEOP/v2/ARM/SGP/ECOR/flagging/SGP_ECOR_flagging_2005_2009.txt ECOR
