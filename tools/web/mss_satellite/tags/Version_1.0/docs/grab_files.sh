#!/bin/tcsh

# copy the files from the cgi-bin
\rm -r cgi/*
\cp -r /web/cgi-bin/dpg/mass_store/* cgi

# copy the files from my home directory
\rm -r crontab/*
\cp -r /home/suldan/mass_store/* crontab

# copy the needed html files
\rm -r html/*
\cp -r /web/docs/satellite/query/* html
