#!/bin/tcsh

# copy the files from the cgi-bin
\rm -r cgi/*
\cp -r /web/cgi-bin/dpg/arctic_ml/* cgi

# copy the needed html files
#\rm -r html/*
#\cp -r /web/docs/dpg/arctic_ml/* html
#\mv html/index.html html/index_.html
