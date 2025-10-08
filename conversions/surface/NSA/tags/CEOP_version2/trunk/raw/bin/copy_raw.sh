cp -r /ingest/CEOP/v2/ARM/NSA/GNDRAD/200?/* ./gndrad
cp /ingest/CEOP/v2/ARM/NSA/GNDRAD/*flagging.txt ./ 
cp -r /ingest/CEOP/v2/ARM/NSA/METTWR/200?/* ./mettwr
cp /ingest/CEOP/v2/ARM/NSA/METTWR/*flagging.txt .
cp -r /ingest/CEOP/v2/ARM/NSA/SKYRAD/200?/* ./skyrad
cp /ingest/CEOP/v2/ARM/NSA/SKYRAD/*flagging.txt .

gunzip -r . 
