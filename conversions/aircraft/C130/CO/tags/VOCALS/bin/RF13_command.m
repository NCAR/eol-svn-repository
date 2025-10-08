%Use built in import command to import gv_081113.CO.prelim.dat
CO_ppbv(96:102) = -32767
CO_ppbv(96:102)
plot(Time,CO_ppbv)
Time(96:102)
fid=fopen('gv_081113.CO.prelim.dat','a')
fprintf(fid,'%s\t%s\n','Time','COMR_AL');
for idx=1:length(CO_ppbv)
    fprintf(fid,'%d\t%f\n',Time(idx),CO_ppbv(idx))
end
fclose(fid)
