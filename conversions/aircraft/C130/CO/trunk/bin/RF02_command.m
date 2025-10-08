%Use built in import command to import gv_081018.CO.prelim.dat
bad=find(Time<47000)
CO_ppbv(1:20)
CO_ppbv(bad)=-32767
plot(Time,CO_ppbv)
CO_ppbv(21:30)=-32767
plot(Time,CO_ppbv)
fid=fopen('gv_081018.CO.prelim.dat','a')
fprintf(fid,'%s\t%s\n','Time','COMR_AL');
for idx=1:length(CO_ppbv)
    fprintf(fid,'%d\t%f\n',Time(idx),CO_ppbv(idx))
end
fclose(fid)
