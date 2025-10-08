%Load CO files and add status variable
tic
clear

slash = '/' % unix, mac convention
% slash = '\' % PC convention
BDF=-999;
hkHdrLns=160;
alHdrLns=69;

%Choose the flight directory
%StartPath='c:\Chemdata\2004OceanWaves\Hal\';
%  StartPath='c:\Chemdata\2004ACME\Hal\';
%  StartPath='d:\Chemdata\2004RICO\Hal\';
%  StartPath='e:\ChemData\2004ACME\Hal\';
% StartPath='/Users/campos/chemData/2006trex/Hal/';
% StartPath='/Users/campos/chemData/2006impex/hal/';
% StartPath='/Users/campos/chemData/2005ProgSci/Hal/';

% Teresa's Mac directory structure:
% StartPath='/Users/campos/Documents/macbak/campos/chemData/2008vocals/co/';
StartPath='/scr/raf/campos/2008vocals/co/';
% eol-wwhd directory structure:
% StartPath='c:\Data\pase2k7\co\';
cd(StartPath);
DirectoryInfo = dir(StartPath);
Directories={DirectoryInfo.name};
[s,v]=listdlg('SelectionMode','single','ListString',Directories);
Flight = Directories(s)
% cd([StartPath, char(Flight)]);

proj='VOCALS'; 
fltno=Flight;
switch char(Flight{1}(1:6))
    case '081124'
        fltno = 'gn01';
    case '081001'
        fltno = 'tf02';
    case '081003'
        fltno = 'tf03';
    case '081018'
        fltno = 'rf02';
    case '081021'
        fltno = 'rf03';
    case '081023'
        fltno = 'rf04';
    case '081025'
        fltno = 'rf05';
    case '081028'
        fltno = 'rf06';
    case '081031'
        fltno = 'rf07';
    case '081102'
        fltno = 'rf08';
    case '081104'
        fltno = 'rf09';
    case '081106'
        fltno = 'rf10';
    case '081109'
        fltno = 'rf11';
    case '081111'
        fltno = 'rf12';
    case '081113'
        fltno = 'rf13';
    case '081115'
        fltno = 'rf14';
end

if( exist([char(Flight), slash, 'pix'])==0)
    mkdir([char(Flight), slash, 'pix'])
end
hkFile = [StartPath,char(Flight),slash, 'A', char(Flight),'0.DAT']
% rafFile = ['../rafDat/503',char(Flight),'.nc']
% rafFile = ['../ifpRAF/502',char(Flight),'.nc']
% rafFile = ['../ifpRAF/506',char(fltno),'.nc'] % PACDEX 2007 proj

if fltno(1)=='g'
    FlightData=0;
else
    FlightData=1;
end
% Teresa's Mac directory structure:
rafFile = ['/scr/raf/Prod_Data/VOCALS/',char(proj),char(fltno),'.nc'] % VOCALS 2007 proj
% eol-wwhd directory structure:
% rafFile = ['..\ifpRAF\',char(proj),char(fltno),'.nc'] % PASE 2007 proj


    corrSlope=1;
    corrInt=0;

%Set calibration and zero limits
[CalLimit,CalMax,ZeroLimit,before,after,DataAfterCal,ptsToAvg,tankCon,toffset,badCals,badZs]=CO_vocalsFlightParams(rafFile,fltno);
ZeroMin=1000;


% rafStartT=50616;
% rafEndT=78923;
%Choose the files to process from the flight directory
FileInfo = dir([StartPath,char(Flight)]);
Files_unsorted={FileInfo.name};
Files=sort(Files_unsorted);  %sorts the filenames to avoid errors
[s,v]=listdlg('ListString',Files(3:length(Files)));
s2=s+2;
FileList=char(Files(s2))

% Incorporate HK data into low res variables:
fid3=fopen(hkFile,'r');
for ix=1:hkHdrLns
    hkHdr=fgetl(fid3);
end
% while ~feof(fid3)
    %       hkDat=fscanf(fid3,'%2d.%2d.%2d %2d.%2d.%2d %x %d %f %f %f %f %f %f %f %f %x %d %d\n',[19,inf]);
    hkDat=fscanf(fid3,'%2d.%2d.%2d %2d:%2d:%2d %x %c %f %f %f %f %f %f %f %f %x %c %c %c %c %c %c %c %c %c %c\n',[27,inf]);
% end

fclose(fid3);

allHKdat = [];
goodHKrows=find(hkDat(9,:)>-90);
goodHKdat=hkDat(:,goodHKrows);

allHKdat=goodHKdat';

hkHr=allHKdat(:,4);
hkMinute=allHKdat(:,5);
hkSecond=allHKdat(:,6);
firstHKsec = hkSecond(1)+60.*hkMinute(1)+3600.*hkHr(1);
hkTime=hkSecond+hkMinute*60+hkHr*3600;
tempIx = find(hkTime < firstHKsec);
if tempIx
firstNewDayPt=tempIx(1);
    HKpastMidniteIx=[firstNewDayPt:length(hkTime)];
hkTime(HKpastMidniteIx) = hkTime(HKpastMidniteIx)+86400;
end

% tailIx=[tempIx(end)+1:length(hkTime)];
% HKpastMidniteIx=[tempIx; tailIx(end)'];
hkStatus1=allHKdat(:,7);
hkStatus2=allHKdat(:,17);
hkZue=allHKdat(:,8);
lampFlow=allHKdat(:,9);
monoFlow=allHKdat(:,10);
monoP=allHKdat(:,11);
calP=allHKdat(:,12);
cellP=allHKdat(:,13);
lampT=allHKdat(:,14);
monoT=allHKdat(:,15);
pmtT=allHKdat(:,16);
calVlv=allHKdat(:,18);
zVlv=allHKdat(:,19);

figure(1);
subplot(5,1,1);plot(hkTime,lampFlow);ylabel('Lamp Flow');title(['VOCALS  ' char(fltno)  '  ' char(Flight)],'FontSize',14);
subplot(5,1,2);plot(hkTime,lampT);ylabel('Lamp T');
subplot(5,1,3);plot(hkTime,monoFlow);ylabel('Mono Flow');
subplot(5,1,4);plot(hkTime,monoT);ylabel('Mono T');
subplot(5,1,5);plot(hkTime,monoP);ylabel('Mono P');xlabel('Time');
saveas(gcf,[char(Flight) slash 'pix' slash char(Flight) '.co.hk.other.fig'],'fig');
saveas(gcf,[char(Flight) slash 'pix' slash char(Flight) '.co.hk.other.jpg'],'jpg');

figure(2);
subplot(3,1,1);plot(hkTime,calP);ylabel('Cal P');title(['VOCALS  ' char(fltno)  '  ' char(Flight)],'FontSize',14);
subplot(3,1,2);plot(hkTime,cellP);ylabel('Cell P');
subplot(3,1,3);plot(hkTime,pmtT);ylabel('PMT T');
saveas(gcf,[char(Flight) slash 'pix' slash char(Flight) '.co.hk.crit.fig'],'fig');
saveas(gcf,[char(Flight) slash 'pix' slash char(Flight) '.co.hk.crit.jpg'],'jpg');

%Load the files
header=[];
dat=[];
NumFiles=size(FileList);
%k=1;
alldata=[];  %matrix of data from all selected files
filerows=0;  %vector containing the number of data rows per file

%Open all files and read in data
for i=1:NumFiles(1)
    files(i,:)=[StartPath,char(Flight), slash,FileList(i,:)]
    fid=fopen(files(i,:));
    %    for j=1:50
    for j=1:alHdrLns
        headerline=fgetl(fid);
%         fprintf('%s',headerline)
    end
%     while ~feof(fid)
        %       dat=fscanf(fid,'%2d.%2d.%2d %2d:%2d:%2d %d %d %f \n',[9,inf]);
        [dat,count]=fscanf(fid,'%2d.%2d.%2d %2d:%2d:%2d %d %d %f %c %c\n',[11,inf]);
%     end

    year=dat(3,1);
    month=dat(2,1);
    day=dat(1,1);
    hr=dat(4,1);
    minute=dat(5,1);
    second=dat(6,1);
    goodRows=find(dat(7,:)>-90);
    goodata=dat(:,goodRows);

    alldata=[alldata;goodata'];
    filerows=[filerows,length(goodata)+filerows(length(filerows))];
    %     goodData=find(alldata(:,7)>-90);
    %    filerows=[filerows,length(goodData)+filerows(length(filerows))];

    YY(i,:)=zero_pad(year);
    MM(i,:)=zero_pad(month);
    DD(i,:)=zero_pad(day);
    HH(i,:)=zero_pad(hr);
    mm(i,:)=zero_pad(minute);
    ss(i,:)=zero_pad(second);

    fclose(fid);

    if i==1
        firstDataSec = hr(1).*3600+minute(1).*60+second(1);
    end
end
filerows(1)=1;

% dday=alldata(goodData,1);
% dmonth=alldata(goodData,2);
% dyear=alldata(goodData,3);
% dhour=alldata(goodData,4);
% dminute=alldata(goodData,5);
% dsecond=alldata(goodData,6);
% time=dsecond+dminute*60+dhour*3600;
% tics=alldata(goodData,7);
% counter=alldata(goodData,8);
% concentration=alldata(goodData,9);

dday=alldata(:,1);
dmonth=alldata(:,2);
dyear=alldata(:,3);
dhour=alldata(:,4);
dminute=alldata(:,5);
dsecond=alldata(:,6);
time=dsecond+dminute*60+dhour*3600;
pastMidniteIx = find(time < firstDataSec);
time(pastMidniteIx) = time(pastMidniteIx)+86400;
tics=alldata(:,7);
counter=alldata(:,8);
oldCts=counter;
counter=medfilt1(oldCts,3); % no de-spiking for VOCALS processing 
% at least for prelim data 5-2-08
figure(99),plot(time,oldCts,'b.',time,counter,'r.');
title(['VOCALS  ' char(fltno)  '  ' char(Flight)],'FontSize',14);
ylim([0 700000]);
saveas(gcf,[char(Flight) slash 'pix' slash char(Flight) '.rawMedFilt.fig'],'fig');
saveas(gcf,[char(Flight) slash 'pix' slash char(Flight) '.rawMedFilt.jpg'],'jpg');
concentration=alldata(:,9);
% cal=and(gt(counter,CalLimit),lt(counter,CalMax));
zero=alldata(:,10)-48;
cal=alldata(:,11)-48;

status=max(cal*2,zero*1);
figure(3),plot(time,concentration,'b.',time,status.*100,'r.');
title(['VOCALS  ' char(fltno)  '  ' char(Flight)],'FontSize',14);


hrCellP = interp1(hkTime,cellP,time);
oldCts=counter;
counter=oldCts.*hrCellP;

clear alldata;

%Flag cal cycle borders with -99
ADJstatus=status;  %adjusted status to add -99's
for ii=2:length(cal)
    if status(ii)-status(ii-1)~=0
        %Flag data after cal (up to DataAfterCal) with -99
        %NOTE: The second part of the if statement below eliminates data flagged as
        % zero between the cal and the zero
        if status(ii)==0 & status(min(ii+after,length(status)))==0
            ADJstatus(ii:ii+DataAfterCal)=-99;
        end
        if ii>before
            ADJstatus(ii-before:ii+after)=-99;
        else
            ADJstatus(1:before)=-99;
        end
    end
end

%Truncate extra -99 values at end of ADJstatus
ADJstatus=ADJstatus(1:length(status));

%Set cal status using ADJstatus
calindex=find(ADJstatus==2);
zeroindex=find(ADJstatus==1);
baddataindex=find(ADJstatus==-99);

%Create new variables containing only good data of types ambient, cal and
%zero.
ambindex=find(ADJstatus==0);
calTime=time(calindex);
zTime=time(zeroindex);
ambTime=time(ambindex);
calDat=counter(calindex);
zDat=counter(zeroindex);
ambDat=counter(ambindex);

dCalTime=diff(calTime);
dZtime=diff(zTime);
calChangeIx=find(dCalTime>1);
zChangeIx=find(dZtime>1);

calEndIx=calChangeIx;
zEndIx=zChangeIx;

calStartIx=calEndIx+1;
zStartIx=zEndIx+1;
calStartIx=[1; calStartIx];
zStartIx=[1; zStartIx];

calEndIx=[calEndIx; length(calTime)];
zEndIx=[zEndIx; length(zTime)];

calStartTimes=calTime(calStartIx);
zStartTimes=zTime(zStartIx);
calEndTimes=calTime(calEndIx);
zEndTimes=zTime(zEndIx);

numCals=length(calStartIx);
numZeroes=length(zStartIx);

% insert code to allow removal of bad or non-cals

calIxAvg=[];
zIxAvg=[];
tCalAvg=[];
calAvg=[];
tZavg=[];
zAvg=[];
for ix=1:numCals
    calIxAvg=[calIxAvg; (calStartIx(ix)+calEndIx(ix))./2];
    tCalAvg=[tCalAvg; mean(calStartTimes(ix):calEndTimes(ix))];
    calAvg=[calAvg; mean(calDat(calStartIx(ix):calEndIx(ix)))];
end

for ix=1:numZeroes
    zIxAvg=[zIxAvg (zStartIx(ix)+zEndIx(ix))./2];
    zAvg=[zAvg; mean(zDat(zStartIx(ix):zEndIx(ix)))];
    tZavg=[tZavg; mean(zTime(zStartIx(ix):zEndIx(ix)))];
end

figure(4);
plot(time,counter,'k.',tCalAvg,calAvg,'b.',tZavg,zAvg,'r.');title(['VOCALS  ' char(fltno)  '  ' char(Flight)],'FontSize',14);
saveas(gcf,[char(Flight) slash 'pix' slash char(Flight) '.co.raw.fig'],'fig');
saveas(gcf,[char(Flight) slash 'pix' slash char(Flight) '.co.raw.jpg'],'jpg');

% [badCals,badZs]=editCOcals_dialog_box(tCalAvg,calAvg,tZavg,zAvg,numCals,numZeroes);


% calStartTime(1)=calTime(1)
numBadCals=length(badCals);
numBadZs=length(badZs);

tCalAvg(badCals)=-999;
gdCalIx=find(tCalAvg>0);
tCalAvg=tCalAvg(gdCalIx);
calAvg=calAvg(gdCalIx);

tZavg(badZs)=-999;
gdZIx=find(tZavg>0);
tZavg=tZavg(gdZIx);
zAvg=zAvg(gdZIx);

figure(5);
plot(time,counter,'b.',tCalAvg,calAvg,'k.',tZavg,zAvg,'r.');title(['VOCALS  ' char(fltno)  '  ' char(Flight)],'FontSize',14);

sens=[];
midCalIx=find(and(time>=tCalAvg(1), time<=tCalAvg(end)));
sens=interp1(tCalAvg,calAvg,time(midCalIx));
earlyCalIx=find(time<tCalAvg(1));
lateCalIx=find(time > tCalAvg(end));
earlySens=ones(size(earlyCalIx)).*sens(1);
lateSens=ones(size(lateCalIx)).*sens(end);
% sens=[ones(calIxAvg(1)).*sens(1); sens; ones(lateCalIx).*sens(end)];
sens2=[earlySens; sens; lateSens];

zFit=[];
midZix=find(and(time>=tZavg(1),time<=tZavg(end)));
zFit=interp1(tZavg,zAvg,time(midZix));
earlyZix=find(time<tZavg(1));
lateZix=find(time>tZavg(end));
earlyZ=ones(size(earlyZix)).*zFit(1);
lateZ=ones(size(lateZix)).*zFit(end);
zFit=[earlyZ; zFit; lateZ];

figure(6);
plotyy(time,sens2,time,zFit);title(['VOCALS  ' char(fltno)  '  ' char(Flight)],'FontSize',14);
saveas(gcf,[char(Flight) slash 'pix' slash char(Flight) '.co.fitparms.fig'],'fig');
saveas(gcf,[char(Flight) slash 'pix' slash char(Flight) '.co.fitparms.jpg'],'jpg');
% badIx = 1;
% if badCals(1) > 1
%     modTCalAvg=[modTCalAvg; tCalAvg(1:badCal(1))];
% end
%
% if badIx > 0
%     for idx=1:numCals
%         if badCals(badIx) ~= idx && badIx <= length(numCals)
%             modTCalAvg=[modTCalAvg; tCalAvg(idx)];
%             badIx = badIx + 1;
%         end
%     end
% end

% sensCoefs = polyfit(calTime,calDat,2);
% zCoefs = polyfit(zTime,zDat,2);
%
% sensFit = sensCoefs(1) + sensCoefs(2).*time + sensCoefs(3).*time.*time;
% zFit = zCoefs(1) + zCoefs(2).*time + zCoefs(3).*time.*time;

comr = (counter - zFit)./(sens2 - zFit).*tankCon;
% correct for early flights' non-unity compressor effy
comr = (comr - corrInt)./corrSlope;

figure(7);
plot(time,comr,'b.');title(['VOCALS  ' char(fltno)  '  ' char(Flight)],'FontSize',14);
ylim([-100 600]);
saveas(gcf,[char(Flight) slash 'pix' slash char(Flight) '.comr.tseries.fig'],'fig');
saveas(gcf,[char(Flight) slash 'pix' slash char(Flight) '.comr.tseries.jpg'],'jpg');

if FlightData
    ncid = netcdf.open(rafFile,'NC_NOWRITE');
    rafInterval=netcdf.getAtt(ncid,netcdf.getConstant('NC_GLOBAL'),'TimeInterval');
    rafMissionT=netcdf.getVar(ncid,0); % varid 0 = Time
    rafHrBeg=rafInterval(1:2);
    rafMnBeg=rafInterval(4:5);
    rafScBeg=rafInterval(7:8);
    rafHrEnd=rafInterval(10:11);
    rafMnEnd=rafInterval(13:14);
    rafScEnd=rafInterval(16:17);
    rafStartT=str2num(rafHrBeg).*3600+str2num(rafMnBeg).*60+str2num(rafScBeg);
    rafTime=double(rafMissionT + rafStartT);
    rafEndT=str2num(rafHrEnd).*3600+str2num(rafMnEnd).*60+str2num(rafScEnd);

    varid = netcdf.inqVarID(ncid,'ALTX');
    rafAlt=netcdf.getVar(ncid,varid);
    varid = netcdf.inqVarID(ncid,'PSXC');
    rafP=netcdf.getVar(ncid,varid);
    varid = netcdf.inqVarID(ncid,'THETA');
    rafTheta=netcdf.getVar(ncid,varid);
    varid = netcdf.inqVarID(ncid,'THETAE');
    rafThetaE=netcdf.getVar(ncid,varid);
    varid = netcdf.inqVarID(ncid,'THETAV');
    rafThetaV=netcdf.getVar(ncid,varid);
    varid = netcdf.inqVarID(ncid,'MR');
    mr=netcdf.getVar(ncid,varid);


    altIx=find(isnan(rafAlt)==0);
    mrgAlt=interp1(rafTime(altIx),rafAlt(altIx),time);
    pIx=find(isnan(rafP)==0);
    mrgP=interp1(rafTime(pIx),rafP(pIx),time);
    thetaIx=find(isnan(rafTheta)==0);
    mrgTheta=interp1(rafTime(thetaIx),rafTheta(thetaIx),time);
    thetaEix=find(isnan(rafThetaE)==0);
    mrgThetaE=interp1(rafTime(thetaEix),rafThetaE(thetaEix),time);
    thetaVix=find(isnan(rafThetaV)==0);
    mrgThetaV=interp1(rafTime(thetaVix),rafThetaV(thetaVix),time);
    mrIx=find(isnan(mr)==0);
    mrgMR=interp1(rafTime(mrIx),mr(mrIx),time);

    figure(8);
    plot(mrgMR,mrgTheta);title(['VOCALS  ' char(fltno)  '  ' char(Flight)],'FontSize',14);
    saveas(gcf,[char(Flight) slash 'pix' slash char(Flight) '.mr.vs.theta.fig'],'fig');
    saveas(gcf,[char(Flight) slash 'pix' slash char(Flight) '.mr.vs.theta.jpg'],'jpg');
end

%Plot every 100th point (plotting all the points is very slow
n=1;
for i=1:5:length(time)
    ptime(n)=time(i);
    pcounter(n)=counter(i);
    pconcentration(n)=concentration(i);
    n=n+1;
end
n=1;
for i=1:5:length(calindex)
    pcalindex(n)=calindex(i);
    n=n+1;
end
n=1;
for i=1:5:length(zeroindex)
    pzeroindex(n)=zeroindex(i);
    n=n+1;
end
n=1;
for i=1:5:length(baddataindex)
    pbaddataindex(n)=baddataindex(i);
    n=n+1;
end

if length(calindex)>1 & length(zeroindex)>1 & length(baddataindex)>1
    %Plot all concentration data
    %   plot(time,concentration,'b.',time(calindex),concentration(calindex),'r.',...
    %      time(zeroindex),concentration(zeroindex),'g.',time(baddataindex),...
    %      concentration(baddataindex),'k.');
    %Plot partial concentration data
    %   plot(ptime,pconcentration,'b.',time(pcalindex),concentration(pcalindex),'r.',...
    %      time(pzeroindex),concentration(pzeroindex),'g.',time(pbaddataindex),...
    %      concentration(pbaddataindex),'k.');
    %Plot partial counter data

    figure(9);
    %     plot(ptime,pcounter,'b.',time(pcalindex),counter(pcalindex),'r.',...
    %        time(pzeroindex),counter(pzeroindex),'g.',time(pbaddataindex),...
    %        counter(pbaddataindex),'k.');
    plot(ptime,pcounter,'b.',time(pbaddataindex),counter(pbaddataindex),'k.',...
        tCalAvg,calAvg,'r.',tZavg,zAvg,'g.');
    title(['VOCALS  ' char(fltno)  '  ' char(Flight)],'FontSize',14);
    ylim([0 1000000]);
    saveas(gcf,[char(Flight) slash 'pix' slash char(Flight) '.co.raw.filtrd.fig'],'fig');
    saveas(gcf,[char(Flight) slash 'pix' slash char(Flight) '.co.raw.filtrd.jpg'],'jpg');

else
    Message='No plot displayed as no calibration or no zero occurred in the period examined'
end

%Delete any previous output files
% warning off;
% for i=1:NumFiles(1)
%    outfile=[YY(i,:) MM(i,:) DD(i,:) '_' HH(i,:) '_CO.dat'];
%    delete([StartPath,char(Flight),slash,outfile]);
% end
% warning on;


hourcheck='99';

outfile=['gv_' YY(1,:) MM(1,:) DD(1,:) '_CO.dat'];
delete([StartPath,char(Flight), slash,outfile]);
outfile=['gv_' YY(1,:) MM(1,:) DD(1,:) '_CO.dat']
fid2=fopen([StartPath,char(Flight), slash,outfile],'a');

fprintf(fid2,'%s\n','#8 Header Lines');
fprintf(fid2, '%s\n','#NCAR N130AR C-130 In Situ CO Data');
fprintf(fid2,'%s\n','#Teresa Campos, Ilana Pollack, and Cliff Heizer, NCAR ESSL and EOL, CARI Group');
fprintf(fid2,'%s\n%s\n','#PO Box 3000','#Boulder, CO 80307');
fprintf(fid2,'%s\n','#Contact Info: 303-497-1048, campos@ucar.edu');
fprintf(fid2,'%s%d  %s%d  %s%d  %s%d  %s%d  %s%d  %s%d %s%d  %s%f  %s%d\n',...
    '#Settings for this processing run: LowCalLim_cts: ',CalLimit,...
    'CalMax_cts: ',CalMax,'ZeroMax_cts: ',ZeroLimit,'ZeroMin_cts: ',ZeroMin,...
    'PriorPtsRemoved: ',before,'PostChangePtsRemoved: ',after,...
    'PostCalPtsRemoved: ',DataAfterCal,'NumPtsToAvg: ',ptsToAvg,...
    'CalTankCon: ',tankCon,'TimeShift_s: ',toffset);
fprintf(fid2,'%s','#Time CO_ppbv Status Counter Sens ');
fprintf(fid2,'%s\n', 'Offset');

if ~FlightData
    bad=find(isnan(counter)==1);
    outtime=time;outtime(bad)=-32767;
    outco=comr;outco(bad)=-32767;
    outStatus=ADJstatus; outStatus(bad)=-32767;
    outCts=counter; outCts(bad)=-32767;
    outSens=sens2; outZ=zFit;

    for i=1:length(time)
        fprintf(fid2,'%d\t%f\t%d\t%f\t%f\t%f\n',time(i),outco(i),outStatus(i),outCts(i),outSens(i),outZ(i));
    end
    fclose(fid2);
end
%    outday=[];
%    outyear=[];
%    outhour=[];
%    outminute=[];
%    outsecond=[];
%    outtics=[];
%    outcounter=[];
%    outconcentration=[];
%    outstatus=[];
%    outmr=[];
%    outsens=[];
%    outz=[];

% for i=1:NumFiles(1)
%    outfile=[YY(i,:) MM(i,:) DD(i,:) '_' HH(i,:) '_CO.dat']
%    fid2=fopen([StartPath,char(Flight),slash,outfile],'a');

%    outmonth=dmonth(filerows(i):filerows(i+1));

%tic
%outmonthS(1:length(outmonth),1)='0';
%toc
%for j=1:length(outmonth)
%   outmonthS(j,:)=zero_pad(outmonth(j));
%end
%toc

windowSize=18;
smoothCounter = filter(ones(1,windowSize)/windowSize,1,counter); %running average algorithm
%     smoothTime = filter(ones(1,windowSize)/windowSize,1,time);

%    outday=dday(filerows(i):filerows(i+1));
%    outyear=dyear(filerows(i):filerows(i+1));
%    outhour=dhour(filerows(i):filerows(i+1));
%    outminute=dminute(filerows(i):filerows(i+1));
%    outsecond=dsecond(filerows(i):filerows(i+1));
%    outtics=tics(filerows(i):filerows(i+1));
%    outcounter=smoothCounter(filerows(i):filerows(i+1));
%    outconcentration=concentration(filerows(i):filerows(i+1));
%    outstatus=ADJstatus(filerows(i):filerows(i+1));
%    outmr=comr(filerows(i):filerows(i+1));
%    outsens=sens2(filerows(i):filerows(i+1));
%    outz=zFit(filerows(i):filerows(i+1));
% outTime=time-toffset;  for constant lag only
% Apply alt-dependent time lag:
if FlightData==0
    lagT=ones(size(counter)).*6;
else
    lagT=0.5344.*mrgP.*6./0.2./1013;  % From 53.44 cc inlet vol*0.4slm*p/1013*60s-min^-1/1000cc-L^-1
    badP=find(isnan(mrgP)==1);
    lagT(badP)=6;
end
adjTime=time-toffset+8-lagT;
adjCOmr=comr;
adjStatus=ADJstatus;
overlap=diff(adjTime);
uniqueT=find(overlap~=0);
redundantT=find(overlap==0);
if (length(redundantT))
    Message='Eliminating redundant time stamps'
    for ii=1:length(redundantT)
        adjTime(redundantT(ii)+1)=0.5.*(adjTime(redundantT(ii))+adjTime(redundantT(ii)+1));
        adjCOmr(redundantT(ii)+1)=0.5.*(comr(redundantT(ii))+comr(redundantT(ii)+1));
        adjStatus(redundantT(ii)+1)=max(ADJstatus(redundantT(ii):redundantT(ii)+1));
    end
    outTime=[adjTime(uniqueT); adjTime(end)];
    adjCOmr=[adjCOmr(uniqueT); adjCOmr(end)];
    adjStatus=[adjStatus(uniqueT); adjStatus(end)];
    % else
    % adjCOmr=comr;
    % adjStatus=ADJstatus;
end;

outTime=adjTime;

dT=diff(outTime);
dT=[dT; 1];
if min(dT)==0
    Message='Repairing Hal"s repeating time stamps...'
    zeroTix=find(dT==0);
    outTime(zeroTix+1)=outTime(zeroTix)+1;
end
% timeIx=find(isnan(outTime)==1|outTime<-30);
% outTime(timeIx)=BDF;
warning off

if FlightData
    outday=interp1q(outTime,dday,rafTime);
    dayIx=find(isnan(outday)==1|outday<-30);
    outday(dayIx)=BDF;
    outmonth=interp1q(outTime,dmonth,rafTime);
    monthIx=find(isnan(outmonth)==1|outmonth<-30);
    outmonth(monthIx)=BDF;
    outyear=interp1q(outTime,dyear,rafTime);
    yrIx=find(isnan(outyear)==1|outyear<-30);
    outyear(yrIx)=BDF;

    outtics=interp1q(outTime,tics,rafTime);
    ticsIx=find(isnan(outtics)==1|outtics<-30);
    outtics(ticsIx)=BDF;
    outcounter=interp1q(outTime,smoothCounter,rafTime);
    ctrIx=find(isnan(outcounter)==1|outcounter<-30);
    outcounter(ctrIx)=BDF;
    outconcentration=interp1q(outTime,concentration,rafTime);
    concIx=find(isnan(outconcentration)==1|outconcentration<-30);
    outconcentration(concIx)=BDF;
    outstatus=interp1q(outTime,ADJstatus,rafTime);
    statIx=find(isnan(outstatus)==1|outstatus<-30);
    outstatus(statIx)=BDF;
    outmr=interp1q(outTime,comr,rafTime);
    mrIx=find(isnan(outmr)==1|outmr<-30);
    outmr(mrIx)=BDF;
    outsens=interp1q(outTime,sens2,rafTime);
    sensIx=find(isnan(outsens)==1|outsens<-30);
    outsens(sensIx)=BDF;
    outz=interp1q(outTime,zFit,rafTime);
    zeroIx=find(isnan(outz)==1|outz<-30);
    outz(zeroIx)=BDF;
    outCellP=interp1q(outTime,hrCellP,rafTime);
    pCellIx=find(isnan(outCellP)==1|outCellP<-30);
    outCellP(pCellIx)=BDF;
    badAlt=find(isnan(mrgAlt)==1|mrgAlt<-30);
    mrgAlt(badAlt)=BDF;
    badTheta=find(isnan(mrgTheta)==1|mrgTheta<-30);
    mrgTheta(badTheta)=BDF;
    badThetaE=find(isnan(mrgThetaE)==1|mrgThetaE<-30);
    mrgThetaE(badThetaE)=BDF;
    badThetaV=find(isnan(mrgThetaV)==1|mrgThetaV<-30);
    mrgThetaV(badThetaV)=BDF;
    badMR=find(isnan(mrgMR)==1|mrgMR<-30);
    mrgMR(badMR)=BDF;

    warning on

    varid = netcdf.inqVarID(ncid,'COMR_AL');
    rafCO=netcdf.getVar(ncid,varid);

    % % %    outputdata=[outday,outmonth,outyear,outhour,outminute,outsecond,outconcentration]';
    % outputdata=[outday,outmonth,outyear,rafTime,outtics,...
    %     outcounter,outconcentration,outstatus,outmr,outsens,outz]';
    % outputdata2=[hrCellP,mrgAlt,mrgTheta,mrgThetaE,mrgThetaV,mrgMR]';
    % %    %Print header at start of combined hour file
    % %    if strcmp(hourcheck,HH(i,:))==0
    % % %       fprintf(fid2,'%s\n','#Date Time Concentration ');
    % %       fprintf(fid2,'%s\n','#Date Time Tics Counter Concentration Status ');
    % %    end
    % %
    % % %    fprintf(fid2,'%02d.%02d.%02d %02d:%02d:%02d %f\n',outputdata);
    % % for ix=1:length(outTime)
    % for ix=1:length(outTime)
    %
    %
    %     fprintf(fid2,'%02d.%02d.%02d %d %d %d %f %d %f %f %f ',outputdata(:,ix));
    %     fprintf(fid2,'%f %f %f %f %f %f\n',outputdata2(:,ix));
    %     if (dT(ix)>1)
    %         for ixx=1:dT(ix)-1
    %             fprintf(fid2,'%02d.%02d.%02d %d -999 -999 -999 -999 -999 -999 -999 ',...
    %                 outday(ix),outmonth(ix),outyear(ix),outTime(ix)+ixx);
    %             fprintf(fid2,'-999 -999 -999 -999 -999 -999\n');
    %         end
    %     end
    % end
    % %    hourcheck=HH(i,:);
    % %    fclose(fid2);
    % % end
    %
    % %    outputdata=[outday,outmonth,outyear,outhour,outminute,outsecond,outtics,...
    % %          outcounter,outconcentration,outstatus,outmr,outsens,outz]';
    %
    % %    fprintf(fid2,'%02d.%02d.%02d %02d:%02d:%02d %f\n',outputdata);
    % %    fprintf(fid2,'%02d.%02d.%02d %02d:%02d:%02d %d %d %f %d %f %f %f\n',outputdata);
    % %    hourcheck=HH(i,:);
    % fclose(fid2);

    outfile=['gv_' YY(1,:) MM(1,:) DD(1,:) '.CO.prelim.dat'];
    delete([StartPath,char(Flight), slash,outfile]);
    outfile=['gv_' YY(1,:) MM(1,:) DD(1,:) '.CO.prelim.dat']
    prelimIx=find(outstatus~=0);
    %    prelimTix=find(outTime>rafStartT&outTime<rafEndT);
    %    prelimT=outTime(prelimTix);
    if length(rafTime)>length(outmr)  % fill time gaps if necessary
        gap=diff(outTime);
        gix=find(gap~=1);
        prelimT=outTime(1:gix(1));
        prelimMR=outmr(1:gix(1));
        prelimStat=outstatus(1:gix(1));
        for ix=1:length(gix)-1
            ixx=gap(gix(ix));
            prelimT=[prelimT; (outTime(gix(ix))+1:outTime(gix(ix))+ixx-1)'];
            prleimT=[prelimT; (outTime(gix(ix))+ixx:outTime(gix(ix+1)))'];
            prleimMR=[prelimMR; (BDF*ones(size(1:ixx-1)))'];
            prelimMR=[prelimMR; outmr(gix(ix)+1:gix(ix+1))];
            prelimStat=[prelimStat; (BDF*ones(size(1:ixx-1)))'];
            prelimStat=[prelimStat; outstatus(gix(ix)+1:gix(ix+1))];
        end

        finalTout=[prelimT; (outTime(gix(end))+1:outTime(gix(end))+gap(gix(end))-1)'];
        finalTout=[finalTout; outTime(gix(end)+1:end)];
        finalMRout=[prelimMR; (BDF*ones(size(1:gap(gix(end))-1)))'];
        finalMRout=[finalMRout; outmr(gix(end)+1:end)];
        finalStatOut=[prelimStat; (BDF*ones(size(1:gap(gix(end))-1)))'];
        finalStatOut=[finalStatOut; outstatus(gix(end)+1:end)];

        % assumes rafT begins earlier and ends later than CO time (not always
        % true).
        if rafTime(1) <= finalTout(1)
            begCOout=find(rafTime==finalTout(1));
            begBDF=BDF*ones(size(rafTime(1:begCOout-1)));
            finalTout=[rafTime(1:begCOout-1); finalTout];
            finalMRout=[begBDF; finalMRout];
            finalStatOut=[begBDF; finalStatOut];
        end

        if rafTime(end) >= finalTout(end)
            lastHalT=find(rafTime==finalTout(end));
            endBDF=BDF*ones(size(rafTime(lastHalT+1:end)));
            finalTout=[finalTout; rafTime(lastHalT+1:end)];
            finalMRout=[finalMRout; endBDF];
            finalStatOut=[finalStatOut; endBDF];
        end

        if rafTime(1) > finalTout(1)
            print('RAF data system turnon later than Hal\n');
        end

        if rafTime(end) < finalTout(end)
            print('RAF data system turned off earlier than Hal\n');
        end
    end % if length(rafTime)>length(outmr)
    if length(rafTime)==length(outmr)
        finalMRout=outmr;
        finalTout=rafTime;
        finalStatOut=outstatus;
    end % if length(rafTime)==length(outmr)
    length(rafTime)
    length(finalTout)
    length(finalMRout)
    length(finalStatOut)

    switch char(fltno)
        case 'gn01'
            badix=1;
        case 'tf02'
            badix=1;
%         case 'ff03'
%             badix=find((rafTime>67500&rafTime<68500)|(rafTime>72500&rafTime<73000));
%         case 'rf01'
%             badix=find((rafTime>59000&rafTime<61000)|(rafTime>64000&rafTime<64300));
%         case 'rf02'
%             badix=find((rafTime>60000&rafTime<62000)|(rafTime>67500&rafTime<68000));
%         case 'rf03'
%             badix=find((rafTime>58500&rafTime<60000)|(rafTime>64000&rafTime<64700));
%         case 'rf04'
%             badix=find((rafTime>64000&rafTime<64500)|(rafTime>67000&rafTime<68000));
%         case 'rf05'
%             badix=find((rafTime>63500&rafTime<64500)|(rafTime>65450&rafTime<65520));
%         case 'rf06'
%             badix=find((rafTime>37500&rafTime<38000)|(rafTime>42500&rafTime<42800));
%         case 'rf07'
%             badix=find((rafTime>59000&rafTime<60000)|(rafTime>79000&rafTime<79500));
%         case 'rf08'
%             badix=find((rafTime>59000&rafTime<61000)|(rafTime>64000&rafTime<64400));
%         case 'rf09'
%             badix=find((rafTime>59000&rafTime<61000)|(rafTime>64000&rafTime<64200));
%         case 'rf10'
%             badix=find((rafTime>58000&rafTime<60000)|(rafTime>64000&rafTime<65000));
%         case 'rf11'
%             badix=find((rafTime>62000&rafTime<62500)|(rafTime>69500&rafTime<70300));
%         case 'rf12'
%             badix=find((rafTime>58000&rafTime<65000));
%         case 'rf13'
%             badix=find((rafTime>36500&rafTime<42900));
%         case 'rf14'
%             badix=find((rafTime>59000&rafTime<64000));
%         case 'ff04'
%             badix=find((rafTime<76400));
        otherwise
            badix=1;
    end

    stripBDF=find(finalStatOut~=0);
    finalMReditd=finalMRout;
    finalMReditd(stripBDF)=-32767;
    finalMReditd(badix)=-32767;
    finalMRnan=finalMReditd;
    finalMRnan(stripBDF)=NaN;
    finalMRnan(badix)=NaN;
    figure(10);plot(adjTime,adjCOmr,'g.',rafTime,rafCO,'r.',finalTout,finalMReditd,'b.');title(['VOCALS  ' char(fltno)  '  ' char(Flight)],'FontSize',14);
    grid;ylim([0 200]);
    saveas(gcf,[char(Flight) slash 'pix' slash char(Flight) '.co.timelag.fig'],'fig');
    saveas(gcf,[char(Flight) slash 'pix' slash char(Flight) '.co.timelag.jpg'],'jpg');

    %    prelimMR(prelimIx)=BDF;
    %    prelimMR=prelimMR(prelimTix);
    %    prelimOut=[prelimT,prelimMR]';
    prelimOut=[finalTout,finalMReditd]';
    %    dPrelimT=diff(prelimT);
    %    dPrelimT=[dPrelimT; 1];

    fid4=fopen([StartPath,char(Flight), slash,outfile],'a');

    fprintf(fid4,'%s\n','#Time CO_ppbv');

    for ix=1:length(finalTout)
        %       for ix=1:length(prelimT)
        %           if(outTime(ix)>rafStartT&&outTime(ix)<rafEndT)
        fprintf(fid4,'%d %d\n',prelimOut(:,ix));
        %               if (dPrelimT(ix)>1)
        %                   for ixx=1:dPrelimT(ix)-1
        %                       fprintf(fid4,'%d -999\n',prelimT(ix)+ixx);
        %                   end
        %               end
        %           end
    end
    fclose(fid4);

    %       writeStatus=nc_varput([char(Flight) slash rafFile(11:end) ] ,'COMR_AL',finalMReditd)
% mrl=nc_varget(rafFile,'XRAWMRL_MC');
varid = netcdf.inqVarID(ncid,'MR');
mr=netcdf.getVar(ncid,varid);

varid = netcdf.inqVarID(ncid,'THETA');
theta=netcdf.getVar(ncid,varid);
varid = netcdf.inqVarID(ncid,'ATX');
atx=netcdf.getVar(ncid,varid);
netcdf.close(ncid);
    Flight
    toc
    figure(11);plot(finalMReditd,rafTheta,'b.'); xlim([0 300]);title(['VOCALS  ' char(fltno)  '  ' char(Flight)],'FontSize',14);
    saveas(gcf,[char(Flight) slash 'pix' slash char(Flight) '.co.vs.theta.fig'],'fig');
    saveas(gcf,[char(Flight) slash 'pix' slash char(Flight) '.co.vs.theta.jpg'],'jpg');
    zoom
end
save([char(Flight) slash char(Flight) '.mat']);

%axlimdlg
