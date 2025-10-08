function [Z,TG,TM] = interpolate(gFilename, mFilename, time, outFilename)

%Created by Sean Stroble, Last Modified July 19 2010
%This script will read in temp vs altitude profiles from a G* file and an M* file and interpolate those values to a standard scale (500 to 15000 in steps of 500)

gFile = fopen(gFilename);
mFile = fopen(mFilename);

%Output vars
Z = [];
TG = [];
TM = [];

%internal vars
GALT = [];
GTEMP = [];
MALT = [];
MTEMP = [];

%Load G File
n = 0;
gLine = fgetl(gFile); %Get a line from G File
while (ischar(gLine)) %make sure the line is valid
    split = sscanf(gLine, '%lg'); %Collect all floating point number from the line
    n = numel(split); %how many did we get?
    if (n == 11) %if there are 11 numbers on the line, its correct data
        if (split(2) ~= -999 && split(3) ~= -999) %ignore -999
            GALT(end+1) = split(2); %GHGT (m)
            GTEMP(end+1) = split(3) + 273.15; %TEMP (K)
        end;
    end;
    gLine = fgetl(gFile); %get the next line
end;

%Find M File position of intrest
n = 0;
mLine = fgetl(mFile);%Get a line from M File
while (ischar(mLine)) %make sure the line is valid
    split = sscanf(mLine, '%lg'); %Collect all floating point number from the line
    n = numel(split); %how many did we get?
    if (n == 16)  %if there are 16 numbers on the line, its header data
        if (split(1) == time) %if we found the time of intrest
            break; %exit the while loop
        end;
    end;
    mLine = fgetl(mFile); %get the next line
end;

%read data from M file
mLine = fgetl(mFile);%Get a line from M File
while (ischar(mLine)) %make sure the line is valid
    split = sscanf(mLine, '%lg'); %Collect all floating point number from the line
    n = numel(split); %how many did we get?
    if (n == 5) %if there are 5 numbers on the line, its data
        MALT(end+1) = split(4); %geometric altitude (m)
        MTEMP(end+1) = split(2); %temp from MTP (K)
    else
        break; %we have exited the block so exit the read loop
    end;
    mLine = fgetl(mFile); %get the next line
end;

%Clean up
fclose(gFile);
fclose(mFile);

Z = [500:500:15000]; %create list of static altitiudes 500 to 15000 in steps of 500

%interpolate 
TG = interp1(GALT, GTEMP, Z);
TM = interp1(MALT, MTEMP, Z);

if (~strcmp(outFilename, ''))
    outFile = fopen(outFilename, 'w');
    fprintf(outFile, 'Z,TG,TM\n');
    fclose(outFile);

    dlmwrite(outFilename, [Z',TG',TM'], '-append'); %Save Z
end


return;
