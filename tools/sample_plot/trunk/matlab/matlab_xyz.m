%Authorship: NCAR Earth Observing Laboratory Data Management & Services Group
%Contact: eol-archive@ucar.edu

%Licensing: The code associated with this document is provided freely and openly. 
% Users are hereby granted a license to access and use this code, unless otherwise 
% stated, subject to the terms and conditions of the GNU Affero General Public 
% License 3.0 (AGPL-3.0; https://www.gnu.org/licenses/agpl-3.0.en.html). This 
% documentation and associated code are provided "as is" and are not supported. 
% By using or downloading this code, the user agrees to the terms and conditions 
% set forth in this code and in the "Using MATLAB to View OPeNDAP Files" document.

%Acknowledgment: This work was sponsored by the National Science Foundation. 
% This material is based upon work supported by the National Center for Atmospheric 
% Research, a major facility sponsored by the National Science Foundation and managed 
% by the University Corporation for Atmospheric Research. Any opinions, findings 
% and conclusions or recommendations expressed in this material do not necessarily 
% reflect the views of the National Science Foundation.

% NOTE:
% For users using MATLAB 2021b:
% If you are on Linux and get this error:
% "Error using matlab.internal.imagesci.netcdflib"

% try creating a .dodsrc file in the directory where the matlab program 
% exists that contains the following:

% HTTP.SSL.CAINFO=/etc/ssl/certs/ca-bundle.crt
% Then restart matlab and try running the program again.

% Refer to the "Using MATLAB to View OPeNDAP Files" document for more information on the script.
% For questions about MATLAB commands and other features, refer to the
% MathWorks help center page at
% https://www.mathworks.com/help/index.html

% This script shows how the data from a OPeNDAP file can be used to create a
% 3D plot to visualize the data. This script takes one time and creates a
% plot for the entire area of the file.

% This script reads in a OPeNDAP file either through a URL or as a saved 
% file. The user stores the desired parameters in variables. This script
% then takes a given time and stores it along with the entire area in the
% OPeNDAP file. The file then plots the parameters for the entire area in
% the given time.

% This script shows an example of taking an opendap url and plotting the
% temperature for a given location. Note that this example is a guide and
% may need to be changed.

% The "disp" MATLAB command is used to display a message to the command 
% window.

disp("Begin sample_matlab_XYZplot.m")

% Create a variable for the opendap url or for the local file.

myFile = 'https://data.eol.ucar.edu/opendap/data/esop_95/hrly_sfc/ES95HRLY_950715.qcf';
%myFile='ES95HRLY_950715.qcf';

% Use the 'ncdisp' command to view the contents of the OPeNDAP file.

%ncdisp(myFile)

% Use the 'ncread' command to take the desired variables. For this example
% date, time, latitude, longitude, and dew point and air temperature were
% used.

lat = ncread(myFile, 'QCF.latitude');
long = ncread(myFile, 'QCF.longitude');

dew_point = ncread(myFile, 'QCF.dew_point_temperature');
air_temp = ncread(myFile, 'QCF.air_temperature');

date = ncread(myFile, 'QCF.date_nominal');

% For this example, some processing was needed as the variables from the
% OPeNDAP file were taken in a weird way. In this example, the variables
% were 'char' and were in a matrix. The following code takes this matrix
% and reorganizes the data for later uses. These steps may not be
% necessary for every file. This was needed for non number type variables.

% Find the dimensions of the 'date' matrix.

[rows,cols] = size(date);

% Create an array of the type 'string'.

final_date = strings(cols,1);

% Repeat for the time.

Time = ncread(myFile, 'QCF.time_nominal');
final_time = strings(cols,1);

% This loop takes every 'char' column from the matrix and stores is as a
% 'string' into the array.

c = 1;
while c <= cols
    final_date(c,1) = string([date(:,c)].');
    final_time(c,1) = string([Time(:,c)].');
    c = c + 1;
end

% The empty elements of the array are removed.

final_date(:,1) = deblank(final_date(:,1));
final_time(:,1) = deblank(final_time(:,1));

% The date and time are then used to create the datetime.

date_time_arr = final_date + ' ' + final_time;
date_time = datetime(date_time_arr,'InputFormat','yyyy/MM/dd HH:mm:ss');
u_datetime = unique(date_time);
u_date = unique(final_date);
u_time = unique(final_time);

% Take one time and find all of the indices where the time shows in the
% file. This is used to find all of the information for the given time.

test_time = 1;

t = u_time(test_time);
array_time = strings(cols,1);
 
for p = 1:cols
    if t == final_time(p)
        array_time(p) = t;
    end
end
 
% This statement removes all empty values in the array.

array_time = array_time(~cellfun('isempty',array_time)); 
% % Create arrays to store lat, long, and dew temp and air temp.
   
latitude = zeros(length(array_time),1);
longitude = zeros(length(array_time),1);
dew_point_temp = zeros(length(array_time),1);
air_temp_arr = zeros(length(array_time),1);

% % This for loop finds the location info and the dew temp and air temp
% % for the given time.
% 
ind = 1;
for p = 1:cols
    if t == final_time(p)
        latitude(ind,1) = lat(p);
        longitude(ind,1) = long(p);
        dew_point_temp(ind,1) = dew_point(p);
        air_temp_arr(ind,1) = air_temp(p);
        ind = ind + 1;
    end
end

% The example file contains -999 values for the temp. The following
% replaces these values with 'NaN' so it doesn't affect the graph.

for p = 1:length(dew_point_temp)
    if dew_point_temp(p) < -100
        dew_point_temp(p) = NaN;
    end
end

for p = 1:length(air_temp_arr)
    if air_temp_arr(p) < -100
        air_temp_arr(p) = NaN;
    end
end

% 
% % Create the 3D grid for the plot.
% 
xv = linspace(min(latitude), max(latitude), 100);
yv = linspace(min(longitude), max(longitude), 100);
[X,Y] = meshgrid(xv, yv);
% 
% % Create a variable to store the 3D plot.
% 
Z = griddata(latitude,longitude,air_temp_arr,X,Y);
% 
% % Plot the data.
% 
figure(1)
% 
% % The surf command creates the surface plot.
% 
surf(X, Y, Z);
hold on
% 
% % The stem3 command creates the vertical lines in the plots.
% 
%stem3(latitude, longitude, air_temp_arr, 'k');
grid on
shading interp
xlabel('latitude')
ylabel('longitude')
zlabel('°C')
title("GCIP/ESOP-95 Surface Air Temp" + " " + datestr(u_datetime(test_time,1)) ...
    + " " + array_time(test_time))
c = colorbar;
c.Label.String = '°C';

disp('End sample_matlab_XYZplot.m')