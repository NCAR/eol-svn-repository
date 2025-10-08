%Authorship: NSF NCAR Earth Observing Laboratory Data Management & Services Group
%Contact: eol-archive@ucar.edu

%Licensing: The code associated with this document is provided freely and openly. 
% Users are hereby granted a license to access and use this code, unless otherwise 
% stated, subject to the terms and conditions of the GNU Affero General Public 
% License 3.0 (AGPL-3.0; https://www.gnu.org/licenses/agpl-3.0.en.html). This 
% documentation and associated code are provided "as is and are not supported. 
% By using or downloading this code, the user agrees to the terms and conditions 
% set forth in this document.

%Acknowledgment: This work was sponsored by the National Science Foundation. 
% This material is based upon work supported by the National Center for Atmospheric 
% Research, a major facility sponsored by the National Science Foundation and managed 
% by the University Corporation for Atmospheric Research. Any opinions, findings 
% and conclusions or recommendations expressed in this material do not necessarily 
% reflect the views of the National Science Foundation.

% NOTE:
% for users using MATLAB 2021b:
% If you are on Linux and get this error:
% "Error using matlab.internal.imagesci.netcdflib"

% try creating a .dodsrc file in the directory where the matlab program 
% exists that contains the following:

% HTTP.SSL.CAINFO=/etc/ssl/certs/ca-bundle.crt
% Then restart matlab and try running the program again.

% Refer to the readme for more information on the script.
% For questions about MATLAB commands and other features, refer to the
% MathWorks help center page at
% https://www.mathworks.com/help/index.html

% This script shows how the data from a netCDF file can be used to create a
% 3D plot to visualize the data.

% The script reads in a netCDF file either through a URL or as a saved 
% file. The user stores the desired parameters in variables. The script
% then takes a given location and stores it along with the entire area in the
% netCDF file. The file then plots the parameters for the entire area in
% the given time.

% The script shows an example of taking netCDF files and plotting the
% temperature for a given location. Note that this example is a guide and
% may need to be changed.

% The "disp" MATLAB command is used to display a message to the command 
% window.

disp("Begin matlab_sounding_nc_xyz.m")

% Create a variable for the opendap url or for the local file.

myFile1 = 'NCAR_SWEX2022_ISS2_RS41_v1_20220404_170007.nc';
myFile2 = 'NCAR_SWEX2022_ISS2_RS41_v1_20220317_221912.nc';
myFile3 = 'NCAR_SWEX2022_ISS2_RS41_v1_20220405_020004.nc';
myFile4 = 'NCAR_SWEX2022_ISS2_RS41_v1_20220406_020007.nc';
myFile5 = 'NCAR_SWEX2022_ISS2_RS41_v1_20220409_230219.nc';
myFile6 = 'NCAR_SWEX2022_ISS2_RS41_v1_20220413_170027.nc';
myFile7 = 'NCAR_SWEX2022_ISS2_RS41_v1_20220414_020003.nc';
myFile8 = 'NCAR_SWEX2022_ISS2_RS41_v1_20220417_170014.nc';
myFile9 = 'NCAR_SWEX2022_ISS2_RS41_v1_20220418_020014.nc';
myFile10 = 'NCAR_SWEX2022_ISS2_RS41_v1_20220513_020001.nc';

% Use the 'ncread' command to take the desired variables. For this example
% latitude, longitude, altitude, and air temperature were
% used.
lat1 = ncread(myFile1, 'lat');
long1 = ncread(myFile1, 'lon');
alt1 = ncread(myFile1, 'alt');
air_temp1 = ncread(myFile1, 'tdry');

lat2 = ncread(myFile2, 'lat');
long2 = ncread(myFile2, 'lon');
alt2 = ncread(myFile2, 'alt');
air_temp2 = ncread(myFile2, 'tdry');

lat3 = ncread(myFile3, 'lat');
long3 = ncread(myFile3, 'lon');
alt3 = ncread(myFile3, 'alt');
air_temp3 = ncread(myFile3, 'tdry');

lat4 = ncread(myFile4, 'lat');
long4 = ncread(myFile4, 'lon');
alt4 = ncread(myFile4, 'alt');
air_temp4 = ncread(myFile4, 'tdry');

lat5 = ncread(myFile5, 'lat');
long5 = ncread(myFile5, 'lon');
alt5 = ncread(myFile5, 'alt');
air_temp5 = ncread(myFile5, 'tdry');

lat6 = ncread(myFile6, 'lat');
long6 = ncread(myFile6, 'lon');
alt6 = ncread(myFile6, 'alt');
air_temp6 = ncread(myFile6, 'tdry');

lat7 = ncread(myFile7, 'lat');
long7 = ncread(myFile7, 'lon');
alt7 = ncread(myFile7, 'alt');
air_temp7 = ncread(myFile7, 'tdry');

lat8 = ncread(myFile8, 'lat');
long8 = ncread(myFile8, 'lon');
alt8 = ncread(myFile8, 'alt');
air_temp8 = ncread(myFile8, 'tdry');

lat9 = ncread(myFile9, 'lat');
long9 = ncread(myFile9, 'lon');
alt9 = ncread(myFile9, 'alt');
air_temp9 = ncread(myFile9, 'tdry');

lat10 = ncread(myFile10, 'lat');
long10 = ncread(myFile10, 'lon');
alt10 = ncread(myFile10, 'alt');
air_temp10 = ncread(myFile10, 'tdry');

% Plot the data with scatter plots.
% 
scatter3(lat1,long1,alt1,50,air_temp1,'filled');

% The text() function is used to add some context for each scatter plot.
% The FontSize and HorizontalAlignment were adjusted for clarity.
% The set() function is used to rotate the text for clarity.
t1 = text(lat1(4968), long1(4968), alt1(4968)+500, '04/04 17');
t1.FontSize = 10;
t1.HorizontalAlignment = "left";
set(t1,'Rotation',60);

figure(1)

hold on
scatter3(lat2,long2,alt2,50,air_temp2,'filled');
t2 = text(lat2(3767), long2(3767), alt2(3767)+500, '03/17 22');
t2.FontSize = 10;
t2.HorizontalAlignment = "left";
set(t2,'Rotation',60);

hold on

scatter3(lat3,long3,alt3,50,air_temp3,'filled');
t3 = text(lat3(4762), long3(4762), alt3(4762)+500, '04/05 02');
t3.FontSize = 10;
t3.HorizontalAlignment = "left";
set(t3,'Rotation',60);

hold on

scatter3(lat4,long4,alt4,50,air_temp4,'filled');
t4 = text(lat4(4811), long4(4811), alt4(4811)+500, '04/06 02');
t4.FontSize = 10;
t4.HorizontalAlignment = "left";
set(t4,'Rotation',60);

hold on

scatter3(lat5,long5,alt5,50,air_temp5,'filled');
t5 = text(lat5(4458), long5(4458), alt5(4458)+500, '04/09 23');
t5.FontSize = 10;
t5.HorizontalAlignment = "left";
set(t5,'Rotation',60);

hold on

scatter3(lat6,long6,alt6,50,air_temp6,'filled');
t6 = text(lat6(3524), long6(3524), alt6(3524)+500, '04/13 17');
t6.FontSize = 10;
t6.HorizontalAlignment = "left";
set(t6,'Rotation',60);

hold on

scatter3(lat7,long7,alt7,50,air_temp7,'filled');
t7 = text(lat7(3666), long7(3666), alt7(3666)+500, '04/14 02');
t7.FontSize = 10;
t7.HorizontalAlignment = "left";
set(t7,'Rotation',60);

hold on

scatter3(lat8,long8,alt8,50,air_temp8,'filled');
t8 = text(lat8(4081), long8(4081), alt8(4081)+500, '04/17 17');
t8.FontSize = 10;
t8.HorizontalAlignment = "left";
set(t8,'Rotation',60);

hold on

scatter3(lat9,long9,alt9,50,air_temp9,'filled');
t9 = text(lat9(4883), long9(4883), alt9(4883)+500, '04/18 02');
t9.FontSize = 10;
t9.HorizontalAlignment = "left";
set(t9,'Rotation',60);

hold on

scatter3(lat10,long10,alt10,50,air_temp10,'filled');
t10 = text(lat10(4787), long10(4787), alt10(4787)+500, '05/13 02');
t10.FontSize = 10;
t10.HorizontalAlignment = "left";
set(t10,'Rotation',60);

hold on

plot3(lat1(1),long1(1),alt1(1),'k.', 'MarkerSize', 50);

hold on

% Additional plot features.

grid on
shading interp
xlabel('latitude')
ylabel('longitude')
zlabel('altitude (m)')
title("SWEX Air Temperature ISS2 (34.56, -119.95) 2022 03/17 - 05/13")

c = colorbar;
set( c, 'YDir', 'reverse' );
c.Label.String = ([char(176) 'C']);

disp('End matlab_sounding_nc_xyz.m')