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

% This script shows how the data from a txt file can be used to create a
% 3D plot to visualize the data.

% The script reads in a txt file. The user stores the desired parameters in 
% variables. The script then takes location and temperature data.
% The file then plots the parameters for the entire area.

% The script shows an example of taking a txt file and plotting the
% temperature for a given location. Note that this example is a guide and
% may need to be changed.

% The "disp" MATLAB command is used to display a message to the command 
% window.

disp("Begin matlab_sounding_txt_xyz.m")

% Create a variable for local file. Use readtable() to get the data as
% an array of numbers.

myFile1 = readtable('Plattsburg.txt');
myFile2 = readtable('Gault.txt');
myFile3 = readtable('Jean.txt');
myFile4 = readtable('Sorel.txt');

% Create variables and get the temperature, latitude, longitude, and
% altitude from the columns in the txt file.

temp1 = table2array(myFile1(:,3));
lat1 = table2array(myFile1(:,12));
long1 = table2array(myFile1(:,11));
alt1 = table2array(myFile1(:,15));

temp2 = table2array(myFile2(:,3));
lat2 = table2array(myFile2(:,12));
long2 = table2array(myFile2(:,11));
alt2 = table2array(myFile2(:,15));

temp3 = table2array(myFile3(:,3));
lat3 = table2array(myFile3(:,12));
long3 = table2array(myFile3(:,11));
alt3 = table2array(myFile3(:,15));

temp4 = table2array(myFile4(:,3));
lat4 = table2array(myFile4(:,12));
long4 = table2array(myFile4(:,11));
alt4 = table2array(myFile4(:,15));

% % Plot the data as scatter plots.

scatter3(lat1,long1,alt1,50,temp1,'filled');

% The text() function is used to add some context for each scatter plot.
% The FontSize and HorizontalAlignment were adjusted for clarity.
% The set() function is used to rotate the text for clarity.
t1 = text(lat1(1479), long1(1479), alt1(1479)+500, 'Plattsburg');
t1.FontSize = 10;
t1.HorizontalAlignment = "left";
set(t1,'Rotation',60);

figure(1)

hold on

scatter3(lat2,long2,alt2,50,temp2,'filled');
t1 = text(lat2(4576), long2(4576), alt2(4576)+500, 'Gault');
t1.FontSize = 10;
t1.HorizontalAlignment = "left";
set(t1,'Rotation',60);

hold on

scatter3(lat3,long3,alt3,50,temp3,'filled');
t1 = text(lat3(4261), long3(4261), alt3(4261)+1000, 'Jean');
t1.FontSize = 10;
t1.HorizontalAlignment = "left";
set(t1,'Rotation',60);

hold on

scatter3(lat4,long4,alt4,50,temp4,'filled');
t1 = text(lat4(4303), long4(4303), alt4(4303)+500, 'Sorel');
t1.FontSize = 10;
t1.HorizontalAlignment = "left";
set(t1,'Rotation',60);

hold on

% Add a black dot to see the origin of each plot for clarity.

plot3(lat1(1),long1(1),alt1(1),'k.', 'MarkerSize', 50);
plot3(lat2(1),long2(1),alt2(1),'k.', 'MarkerSize', 50);
plot3(lat3(1),long3(1),alt3(1),'k.', 'MarkerSize', 50);
plot3(lat4(1),long4(1),alt4(1),'k.', 'MarkerSize', 50);

hold on

% Additional plot features.

grid on
shading interp
xlabel('latitude')
ylabel('longitude')
zlabel('altitude (m)')
title("WINTRE-MIX IOP10, March 12, 2022 02:00")

c = colorbar;
set( c, 'YDir', 'reverse' );
c.Label.String = ([char(176) 'C']);

disp('End matlab_sounding_txt_xyz.m')