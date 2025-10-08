%Authorship: NCAR Earth Observing Laboratory Data Management & Services Group
%Contact: eol-archive@ucar.edu

%Licensing: The code associated with this document is provided freely and openly. 
% Users are hereby granted a license to access and use this code, unless otherwise 
% stated, subject to the terms and conditions of the GNU Affero General Public 
% License 3.0 (AGPL-3.0; https://www.gnu.org/licenses/agpl-3.0.en.html). This 
% documentation and associated code are provided "as is” and are not supported. 
% By using or downloading this code, the user agrees to the terms and conditions 
% set forth in this document.

%Acknowledgment: This work was sponsored by the National Science Foundation. 
% This material is based upon work supported by the National Center for Atmospheric 
% Research, a major facility sponsored by the National Science Foundation and managed 
% by the University Corporation for Atmospheric Research. Any opinions, findings 
% and conclusions or recommendations expressed in this material do not necessarily 
% reflect the views of the National Science Foundation.

%NOTE:
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

% The sample_matlab_XYplot.m script is used for visualization of the data 
% file. The script takes in a netCDF file. The user puts in the netCDF file
% and can take variables from the file. The script shows how the data can be
% used to plot 2d plots for a given location.

% The script first reads in the netCDF file either through a URL or through
% a downloaded file. The user stores the wanted information as variables
% and stores the desired location information. The script then finds all of
% the desired information for the given location and plots the information.

% The script shows an example of taking an opendap url and plotting the
% temperature for a given location. Note that this example is a guide and
% may need to be changed.


% The "disp" MATLAB command is used to display a message to the command 
% window.

disp("Begin sample_matlab_XYplot.m")

% Create the variable to store the opendap url or for the local file. 

myFile='https://data.eol.ucar.edu/opendap/data/esop_95/hrly_sfc/ES95HRLY_950715.qcf';
%myFile='ES95HRLY_950715.qcf';

% Display the information in the netCDF file with 'ncdisp'.

%ncdisp(myFile)

% Read the date and time of the file.

% Use the 'ncread' command to take the desired variables. For this example
% date, time, network, station, and dew point and air temperature were
% used.

date = ncread(myFile, 'QCF.date_nominal');

% For this example, some processing was needed as the variables from the
% NetCDF file were taken in a weird way. In this example, the variables
% were 'char' and were in a matrix. The following code takes this matrix
% and reorganizes the data for later uses. These steps may not be
% necessary for every file. This was needed for non number type variables.

% Find the dimensions of the 'date' matrix.

[rows,cols] = size(date);

% Create an array of the type 'string'.

final_date = strings(cols,1);

% Repeat this for the time.

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

% Read the dew point and air temp.

dew_point = ncread(myFile, 'QCF.dew_point_temperature');
air_temp = ncread(myFile, 'QCF.air_temperature');

% Read in the network and repeat the process above.

Network =ncread(myFile, 'QCF.network_name');

[netrows,netcols] = size(Network);

final_net = strings(netcols,1);

c = 1;
while c <= cols
    final_net(c,1) = string([Network(:,c)].');
    c = c + 1;
end

final_net(:,1) = deblank(final_net(:,1));

% Read in the platform and repeat the process.

Station =ncread(myFile, 'QCF.platform_name');

[starows,stacols] = size(Station);

final_sta = strings(stacols,1);

c = 1;
while c <= cols
    final_sta(c,1) = string([Station(:,c)].');
    c = c + 1;
end

final_sta(:,1) = deblank(final_sta(:,1));

% Combine the network and platform to make the location.

location = final_net + final_sta;

% Remove the duplicate locations.

unique_location = unique(location);

% Use a for loop to repeat the process for any number of locations.

for a = 1

% Take the 'a' location and use it later in the loop.    
    
example_location = unique_location(a);
  
% The information in this file is organized by time. The following lines
% here gathers the information for the given location. The array "array"
% stores the indices where the chosen station information is located.
 
time_size = length(final_time);
array = zeros(time_size,1);
 
% A for loop is created to find every index for the given station. Note
% that MATLAB starts at 1 for counting.
 
inc = 1;
  
for i = 1:time_size
     
    % The script saves the current network, station, and occurrence for the
    % current iteration of the loop.
     
    current_station = location(i);
     
    % If the location information is the same as the current station, save
    % the index into the array. Note that "..." means continue the code on
    % a different line.
    
    if isequal(current_station, example_location)
         
         array(inc) = i;
         inc = inc + 1;
    end
 end
 
% The "array" variable contains the indices but also contains unnecessary
% zeros at the end. The following loop removes the ending zeros 
% Because the previous for loop icremented the "inc" one
% more than needed, this loop subtracts the "inc" by 1.
 
final_arr = zeros(inc-1,1);
for j = 1:(inc-1)
    final_arr(j) = array(j);
end

% With the given indices, gather the time, temperature, and dew point 
% temperature that correspond to the given location. 

final_temp = zeros(length(final_arr),1);
final_dew_temp = zeros(length(final_arr),1);
 
% The file contains values of -999 temp. Change these values to 'NaN' so it
% does not plot these values.

for k = 1:(length(final_arr))
    if air_temp(final_arr(k)) < -100
        final_temp(k) = NaN;
    end
    if air_temp(final_arr(k)) > -100
        final_temp(k) = air_temp(final_arr(k));
    end
    if dew_point(final_arr(k)) < -100
        final_dew_temp(k) = NaN;
    end
    if dew_point(final_arr(k)) > -100
        final_dew_temp(k) = dew_point(final_arr(k));
    end

end
 
% Plot the data. x axis is date time and y axis is temperature in Celsius.
 
figure(a)
plot(u_datetime,final_dew_temp,'b','DisplayName','dew point temperature')
hold on;
scatter(u_datetime,final_dew_temp,'b', 'DisplayName', 'dew point temperature')
hold on;
plot(u_datetime,final_temp,'r', 'DisplayName', 'air temperature')
hold on;
scatter(u_datetime,final_temp,'r', 'DisplayName', 'air temperature' )
 
% xlabel('date time ')
ylabel('°C')
%legend({'dew point temp', 'air temp'}, 'Location', 'northwest')
legend('Location', 'northwest')
title("GCIP/ESOP-95 Surface Temp UTC" + strjoin(split(example_location)))
% Label the x and y axes.
 
% The datetime axis can be limited by using the format:
 
% xlim(datetime([YYYY YYYY,[MM MM],[dd dd]))
 
% The xlabel can be formatted by using the line below
 
% xtickformat('dd-MMM-yyyy')
end
% ylim([14 30])
disp("End sample_matlab_XYplot.m")