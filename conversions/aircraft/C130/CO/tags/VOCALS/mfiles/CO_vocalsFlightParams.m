function [CalLimit,CalMax,ZeroLimit,before,after,DataAfterCal,ptsToAvg,tankCon,toffset,badCals,badZs]=CO_vocalsFlightParams(rafFile,Flight)
%
%CO_trexFlightParams
%
%Dialog box assigns Flight dependent variables:
%	*minimum concentration for CO calibration
%	*Enter maximum concentration for CO zero
%	*number of points before change in calibration status to remove
%	*number of points after change in calibration status to remove
%	*number of data points to remove after calibration
%
%The last three items flag removed data with '-99'

% prompt  = {'Enter minimum counts for CO calibration:',...
%       'Enter maximum counts for CO calibration:',...
%       'Enter maximum counts for CO zero:',...
%       'Enter number of points before change in calibration status to remove',...
%       'Enter number of points after change in calibration status to remove',...
%       'Enter number of data points to remove after calibration',...
%       'Enter number of data points to average for calibration curve fitting',...
%       'Enter calibration gas concentration (ppbv)',...
%       'Enter time offset in seconds (positive value moves CO earlier):'};
% title   = 'Inputs for CO status calculation';
lines= 1;

            before = 6;
            after = 6;
            DataAfterCal = 30;
            ptsToAvg = 30;
            tankCon = 198;
%             tankCon = 152.2;


             if(rafFile(20:22)=='VOC') %  proj
                switch char(Flight)
                    case 'gn01'
                        CalLimit= 115000;
                        CalMax = 190000;
                        ZeroLimit = 25000;
                        toffset = 0; % init START settings
                        badCals = [];
                        badZs = [];
            tankCon = 509;
                        tSlope=1.000;
                        tInt=0;
                    case 'gn02'
                        CalLimit= 115000;
                        CalMax = 190000;
                        ZeroLimit = 25000;
                        toffset = 0; % init START settings
                        badCals = [];
                        badZs = [];
            tankCon = 509;
                        tSlope=1.000;
                        tInt=0;
                    case 'gn03'
                        CalLimit= 115000;
                        CalMax = 190000;
                        ZeroLimit = 25000;
                        toffset = 0; % init START settings
                        badCals = [];
                        badZs = [];
            tankCon = 509;
                        tSlope=1.000;
                        tInt=0;
                    case 'gn04'
                        CalLimit= 115000;
                        CalMax = 190000;
                        ZeroLimit = 25000;
                        toffset = 0; % init START settings
                        badCals = [];
                        badZs = [];
            tankCon = 509;
                        tSlope=1.000;
                        tInt=0;
                    case 'gn05'
                        CalLimit= 115000;
                        CalMax = 190000;
                        ZeroLimit = 25000;
                        toffset = 0; % init START settings
                        badCals = [];
                        badZs = [];
            tankCon = 509;
                        tSlope=1.000;
                        tInt=0;
                    case 'gn06'
                        CalLimit= 115000;
                        CalMax = 190000;
                        ZeroLimit = 25000;
                        toffset = 0; % init START settings
                        badCals = [];
                        badZs = [];
            tankCon = 509;
                        tSlope=1.000;
                        tInt=0;
                    case 'gn07'
                        CalLimit= 115000;
                        CalMax = 190000;
                        ZeroLimit = 25000;
                        toffset = 0; % init START settings
                        badCals = [];
                        badZs = [];
                        tSlope=1.000;
                        tInt=0;
                    case 'gn08'
                        CalLimit= 40000;
                        CalMax = 80000;
                        ZeroLimit = 35000;
                        toffset = 0; % init START settings
                        badCals = [];
                        badZs = [];
                        tSlope=1.000;
                        tInt=0;
                    case 'gn09'
                        CalLimit= 40000;
                        CalMax = 80000;
                        ZeroLimit = 35000;
                        toffset = 0; % init START settings
                        badCals = [];
                        badZs = [];
                        tSlope=1.000;
                        tInt=0;
                    case 'tf02'
                        CalLimit= 115000;
                        CalMax = 190000;
                        ZeroLimit = 25000;
                        toffset = 77; % rf01 PASE settings
                        badCals = [];
                        badZs = [];
            tankCon = 509;
                        tSlope=1.000;
                        tInt=0;
                    case 'rf01'
                        CalLimit= 40000;
                        CalMax = 80000;
                        ZeroLimit = 35000;
                        toffset = 63355 - 62322; % rf01 START settings
                        badCals = [];
                        badZs = [];
                        tSlope=1.0001;
                        tInt=-5.6329;
                    case 'rf02'
                        CalLimit= 46000;
                        CalMax = 60000;
                        ZeroLimit = 11000;
                        toffset = -5; % rf02 vocals settings
                        badCals = [];
                        badZs = [];
                        tSlope=1.0;
                        tInt=0.0;
                    case 'rf03'
                        CalLimit= 46000;
                        CalMax = 60000;
                        ZeroLimit = 11000;
                        toffset = -5; % rf02 vocals settings
                        badCals = [];
                        badZs = [7];
                        tSlope=1.0;
                        tInt=0.0;
                    case 'rf04'
                        CalLimit= 46000;
                        CalMax = 60000;
                        ZeroLimit = 11000;
                        toffset = -5; % rf02 vocals settings
                        badCals = [];
                        badZs = [];
                        tSlope=1.0;
                        tInt=0.0;
                    case 'rf05'
                        CalLimit= 46000;
                        CalMax = 60000;
                        ZeroLimit = 11000;
                        toffset = -5; % rf02 vocals settings
                        badCals = [];
                        badZs = [];
                        tSlope=1.0;
                        tInt=0.0;
                    case 'rf06'
                        CalLimit= 46000;
                        CalMax = 60000;
                        ZeroLimit = 11000;
                        toffset = -5; % rf02 vocals settings
                        badCals = [];
                        badZs = [];
                        tSlope=1.0;
                        tInt=0.0;
                    case 'rf07'
                        CalLimit= 46000;
                        CalMax = 60000;
                        ZeroLimit = 11000;
                        toffset = -5; % rf02 vocals settings
                        badCals = [];
                        badZs = [];
                        tSlope=1.0;
                        tInt=0.0;
                    case 'rf08'
                        CalLimit= 46000;
                        CalMax = 60000;
                        ZeroLimit = 11000;
                        toffset = -5; % rf02 vocals settings
                        badCals = [];
                        badZs = [];
                        tSlope=1.0;
                        tInt=0.0;
                    case 'rf09'
                        CalLimit= 46000;
                        CalMax = 60000;
                        ZeroLimit = 11000;
                        toffset = -5; % rf02 vocals settings
                        badCals = [];
                        badZs = [];
                        tSlope=1.0;
                        tInt=0.0;
                    case 'rf10'
                        CalLimit= 46000;
                        CalMax = 60000;
                        ZeroLimit = 11000;
                        toffset = -5; % rf02 vocals settings
                        badCals = [];
                        badZs = [];
                        tSlope=1.0;
                        tInt=0.0;
                    case 'rf11'
                        CalLimit= 46000;
                        CalMax = 60000;
                        ZeroLimit = 11000;
                        toffset = -5; % rf02 vocals settings
                        badCals = [];
                        badZs = [];
                        tSlope=1.0;
                        tInt=0.0;
                    case 'rf12'
                        CalLimit= 46000;
                        CalMax = 60000;
                        ZeroLimit = 11000;
                        toffset = -5; % rf02 vocals settings
                        badCals = [];
                        badZs = [];
                        tSlope=1.0;
                        tInt=0.0;
                    case 'rf13'
                        CalLimit= 46000;
                        CalMax = 60000;
                        ZeroLimit = 11000;
                        toffset = -5; % rf02 vocals settings
                        badCals = [];
                        badZs = [];
                        tSlope=1.0;
                        tInt=0.0;
                    case 'rf14'
                        CalLimit= 46000;
                        CalMax = 60000;
                        ZeroLimit = 11000;
                        toffset = -5; % rf02 vocals settings
                        badCals = [];
                        badZs = [];
                        tSlope=1.0;
                        tInt=0.0;
                    otherwise
                        CalLimit= 46000;
                        CalMax = 60000;
                        ZeroLimit = 11000;
                        toffset = -5; % rf02 vocals settings
                        badCals = [];
                        badZs = [];
                        tSlope=1.0;
                        tInt=0.0;
                end
             end
             

            end
            
