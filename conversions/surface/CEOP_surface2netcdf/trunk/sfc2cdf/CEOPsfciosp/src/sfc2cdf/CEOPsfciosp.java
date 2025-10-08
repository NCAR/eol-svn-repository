/*
 * CEOPsfciosp.java
 *
 */
package sfc2cdf;

import ucar.ma2.*;

import ucar.nc2.*;
import ucar.nc2.iosp.*;
import ucar.nc2.util.CancelTask;
import ucar.nc2.FileWriter;

import ucar.unidata.io.RandomAccessFile;

import java.io.*;
import java.util.*;
import java.lang.String;
import java.text.ParseException;
import java.text.SimpleDateFormat;

import gnu.getopt.*;

import dmg.util.*;

/**
 * Read CEOP data files and convert to a NetCDF file.
 *
 * This program reads the CEOP Surface Meteorological and Radiation Dataset data
 * files and converts them to a standard netCDF format using CF Conventions for
 * the variable names.  This program utilizes the IOSP (Input/Output Service
 * Provider) interface from Unidata to create the netCDF files.  Each station will
 * be written to a separate netCDF file even though the CEOP data files may
 * contain more than one station.  The name of the output netCDF file is created
 * from the RHP, RSI, and station information for the data.
 *
 * This program will read through the data file until a different station is found.
 * Then, the netCDF file is created for the first station.  The data file is then
 * read from the start of the second station and continues in this manner until
 * the end of the CEOP data file has been reached.
 *
 * @author $Author: anstett $
 * @version $Revision: 1.0 $
 */
public class CEOPsfciosp extends AbstractIOServiceProvider {
   protected NetcdfFile ncfile;        /* output netCDF file */
   protected RandomAccessFile infile;  /* input CEOP data file */
   protected static String outdirname; /* output directory name */
// list of netCDF file names created from the CEOP data file
   protected static ArrayList<String> fileNames;
// data gathered from the CEOP data file name
   protected static String inrhp, inrsi, instartdate, inenddate, fileType;
   protected static String partialFileName;

   public boolean isValidFile(RandomAccessFile infile) throws IOException {

/**
 * Determine quickly whether the input file is of the CEOP surface type.  This
 * checks for slashes and a colon in particular columns where the date and time
 * are located and also checks for a newline character at the end of the
 * expected length of a data line.  The newline character check is to
 * distinguish between the formats of the 4 different CEOP formats, since all
 * the CEOP data files will have the date and time in the same spot in the file.
 * It also checks the suffix of the input file.
 */
      infile.seek(0);
      byte[] b = new byte[310];
      infile.read(b);
      String test = new String(b);
      return (test.substring(4, 5).equals("/") && test.substring(7, 8).equals("/") &&
            test.substring(10, 11).equals(" ") && test.substring(13, 14).equals(":") &&
            ((test.charAt(305) == '\n') || ((test.charAt(305) == '\r') &&
            (test.charAt(306) == '\n'))) && fileType.equals("sfc"));
   }

   /**
    * Arrays used to store the data for each variable in the netcdf file.
    */
   private ArrayDouble.D1 timeArray, timeNominalArray, dphofluxArray, uphofluxArray, shArray;
   private ArrayDouble.D0 latArray, lonArray, altArray;
   private ArrayDouble.D1 pressArray, tempArray, dpArray, rhArray, wspdArray;
   private ArrayDouble.D1 wdirArray, eastwArray, northwArray, precipArray, snowArray;
   private ArrayDouble.D1 dshortfluxArray, ushortfluxArray, dlongfluxArray, ulongfluxArray;
   private ArrayDouble.D1 radArray, surftArray;
   private ArrayChar.D1 pflagArray, tflagArray, dpflagArray, rhflagArray, shflagArray;
   private ArrayChar.D1 wsflagArray, wdflagArray, ewflagArray, nwflagArray;
   private ArrayChar.D1 paflagArray, snowflagArray, dsfflagArray, usfflagArray;
   private ArrayChar.D1 dlfflagArray, ulfflagArray, radflagArray, stflagArray;
   private ArrayChar.D1 dpfflagArray, upfflagArray;
//  Data saved for the global attributes.
   private String rhp, rsi, station;
// Data saved if previous values found in data to create the comments in the header.
   private String previousRhp, previousRsi;
   private String previousRhpDate, previousRsiDate;
   private Double fillValue;  /* missing value for the variables. */
// Start and end times saved for the global attributes in the netCDF file.
   private String startDateTime, endDateTime;
// Needed for formatting the dates for the netCDF file.
   private SimpleDateFormat sdf;

/**
 * This routine opens the CEOP data file, reads the data and creates the netCDF
 * files.  This routine catches any parse exceptions when the String data is
 * parsed into a number and also catches any conversion exception that may occur
 * when converting values between different units (i.e. converting temperatures
 * from Kelvin to Celsius).
 *
 * @param infile  - CEOP surface input data file
 * @param ncfile  - CEOP surface output netCDF file
 * @param cancelTask - required for the IOSP
 * @throws IOException if there is a problem with either the input or output files.
 */

   public void open(RandomAccessFile infile, NetcdfFile ncfile, CancelTask cancelTask)
         throws IOException {

      this.infile = infile;
      this.ncfile = ncfile;
      fillValue = -999.99;
      fileNames = new ArrayList<String>();
      sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss Z");
      sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
      try {
         readAllData(infile);
      } catch (ParseException e) {
         e.printStackTrace();
         throw new IOException("CEOPsfciosp: Parse exception occurred.");
      } catch (ConversionException e) {
         e.printStackTrace();
         throw new IOException("CEOPsfciosp: Invalid conversion attempted.");
      }
   }

   /**
    * This routine is called after each station has been read from the CEOP data file.
    * It writes out the netCDF file for that station.
    *
    * @param numRecords  number of different times for the current station.
    * @throws IOException if there is a problem writing to the netCDF file.
    */
   void createOutputFile(int numRecords) throws IOException {

      String outfilename;
      int nameLength = 30;
      int[] shapeChar = new int[]{nameLength};
      ArrayChar.D1 idArray = (ArrayChar.D1) Array.factory(DataType.CHAR, shapeChar);

// Create output netCDF file name and add the name to the list of filenames created.
      idArray.setString(station);
      outfilename = rhp + "_" + rsi + "_" + station + "_sfc.nc";
      if (outdirname != null) {
         outfilename = outdirname + "/" + outfilename;
      }     
      fileNames.add(outfilename);
      String date = sdf.format(Calendar.getInstance().getTime());
// Create global attributes for the netCDF file.
      ncfile.addAttribute(null, new Attribute("Conventions", "CF-1.6-beta"));
      ncfile.addAttribute(null, new Attribute("Conventions_Comment", 
            "Used an incomplete draft of the future version CF-1.6 for the time series representation."));
      ncfile.addAttribute(null, new Attribute("title",
            "CEOP Surface Meteorological and Radiation Dataset"));
      ncfile.addAttribute(null, new Attribute("RHP_Identifier_(formerly_known_as_CSE)", rhp));
      if (previousRhp != null) {
         ncfile.addAttribute(null, new Attribute("Previous_RHP_Identifier", 
            previousRhp + " until " + previousRhpDate));
      }
      ncfile.addAttribute(null, new Attribute("Reference_Site_Identifier", rsi));
      if (previousRsi != null) {
         ncfile.addAttribute(null, new Attribute("Previous_RSI_Identifier", 
            previousRsi + " until " + previousRsiDate));
      }
      ncfile.addAttribute(null, new Attribute("Station_Identifier", station));     
      ncfile.addAttribute(null, new Attribute("Data_Type", "sfc"));
      ncfile.addAttribute(null, new Attribute("Data_Type_Long_Name", "Surface"));
      ncfile.addAttribute(null, new Attribute("project",
            "Coordinated Energy and Water Cycle Observation Project"));
      ncfile.addAttribute(null, new Attribute("acknowledgment",
            "NCAR/EOL Reference Site Data activities and coordination supported by NOAA Climate Program Office (CPO)"));
      ncfile.addAttribute(null, new Attribute("date_created", date));
      ncfile.addAttribute(null, new Attribute("institution",
            "National Center for Atmospheric Research - Earth Observing Laboratory"));
      ncfile.addAttribute(null, new Attribute("creator_url",
            "http://www.eol.ucar.edu"));
      ncfile.addAttribute(null, new Attribute("creator_email", "codiac at ucar dot edu"));
      ncfile.addAttribute(null, new Attribute("observationDimension", "time"));
      ncfile.addAttribute(null, new Attribute("time_coverage_start", startDateTime));
      ncfile.addAttribute(null, new Attribute("time_coverage_end", endDateTime));
      ncfile.addAttribute(null, new Attribute("featureType", "timeSeries"));

// Create time and name string length dimensions for netcdf file.
      Dimension timeDim = new Dimension("time", numRecords, true);
      ncfile.addDimension(null, timeDim);
      Dimension nameLengthDim = new Dimension("name_strlen", nameLength, true);
      ncfile.addDimension(null, nameLengthDim);

// Create all the variables in the netCDF file.
      Variable longitude = new Variable(ncfile, null, null, "longitude");
      longitude.setDimensions("");
      longitude.setDataType(DataType.DOUBLE);
      longitude.addAttribute(new Attribute("standard_name", "longitude"));
      longitude.addAttribute(new Attribute("long_name", "Station Longitude"));
      longitude.addAttribute(new Attribute("units", "degrees_east"));
      longitude.setCachedData(lonArray, false);
      ncfile.addVariable(null, longitude);
      Variable latitude = new Variable(ncfile, null, null, "latitude");
      latitude.setDimensions("");
      latitude.setDataType(DataType.DOUBLE);
      latitude.addAttribute(new Attribute("standard_name", "latitude"));
      latitude.addAttribute(new Attribute("long_name", "Station Latitude"));
      latitude.addAttribute(new Attribute("units", "degrees_north"));
      latitude.setCachedData(latArray, false);
      ncfile.addVariable(null, latitude);
      Variable altitude = new Variable(ncfile, null, null, "altitude");
      altitude.setDimensions("");
      altitude.setDataType(DataType.DOUBLE);
      altitude.addAttribute(new Attribute("standard_name", "altitude"));
      altitude.addAttribute(new Attribute("long_name", "Station Altitude"));
      altitude.addAttribute(new Attribute("units", "m"));
      altitude.addAttribute(new Attribute("positive", "up"));
      altitude.setCachedData(altArray, false);
      ncfile.addVariable(null, altitude);
      Variable station_name = new Variable(ncfile, null, null, "station_name");
      station_name.setDimensions("name_strlen");
      station_name.setDataType(DataType.CHAR);
      station_name.addAttribute(new Attribute("standard_name", "station_name"));
      station_name.addAttribute(new Attribute("long_name", "Station Name"));
      station_name.addAttribute(new Attribute("cf_role", "timeseries_id"));
      station_name.setCachedData(idArray, false);
      ncfile.addVariable(null, station_name);
      Variable time = new Variable(ncfile, null, null, "time");
      time.setDimensions("time");
      time.setDataType(DataType.DOUBLE);
      time.addAttribute(new Attribute("standard_name", "time"));
      time.addAttribute(new Attribute("long_name", "UTC Actual Date/Time"));
      time.addAttribute(new Attribute("units", "seconds since 1970-01-01 00:00:00"));
      time.addAttribute(new Attribute("calendar", "gregorian"));
      time.setCachedData(timeArray, false);
      ncfile.addVariable(null, time);
      Variable timeNominal = new Variable(ncfile, null, null, "time_nominal");
      timeNominal.setDimensions("time");
      timeNominal.setDataType(DataType.DOUBLE);
      timeNominal.addAttribute(new Attribute("standard_name", "time_nominal"));
      timeNominal.addAttribute(new Attribute("long_name", "UTC Nominal Date/Time"));
      timeNominal.addAttribute(new Attribute("units",
            "seconds since 1970-01-01 00:00:00"));
      timeNominal.setCachedData(timeNominalArray, false);
      ncfile.addVariable(null, timeNominal);
      Variable press = new Variable(ncfile, null, null, "surface_air_pressure");
      press.setDimensions("time");
      press.setDataType(DataType.DOUBLE);
      press.addAttribute(new Attribute("standard_name", "surface_air_pressure"));
      press.addAttribute(new Attribute("long_name", "Station Pressure"));
      press.addAttribute(new Attribute("units", "Pa"));
      press.addAttribute(new Attribute("coordinates", "time longitude latitude altitude"));
      press.addAttribute(new Attribute("ancillary_variables", "surface_air_pressure_flag"));
      press.addAttribute(new Attribute("_FillValue", fillValue));
      press.addAttribute(new Attribute("missing_value", fillValue));
      press.setCachedData(pressArray, false);
      ncfile.addVariable(null, press);
      Variable pressflag = new Variable(ncfile, null, null,
            "surface_air_pressure_flag");
      pressflag.setDimensions("time");
      pressflag.setDataType(DataType.CHAR);
      pressflag.addAttribute(new Attribute("standard_name",
            "surface_air_pressure status_flag"));
      pressflag.addAttribute(new Attribute("long_name", "Station Pressure Flag"));
      pressflag.addAttribute(new Attribute("flag_values", "C M B I D G U"));
      pressflag.addAttribute(new Attribute("flag_meanings",
          "exceeds_field_size missing bad interpolated_or_estimated_or_gap_filled questionable good unchecked"));
      pressflag.addAttribute(new Attribute("coordinates", "time longitude latitude altitude"));
      pressflag.addAttribute(new Attribute("_FillValue", "M"));
      pressflag.addAttribute(new Attribute("missing_value", "M"));
      pressflag.setCachedData(pflagArray, false);
      ncfile.addVariable(null, pressflag);
      Variable airt = new Variable(ncfile, null, null, "air_temperature");
      airt.setDimensions("time");
      airt.setDataType(DataType.DOUBLE);
      airt.addAttribute(new Attribute("standard_name", "air_temperature"));
      airt.addAttribute(new Attribute("long_name", "Air Temperature"));
      airt.addAttribute(new Attribute("units", "K"));
      airt.addAttribute(new Attribute("coordinates", "time longitude latitude altitude"));
      airt.addAttribute(new Attribute("ancillary_variables", "air_temperature_flag"));
      airt.addAttribute(new Attribute("_FillValue", fillValue));
      airt.addAttribute(new Attribute("missing_value", fillValue));
      airt.setCachedData(tempArray, false);
      ncfile.addVariable(null, airt);
      Variable atflag = new Variable(ncfile, null, null, "air_temperature_flag");
      atflag.setDimensions("time");
      atflag.setDataType(DataType.CHAR);
      atflag.addAttribute(new Attribute("standard_name", "air_temperature status_flag"));
      atflag.addAttribute(new Attribute("long_name", "Air Temperature Flag"));
      atflag.addAttribute(new Attribute("flag_values", "C M B I D G U"));
      atflag.addAttribute(new Attribute("flag_meanings",
          "exceeds_field_size missing bad interpolated_or_estimated_or_gap_filled questionable good unchecked"));
      atflag.addAttribute(new Attribute("coordinates", "time longitude latitude altitude"));
      atflag.addAttribute(new Attribute("_FillValue", "M"));
      atflag.addAttribute(new Attribute("missing_value", "M"));
      atflag.setCachedData(tflagArray, false);
      ncfile.addVariable(null, atflag);
      Variable dp = new Variable(ncfile, null, null, "dew_point_temperature");
      dp.setDimensions("time");
      dp.setDataType(DataType.DOUBLE);
      dp.addAttribute(new Attribute("standard_name", "dew_point_temperature"));
      dp.addAttribute(new Attribute("long_name", "Dew Point Temperature"));
      dp.addAttribute(new Attribute("units", "K"));
      dp.addAttribute(new Attribute("coordinates", "time longitude latitude altitude"));
      dp.addAttribute(new Attribute("ancillary_variables", "dew_point_temperature_flag"));
      dp.addAttribute(new Attribute("_FillValue", fillValue));
      dp.addAttribute(new Attribute("missing_value", fillValue));
      dp.setCachedData(dpArray, false);
      ncfile.addVariable(null, dp);
      Variable dpflag = new Variable(ncfile, null, null,
            "dew_point_temperature_flag");
      dpflag.setDimensions("time");
      dpflag.setDataType(DataType.CHAR);
      dpflag.addAttribute(new Attribute("standard_name",
            "dew_point_temperature status_flag"));
      dpflag.addAttribute(new Attribute("long_name", "Dew Point Temperature Flag"));
      dpflag.addAttribute(new Attribute("flag_values", "C M B I D G U"));
      dpflag.addAttribute(new Attribute("flag_meanings",
          "exceeds_field_size missing bad interpolated_or_estimated_or_gap_filled questionable good unchecked"));
      dpflag.addAttribute(new Attribute("coordinates", "time longitude latitude altitude"));
      dpflag.addAttribute(new Attribute("_FillValue", "M"));
      dpflag.addAttribute(new Attribute("missing_value", "M"));
      dpflag.setCachedData(dpflagArray, false);
      ncfile.addVariable(null, dpflag);
      Variable rh = new Variable(ncfile, null, null, "relative_humidity");
      rh.setDimensions("time");
      rh.setDataType(DataType.DOUBLE);
      rh.addAttribute(new Attribute("standard_name", "relative_humidity"));
      rh.addAttribute(new Attribute("long_name", "Relative Humidity"));
      rh.addAttribute(new Attribute("units", "percent"));
      rh.addAttribute(new Attribute("coordinates", "time longitude latitude altitude"));
      rh.addAttribute(new Attribute("ancillary_variables", "relative_humidity_flag"));
      rh.addAttribute(new Attribute("_FillValue", fillValue));
      rh.addAttribute(new Attribute("missing_value", fillValue));
      rh.setCachedData(rhArray, false);
      ncfile.addVariable(null, rh);
      Variable rhflag = new Variable(ncfile, null, null, "relative_humidity_flag");
      rhflag.setDimensions("time");
      rhflag.setDataType(DataType.CHAR);
      rhflag.addAttribute(new Attribute("standard_name", "relative_humidity status_flag"));
      rhflag.addAttribute(new Attribute("long_name", "Relative Humidity Flag"));
      rhflag.addAttribute(new Attribute("flag_values", "C M B I D G U"));
      rhflag.addAttribute(new Attribute("flag_meanings",
          "exceeds_field_size missing bad interpolated_or_estimated_or_gap_filled questionable good unchecked"));
      rhflag.addAttribute(new Attribute("coordinates", "time longitude latitude altitude"));
      rhflag.addAttribute(new Attribute("_FillValue", "M"));
      rhflag.addAttribute(new Attribute("missing_value", "M"));
      rhflag.setCachedData(rhflagArray, false);
      ncfile.addVariable(null, rhflag);
      Variable sh = new Variable(ncfile, null, null, "specific_humidity");
      sh.setDimensions("time");
      sh.setDataType(DataType.DOUBLE);
      sh.addAttribute(new Attribute("standard_name", "specific_humidity"));
      sh.addAttribute(new Attribute("long_name", "Specific Humidity"));
      sh.addAttribute(new Attribute("units", "kg/kg"));
      sh.addAttribute(new Attribute("coordinates", "time longitude latitude altitude"));
      sh.addAttribute(new Attribute("ancillary_variables", "specific_humidity_flag"));
      sh.addAttribute(new Attribute("_FillValue", fillValue));
      sh.addAttribute(new Attribute("missing_value", fillValue));
      sh.setCachedData(shArray, false);
      ncfile.addVariable(null, sh);
      Variable shflag = new Variable(ncfile, null, null, "specific_humidity_flag");
      shflag.setDimensions("time");
      shflag.setDataType(DataType.CHAR);
      shflag.addAttribute(new Attribute("standard_name", "specific_humidity status_flag"));
      shflag.addAttribute(new Attribute("long_name", "Specific Humidity Flag"));
      shflag.addAttribute(new Attribute("flag_values", "C M B I D G U"));
      shflag.addAttribute(new Attribute("flag_meanings",
          "exceeds_field_size missing bad interpolated_or_estimated_or_gap_filled questionable good unchecked"));
      shflag.addAttribute(new Attribute("coordinates", "time longitude latitude altitude"));
      shflag.addAttribute(new Attribute("_FillValue", "M"));
      shflag.addAttribute(new Attribute("missing_value", "M"));
      shflag.setCachedData(shflagArray, false);
      ncfile.addVariable(null, shflag);
      Variable ws = new Variable(ncfile, null, null, "wind_speed");
      ws.setDimensions("time");
      ws.setDataType(DataType.DOUBLE);
      ws.addAttribute(new Attribute("standard_name", "wind_speed"));
      ws.addAttribute(new Attribute("long_name", "Wind Speed"));
      ws.addAttribute(new Attribute("units", "m/s"));
      ws.addAttribute(new Attribute("coordinates", "time longitude latitude altitude"));
      ws.addAttribute(new Attribute("ancillary_variables", "wind_speed_flag"));
      ws.addAttribute(new Attribute("_FillValue", fillValue));
      ws.addAttribute(new Attribute("missing_value", fillValue));
      ws.setCachedData(wspdArray, false);
      ncfile.addVariable(null, ws);
      Variable wsflag = new Variable(ncfile, null, null, "wind_speed_flag");
      wsflag.setDimensions("time");
      wsflag.setDataType(DataType.CHAR);
      wsflag.addAttribute(new Attribute("standard_name", "wind_speed status_flag"));
      wsflag.addAttribute(new Attribute("long_name", "Wind Speed Flag"));
      wsflag.addAttribute(new Attribute("flag_values", "C M B I D G U"));
      wsflag.addAttribute(new Attribute("flag_meanings",
          "exceeds_field_size missing bad interpolated_or_estimated_or_gap_filled questionable good unchecked"));
      wsflag.addAttribute(new Attribute("coordinates", "time longitude latitude altitude"));
      wsflag.addAttribute(new Attribute("_FillValue", "M"));
      wsflag.addAttribute(new Attribute("missing_value", "M"));
      wsflag.setCachedData(wsflagArray, false);
      ncfile.addVariable(null, wsflag);
      Variable wdir = new Variable(ncfile, null, null, "wind_from_direction");
      wdir.setDimensions("time");
      wdir.setDataType(DataType.DOUBLE);
      wdir.addAttribute(new Attribute("standard_name", "wind_from_direction"));
      wdir.addAttribute(new Attribute("long_name", "Wind Direction"));
      wdir.addAttribute(new Attribute("units", "degrees"));
      wdir.addAttribute(new Attribute("coordinates", "time longitude latitude altitude"));
      wdir.addAttribute(new Attribute("ancillary_variables", "wind_from_direction_flag"));
      wdir.addAttribute(new Attribute("_FillValue", fillValue));
      wdir.addAttribute(new Attribute("missing_value", fillValue));
      wdir.setCachedData(wdirArray, false);
      ncfile.addVariable(null, wdir);
      Variable wdflag = new Variable(ncfile, null, null, "wind_from_direction_flag");
      wdflag.setDimensions("time");
      wdflag.setDataType(DataType.CHAR);
      wdflag.addAttribute(new Attribute("standard_name", "wind_from_direction status_flag"));
      wdflag.addAttribute(new Attribute("long_name", "Wind Direction Flag"));
      wdflag.addAttribute(new Attribute("flag_values", "C M B I D G U"));
      wdflag.addAttribute(new Attribute("flag_meanings",
          "exceeds_field_size missing bad interpolated_or_estimated_or_gap_filled questionable good unchecked"));
      wdflag.addAttribute(new Attribute("coordinates", "time longitude latitude altitude"));
      wdflag.addAttribute(new Attribute("_FillValue", "M"));
      wdflag.addAttribute(new Attribute("missing_value", "M"));
      wdflag.setCachedData(wdflagArray, false);
      ncfile.addVariable(null, wdflag);
      Variable eastw = new Variable(ncfile, null, null, "eastward_wind");
      eastw.setDimensions("time");
      eastw.setDataType(DataType.DOUBLE);
      eastw.addAttribute(new Attribute("standard_name", "eastward_wind"));
      eastw.addAttribute(new Attribute("long_name", "U Wind Component"));
      eastw.addAttribute(new Attribute("units", "m/s"));
      eastw.addAttribute(new Attribute("coordinates", "time longitude latitude altitude"));
      eastw.addAttribute(new Attribute("ancillary_variables", "eastward_wind_flag"));
      eastw.addAttribute(new Attribute("_FillValue", fillValue));
      eastw.addAttribute(new Attribute("missing_value", fillValue));
      eastw.setCachedData(eastwArray, false);
      ncfile.addVariable(null, eastw);
      Variable ewflag = new Variable(ncfile, null, null, "eastward_wind_flag");
      ewflag.setDimensions("time");
      ewflag.setDataType(DataType.CHAR);
      ewflag.addAttribute(new Attribute("standard_name", "eastward_wind status_flag"));
      ewflag.addAttribute(new Attribute("long_name", "U Wind Component Flag"));
      ewflag.addAttribute(new Attribute("flag_values", "C M B I D G U"));
      ewflag.addAttribute(new Attribute("flag_meanings",
          "exceeds_field_size missing bad interpolated_or_estimated_or_gap_filled questionable good unchecked"));
      ewflag.addAttribute(new Attribute("coordinates", "time longitude latitude altitude"));
      ewflag.addAttribute(new Attribute("_FillValue", "M"));
      ewflag.addAttribute(new Attribute("missing_value", "M"));
      ewflag.setCachedData(ewflagArray, false);
      ncfile.addVariable(null, ewflag);
      Variable northw = new Variable(ncfile, null, null, "northward_wind");
      northw.setDimensions("time");
      northw.setDataType(DataType.DOUBLE);
      northw.addAttribute(new Attribute("standard_name", "northward_wind"));
      northw.addAttribute(new Attribute("long_name", "V Wind Component"));
      northw.addAttribute(new Attribute("units", "m/s"));
      northw.addAttribute(new Attribute("coordinates", "time longitude latitude altitude"));
      northw.addAttribute(new Attribute("ancillary_variables", "northward_wind_flag"));
      northw.addAttribute(new Attribute("_FillValue", fillValue));
      northw.addAttribute(new Attribute("missing_value", fillValue));
      northw.setCachedData(northwArray, false);
      ncfile.addVariable(null, northw);
      Variable nwflag = new Variable(ncfile, null, null, "northward_wind_flag");
      nwflag.setDimensions("time");
      nwflag.setDataType(DataType.CHAR);
      nwflag.addAttribute(new Attribute("standard_name", "northward_wind status_flag"));
      nwflag.addAttribute(new Attribute("long_name", "V Wind Component Flag"));
      nwflag.addAttribute(new Attribute("flag_values", "C M B I D G U"));
      nwflag.addAttribute(new Attribute("flag_meanings",
          "exceeds_field_size missing bad interpolated_or_estimated_or_gap_filled questionable good unchecked"));
      nwflag.addAttribute(new Attribute("coordinates", "time longitude latitude altitude"));
      nwflag.addAttribute(new Attribute("_FillValue", "M"));
      nwflag.addAttribute(new Attribute("missing_value", "M"));
      nwflag.setCachedData(nwflagArray, false);
      ncfile.addVariable(null, nwflag);
      Variable precip = new Variable(ncfile, null, null, "precipitation_amount");
      precip.setDimensions("time");
      precip.setDataType(DataType.DOUBLE);
      precip.addAttribute(new Attribute("standard_name", "precipitation_amount"));
      precip.addAttribute(new Attribute("long_name", "Precipitation"));
      precip.addAttribute(new Attribute("units", "kg/m2"));
      precip.addAttribute(new Attribute("coordinates", "time longitude latitude altitude"));
      precip.addAttribute(new Attribute("ancillary_variables", "precipitation_amount_flag"));
      precip.addAttribute(new Attribute("_FillValue", fillValue));
      precip.addAttribute(new Attribute("missing_value", fillValue));
      precip.setCachedData(precipArray, false);
      ncfile.addVariable(null, precip);
      Variable paflag = new Variable(ncfile, null, null, "precipitation_amount_flag");
      paflag.setDimensions("time");
      paflag.setDataType(DataType.CHAR);
      paflag.addAttribute(new Attribute("standard_name",
            "precipitation_amount status_flag"));
      paflag.addAttribute(new Attribute("long_name", "Precipitation Flag"));
      paflag.addAttribute(new Attribute("flag_values", "C M B I D G U"));
      paflag.addAttribute(new Attribute("flag_meanings",
          "exceeds_field_size missing bad interpolated_or_estimated_or_gap_filled questionable good unchecked"));
      paflag.addAttribute(new Attribute("coordinates", "time longitude latitude altitude"));
      paflag.addAttribute(new Attribute("_FillValue", "M"));
      paflag.addAttribute(new Attribute("missing_value", "M"));
      paflag.setCachedData(paflagArray, false);
      ncfile.addVariable(null, paflag);
      Variable snow = new Variable(ncfile, null, null, "surface_snow_thickness");
      snow.setDimensions("time");
      snow.setDataType(DataType.DOUBLE);
      snow.addAttribute(new Attribute("standard_name", "surface_snow_thickness"));
      snow.addAttribute(new Attribute("long_name", "Snow Depth"));
      snow.addAttribute(new Attribute("units", "m"));
      snow.addAttribute(new Attribute("coordinates", "time longitude latitude altitude"));
      snow.addAttribute(new Attribute("ancillary_variables", "surface_snow_thickness_flag"));
      snow.addAttribute(new Attribute("_FillValue", fillValue));
      snow.addAttribute(new Attribute("missing_value", fillValue));
      snow.setCachedData(snowArray, false);
      ncfile.addVariable(null, snow);
      Variable sflag = new Variable(ncfile, null, null,
            "surface_snow_thickness_flag");
      sflag.setDimensions("time");
      sflag.setDataType(DataType.CHAR);
      sflag.addAttribute(new Attribute("standard_name",
            "surface_snow_thickness status_flag"));
      sflag.addAttribute(new Attribute("long_name", "Snow Depth Flag"));
      sflag.addAttribute(new Attribute("flag_values", "C M B I D G U"));
      sflag.addAttribute(new Attribute("flag_meanings",
          "exceeds_field_size missing bad interpolated_or_estimated_or_gap_filled questionable good unchecked"));
      sflag.addAttribute(new Attribute("coordinates", "time longitude latitude altitude"));
      sflag.addAttribute(new Attribute("_FillValue", "M"));
      sflag.addAttribute(new Attribute("missing_value", "M"));
      sflag.setCachedData(snowflagArray, false);
      ncfile.addVariable(null, sflag);
      Variable dshort = new Variable(ncfile, null, null,
            "surface_downwelling_shortwave_flux_in_air");
      dshort.setDimensions("time");
      dshort.setDataType(DataType.DOUBLE);
      dshort.addAttribute(new Attribute("standard_name",
            "surface_downwelling_shortwave_flux_in_air"));
      dshort.addAttribute(new Attribute("long_name", "Incoming Shortwave"));
      dshort.addAttribute(new Attribute("units", "W/m2"));
      dshort.addAttribute(new Attribute("coordinates", "time longitude latitude altitude"));
      dshort.addAttribute(new Attribute("ancillary_variables",
            "surface_downwelling_shortwave_flux_in_air_flag"));
      dshort.addAttribute(new Attribute("_FillValue", fillValue));
      dshort.addAttribute(new Attribute("missing_value", fillValue));
      dshort.setCachedData(dshortfluxArray, false);
      ncfile.addVariable(null, dshort);
      Variable dsflag = new Variable(ncfile, null, null,
            "surface_downwelling_shortwave_flux_in_air_flag");
      dsflag.setDimensions("time");
      dsflag.setDataType(DataType.CHAR);
      dsflag.addAttribute(new Attribute("standard_name",
            "surface_downwelling_shortwave_flux_in_air status_flag"));
      dsflag.addAttribute(new Attribute("long_name", "Incoming Shortwave Flag"));
      dsflag.addAttribute(new Attribute("flag_values", "C M B I D G U"));
      dsflag.addAttribute(new Attribute("flag_meanings",
          "exceeds_field_size missing bad interpolated_or_estimated_or_gap_filled questionable good unchecked"));
      dsflag.addAttribute(new Attribute("coordinates", "time longitude latitude altitude"));
      dsflag.addAttribute(new Attribute("_FillValue", "M"));
      dsflag.addAttribute(new Attribute("missing_value", "M"));
      dsflag.setCachedData(dsfflagArray, false);
      ncfile.addVariable(null, dsflag);
      Variable ushort = new Variable(ncfile, null, null,
            "surface_upwelling_shortwave_flux_in_air");
      ushort.setDimensions("time");
      ushort.setDataType(DataType.DOUBLE);
      ushort.addAttribute(new Attribute("standard_name",
            "surface_upwelling_shortwave_flux_in_air"));
      ushort.addAttribute(new Attribute("long_name", "Outgoing Shortwave"));
      ushort.addAttribute(new Attribute("units", "W/m2"));
      ushort.addAttribute(new Attribute("coordinates", "time longitude latitude altitude"));
      ushort.addAttribute(new Attribute("ancillary_variables",
            "surface_upwelling_shortwave_flux_in_air_flag"));
      ushort.addAttribute(new Attribute("_FillValue", fillValue));
      ushort.addAttribute(new Attribute("missing_value", fillValue));
      ushort.setCachedData(ushortfluxArray, false);
      ncfile.addVariable(null, ushort);
      Variable usflag = new Variable(ncfile, null, null,
            "surface_upwelling_shortwave_flux_in_air_flag");
      usflag.setDimensions("time");
      usflag.setDataType(DataType.CHAR);
      usflag.addAttribute(new Attribute("standard_name",
            "surface_upwelling_shortwave_flux_in_air status_flag"));
      usflag.addAttribute(new Attribute("long_name", "Outgoing Shortwave Flag"));
      usflag.addAttribute(new Attribute("flag_values", "C M B I D G U"));
      usflag.addAttribute(new Attribute("flag_meanings",
          "exceeds_field_size missing bad interpolated_or_estimated_or_gap_filled questionable good unchecked"));
      usflag.addAttribute(new Attribute("coordinates", "time longitude latitude altitude"));
      usflag.addAttribute(new Attribute("_FillValue", "M"));
      usflag.addAttribute(new Attribute("missing_value", "M"));
      usflag.setCachedData(usfflagArray, false);
      ncfile.addVariable(null, usflag);
      Variable dlong = new Variable(ncfile, null, null,
            "surface_downwelling_longwave_flux_in_air");
      dlong.setDimensions("time");
      dlong.setDataType(DataType.DOUBLE);
      dlong.addAttribute(new Attribute("standard_name",
            "surface_downwelling_longwave_flux_in_air"));
      dlong.addAttribute(new Attribute("long_name", "Incoming Longwave"));
      dlong.addAttribute(new Attribute("units", "W/m2"));
      dlong.addAttribute(new Attribute("coordinates", "time longitude latitude altitude"));
      dlong.addAttribute(new Attribute("ancillary_variables",
            "surface_downwelling_longwave_flux_in_air_flag"));
      dlong.addAttribute(new Attribute("_FillValue", fillValue));
      dlong.addAttribute(new Attribute("missing_value", fillValue));
      dlong.setCachedData(dlongfluxArray, false);
      ncfile.addVariable(null, dlong);
      Variable dlflag = new Variable(ncfile, null, null,
            "surface_downwelling_longwave_flux_in_air_flag");
      dlflag.setDimensions("time");
      dlflag.setDataType(DataType.CHAR);
      dlflag.addAttribute(new Attribute("standard_name",
            "surface_downwelling_longwave_flux_in_air status_flag"));
      dlflag.addAttribute(new Attribute("long_name", "Incoming Longwave Flag"));
      dlflag.addAttribute(new Attribute("flag_values", "C M B I D G U"));
      dlflag.addAttribute(new Attribute("flag_meanings",
          "exceeds_field_size missing bad interpolated_or_estimated_or_gap_filled questionable good unchecked"));
      dlflag.addAttribute(new Attribute("coordinates", "time longitude latitude altitude"));
      dlflag.addAttribute(new Attribute("_FillValue", "M"));
      dlflag.addAttribute(new Attribute("missing_value", "M"));
      dlflag.setCachedData(dlfflagArray, false);
      ncfile.addVariable(null, dlflag);
      Variable ulong = new Variable(ncfile, null, null,
            "surface_upwelling_longwave_flux_in_air");
      ulong.setDimensions("time");
      ulong.setDataType(DataType.DOUBLE);
      ulong.addAttribute(new Attribute("standard_name",
            "surface_upwelling_longwave_flux_in_air"));
      ulong.addAttribute(new Attribute("long_name", "Outgoing Longwave"));
      ulong.addAttribute(new Attribute("units", "W/m2"));
      ulong.addAttribute(new Attribute("coordinates", "time longitude latitude altitude"));
      ulong.addAttribute(new Attribute("ancillary_variables",
            "surface_upwelling_longwave_flux_in_air_flag"));
      ulong.addAttribute(new Attribute("_FillValue", fillValue));
      ulong.addAttribute(new Attribute("missing_value", fillValue));
      ulong.setCachedData(ulongfluxArray, false);
      ncfile.addVariable(null, ulong);
      Variable ulflag = new Variable(ncfile, null, null,
            "surface_upwelling_longwave_flux_in_air_flag");
      ulflag.setDimensions("time");
      ulflag.setDataType(DataType.CHAR);
      ulflag.addAttribute(new Attribute("standard_name",
            "surface_upwelling_longwave_flux_in_air status_flag"));
      ulflag.addAttribute(new Attribute("long_name", "Outgoing Longwave Flag"));
      ulflag.addAttribute(new Attribute("flag_values", "C M B I D G U"));
      ulflag.addAttribute(new Attribute("flag_meanings",
          "exceeds_field_size missing bad interpolated_or_estimated_or_gap_filled questionable good unchecked"));
      ulflag.addAttribute(new Attribute("coordinates", "time longitude latitude altitude"));
      ulflag.addAttribute(new Attribute("_FillValue", "M"));
      ulflag.addAttribute(new Attribute("missing_value", "M"));
      ulflag.setCachedData(ulfflagArray, false);
      ncfile.addVariable(null, ulflag);
      Variable rad = new Variable(ncfile, null, null, "surface_net_downward_radiative_flux");
      rad.setDimensions("time");
      rad.setDataType(DataType.DOUBLE);
      rad.addAttribute(new Attribute("standard_name", "surface_net_downward_radiative_flux"));
      rad.addAttribute(new Attribute("long_name", "Net Radiation"));
      rad.addAttribute(new Attribute("units", "W/m2"));
      rad.addAttribute(new Attribute("coordinates", "time longitude latitude altitude"));
      rad.addAttribute(new Attribute("ancillary_variables",
            "surface_net_downward_radiative_flux_flag"));
      rad.addAttribute(new Attribute("_FillValue", fillValue));
      rad.addAttribute(new Attribute("missing_value", fillValue));
      rad.setCachedData(radArray, false);
      ncfile.addVariable(null, rad);
      Variable rflag = new Variable(ncfile, null, null,
            "surface_net_downward_radiative_flux_flag");
      rflag.setDimensions("time");
      rflag.setDataType(DataType.CHAR);
      rflag.addAttribute(new Attribute("standard_name",
            "surface_net_downward_radiative_flux status_flag"));
      rflag.addAttribute(new Attribute("long_name", "Net Radiation Flag"));
      rflag.addAttribute(new Attribute("flag_values", "C M B I D G U"));
      rflag.addAttribute(new Attribute("flag_meanings",
          "exceeds_field_size missing bad interpolated_or_estimated_or_gap_filled questionable good unchecked"));
      rflag.addAttribute(new Attribute("coordinates", "time longitude latitude altitude"));
      rflag.addAttribute(new Attribute("_FillValue", "M"));
      rflag.addAttribute(new Attribute("missing_value", "M"));
      rflag.setCachedData(radflagArray, false);
      ncfile.addVariable(null, rflag);
      Variable surft = new Variable(ncfile, null, null, "surface_temperature");
      surft.setDimensions("time");
      surft.setDataType(DataType.DOUBLE);
      surft.addAttribute(new Attribute("standard_name", "surface_temperature"));
      surft.addAttribute(new Attribute("long_name", "Skin Temperature"));
      surft.addAttribute(new Attribute("units", "K"));
      surft.addAttribute(new Attribute("coordinates", "time longitude latitude altitude"));
      surft.addAttribute(new Attribute("ancillary_variables", "surface_temperature_flag"));
      surft.addAttribute(new Attribute("_FillValue", fillValue));
      surft.addAttribute(new Attribute("missing_value", fillValue));
      surft.setCachedData(surftArray, false);
      ncfile.addVariable(null, surft);
      Variable stflag = new Variable(ncfile, null, null, "surface_temperature_flag");
      stflag.setDimensions("time");
      stflag.setDataType(DataType.CHAR);
      stflag.addAttribute(new Attribute("standard_name", "surface_temperature status_flag"));
      stflag.addAttribute(new Attribute("long_name", "Skin Temperature Flag"));
      stflag.addAttribute(new Attribute("flag_values", "C M B I D G U"));
      stflag.addAttribute(new Attribute("flag_meanings",
          "exceeds_field_size missing bad interpolated_or_estimated_or_gap_filled questionable good unchecked"));
      stflag.addAttribute(new Attribute("coordinates", "time longitude latitude altitude"));
      stflag.addAttribute(new Attribute("_FillValue", "M"));
      stflag.addAttribute(new Attribute("missing_value", "M"));
      stflag.setCachedData(stflagArray, false);
      ncfile.addVariable(null, stflag);
      Variable dpho = new Variable(ncfile, null, null,
            "surface_downwelling_photosynthetic_photon_flux_in_air");
      dpho.setDimensions("time");
      dpho.setDataType(DataType.DOUBLE);
      dpho.addAttribute(new Attribute("standard_name",
            "surface_downwelling_photosynthetic_photon_flux_in_air"));
      dpho.addAttribute(new Attribute("long_name", "Incoming PAR"));
      dpho.addAttribute(new Attribute("units", "mol/m2/s"));
      dpho.addAttribute(new Attribute("coordinates", "time longitude latitude altitude"));
      dpho.addAttribute(new Attribute("ancillary_variables",
            "surface_downwelling_photosynthetic_photon_flux_in_air_flag"));
      dpho.addAttribute(new Attribute("_FillValue", fillValue));
      dpho.addAttribute(new Attribute("missing_value", fillValue));
      dpho.setCachedData(dphofluxArray, false);
      ncfile.addVariable(null, dpho);
      Variable dpfflag = new Variable(ncfile, null, null,
            "surface_downwelling_photosynthetic_photon_flux_in_air_flag");
      dpfflag.setDimensions("time");
      dpfflag.setDataType(DataType.CHAR);
      dpfflag.addAttribute(new Attribute("standard_name",
            "surface_downwelling_photosynthetic_photon_flux_in_air status_flag"));
      dpfflag.addAttribute(new Attribute("long_name", "Incoming PAR Flag"));
      dpfflag.addAttribute(new Attribute("flag_values", "C M B I D G U"));
      dpfflag.addAttribute(new Attribute("flag_meanings",
          "exceeds_field_size missing bad interpolated_or_estimated_or_gap_filled questionable good unchecked"));
      dpfflag.addAttribute(new Attribute("coordinates", "time longitude latitude altitude"));
      dpfflag.addAttribute(new Attribute("_FillValue", "M"));
      dpfflag.addAttribute(new Attribute("missing_value", "M"));
      dpfflag.setCachedData(dpfflagArray, false);
      ncfile.addVariable(null, dpfflag);
      Variable upho = new Variable(ncfile, null, null,
            "surface_upwelling_photosynthetic_photon_flux_in_air");
      upho.setDimensions("time");
      upho.setDataType(DataType.DOUBLE);
      upho.addAttribute(new Attribute("standard_name",
            "surface_upwelling_photosynthetic_photon_flux_in_air"));
      upho.addAttribute(new Attribute("long_name", "Outgoing PAR"));
      upho.addAttribute(new Attribute("units", "mol/m2/s"));
      upho.addAttribute(new Attribute("coordinates", "time longitude latitude altitude"));
      upho.addAttribute(new Attribute("ancillary_variables",
            "surface_upwelling_photosynthetic_photon_flux_in_air_flag"));
      upho.addAttribute(new Attribute("_FillValue", fillValue));
      upho.addAttribute(new Attribute("missing_value", fillValue));
      upho.setCachedData(uphofluxArray, false);
      ncfile.addVariable(null, upho);
      Variable upflag = new Variable(ncfile, null, null,
            "surface_upwelling_photosynthetic_photon_flux_in_air_flag");
      upflag.setDimensions("time");
      upflag.setDataType(DataType.CHAR);
      upflag.addAttribute(new Attribute("standard_name",
            "surface_upwelling_photosynthetic_photon_flux_in_air status_flag"));
      upflag.addAttribute(new Attribute("long_name", "Outgoing PAR Flag"));
      upflag.addAttribute(new Attribute("flag_values", "C M B I D G U"));
      upflag.addAttribute(new Attribute("flag_meanings",
          "exceeds_field_size missing bad interpolated_or_estimated_or_gap_filled questionable good unchecked"));
      upflag.addAttribute(new Attribute("coordinates", "time longitude latitude altitude"));
      upflag.addAttribute(new Attribute("_FillValue", "M"));
      upflag.addAttribute(new Attribute("missing_value", "M"));
      upflag.setCachedData(upfflagArray, false);
      ncfile.addVariable(null, upflag);

// This is called to denote that the definition of the netCDF file is complete.
      ncfile.finish();
// Write all data out to the netCDF file.
      NetcdfFile nc = FileWriter.writeToFile(ncfile, outfilename);
// Clear out the netCDF file for the next station.
      ncfile.empty();
   }

   /**
    * This routine is not used because the readAllData routine reads the
    * entire file, so this routine will never be called.
    *
    * @param v2 - variable to read
    * @param section - section
    * @return - null, this routine is never called.
    * @throws IOException - if data can not be read
    * @throws InvalidRangeException - if range is incorrect
    */
   public Array readData(Variable v2, Section section) throws IOException, 
         InvalidRangeException {
      return null;
   }
   
   /**
    * This routine reads all the data from the CEOP data file.  It calls
    * createOutputFile, when all the data for one station has been read,
    * to write out the netCDF file.
    *
    * @param infile the CEOP data file.
    * @throws IOException when there is a problem with the CEOP data file.
    * @throws ConversionException when there is an invalid conversion performed
    *            on a variable.
    * @throws ParseException when there is a problem parsing a string, containing
    *            a number, to a number.
    */
   void readAllData(RandomAccessFile infile) throws IOException, ConversionException,
         ParseException {

// used to compare station just read with station last read.
      String saveStation, currentStation;
      long filePosition; /* keep track of current position in the CEOP data file. */
      String[] data; /* holds the data read from file */
      boolean endFile = false; /* set when at end of CEOP data file */
      boolean firstStation = true; /* only perform start date check on first station */
      double kilogramConversionFactor = 1000.0;
      double moleConversionFactor = 1000000.0;
      double var;

// Set up variable to hold date
      SimpleDateFormat dateTime = new SimpleDateFormat("yyyy/MM/ddHH:mm");
      dateTime.setTimeZone(TimeZone.getTimeZone("UTC"));
// Start at beginning of CEOP data file (file was previously read to determine
//    if it was a valid CEOP data file).
      infile.seek(0);
      while (! endFile) {    /* loop to read through entire CEOP data file */
         boolean first = true;
// reset all values for next station
         previousRhp = null;
         previousRsi = null;
         previousRhpDate = null;
         previousRsiDate = null;
         ArrayList<String[]> records = new ArrayList<String[]>();
/* Set strings to fake value so not null strings */
         saveStation = "1";
         currentStation = "1";
         while (true) {     /* loop to read each station */
            filePosition = infile.getFilePointer(); /* save file position */
            String line = infile.readLine();
            if (line == null) {    /* if at end of file */
               infile.close();
               endFile = true;
               break;
            }
            data = line.split("\\s+"); /* split data line at white space */
            saveStation = currentStation;
            currentStation = data[6];
            if (first) {
               saveStation = currentStation;
               first = false;
            }
// Check if a different station has just been read.
            if (! saveStation.equals(currentStation)) {
               break;
            }
            records.add(data);
         }
         int count = records.size();
// create dimension for all variables in netCDF file
         int[] shape = new int[]{count};
// check if data in first and last records of station agree with input file name,
// if not print warning.
         data = records.get(0);
         int inrhplength = data[4].length();
         int inrsilength = data[5].length();
         inrhp = partialFileName.substring(0, inrhplength);
         inrsi = partialFileName.substring(inrhplength + 1, inrhplength + inrsilength + 1);
         String checkdate = data[0].substring(0, 4) + data[0].substring(5, 7) +
               data[0].substring(8, 10);
         if (! inrhp.equals(data[4])) {
            System.out.println("WARNING: Station: " + data[6] + " RHP data in first record in file: " + data[4] +
                  " does not agree with file name: " + inrhp + ".\n");
         }
         if (! inrsi.equals(data[5])) {
            System.out.println("WARNING: Station: " + data[6] + " RSI data in first record in file: " + data[5] +
                  " does not agree with file name: " + inrsi + ".\n");
         }
// Only check if the first station has the same date in the first record as the input 
// file name, since any following station may not start with the date that appears in 
// the file name.
         if (firstStation) {
            if (! instartdate.equals(checkdate)) {
               System.out.println("WARNING: Station: " + data[6] + " Start date in first record in file: " +
                     checkdate + " does not agree with file name: " + instartdate + ".\n");
            }
         }
         data = records.get(count - 1);
         checkdate = data[0].substring(0, 4) + data[0].substring(5, 7) +
               data[0].substring(8, 10);
         if (! inrhp.equals(data[4])) {
            System.out.println("WARNING: Station: " + data[6] + " RHP data in last record in file: " + data[4] +
                  " does not agree with file name: " + inrhp + ".\n");
         }
         if (! inrsi.equals(data[5])) {
            System.out.println("WARNING: Station: " + data[6] + " RSI data in last record in file: " + data[5] +
                  " does not agree with file name: " + inrsi + ".\n");
         }
         if (! inenddate.equals(checkdate)) {
            System.out.println("WARNING: Station: " + data[6] + " End date in last record in file: " + checkdate +
                  " does not agree with file name: " + inenddate + ".\n");
         }
         firstStation = false;
// Set size of data arrays
         timeArray = (ArrayDouble.D1) Array.factory(DataType.DOUBLE, shape);
         timeNominalArray = (ArrayDouble.D1) Array.factory(DataType.DOUBLE, shape);
         pressArray = (ArrayDouble.D1) Array.factory(DataType.DOUBLE, shape);
         pflagArray = (ArrayChar.D1) Array.factory(DataType.CHAR, shape);
         tempArray = (ArrayDouble.D1) Array.factory(DataType.DOUBLE, shape);
         tflagArray = (ArrayChar.D1) Array.factory(DataType.CHAR, shape);
         dpArray = (ArrayDouble.D1) Array.factory(DataType.DOUBLE, shape);
         dpflagArray = (ArrayChar.D1) Array.factory(DataType.CHAR, shape);
         rhArray = (ArrayDouble.D1) Array.factory(DataType.DOUBLE, shape);
         rhflagArray = (ArrayChar.D1) Array.factory(DataType.CHAR, shape);
         shArray = (ArrayDouble.D1) Array.factory(DataType.DOUBLE, shape);
         shflagArray = (ArrayChar.D1) Array.factory(DataType.CHAR, shape);
         wspdArray = (ArrayDouble.D1) Array.factory(DataType.DOUBLE, shape);
         wsflagArray = (ArrayChar.D1) Array.factory(DataType.CHAR, shape);
         wdirArray = (ArrayDouble.D1) Array.factory(DataType.DOUBLE, shape);
         wdflagArray = (ArrayChar.D1) Array.factory(DataType.CHAR, shape);
         eastwArray = (ArrayDouble.D1) Array.factory(DataType.DOUBLE, shape);
         ewflagArray = (ArrayChar.D1) Array.factory(DataType.CHAR, shape);
         northwArray = (ArrayDouble.D1) Array.factory(DataType.DOUBLE, shape);
         nwflagArray = (ArrayChar.D1) Array.factory(DataType.CHAR, shape);
         precipArray = (ArrayDouble.D1) Array.factory(DataType.DOUBLE, shape);
         paflagArray = (ArrayChar.D1) Array.factory(DataType.CHAR, shape);
         snowArray = (ArrayDouble.D1) Array.factory(DataType.DOUBLE, shape);
         snowflagArray = (ArrayChar.D1) Array.factory(DataType.CHAR, shape);
         dshortfluxArray = (ArrayDouble.D1) Array.factory(DataType.DOUBLE, shape);
         dsfflagArray = (ArrayChar.D1) Array.factory(DataType.CHAR, shape);
         ushortfluxArray = (ArrayDouble.D1) Array.factory(DataType.DOUBLE, shape);
         usfflagArray = (ArrayChar.D1) Array.factory(DataType.CHAR, shape);
         dlongfluxArray = (ArrayDouble.D1) Array.factory(DataType.DOUBLE, shape);
         dlfflagArray = (ArrayChar.D1) Array.factory(DataType.CHAR, shape);
         ulongfluxArray = (ArrayDouble.D1) Array.factory(DataType.DOUBLE, shape);
         ulfflagArray = (ArrayChar.D1) Array.factory(DataType.CHAR, shape);
         radArray = (ArrayDouble.D1) Array.factory(DataType.DOUBLE, shape);
         radflagArray = (ArrayChar.D1) Array.factory(DataType.CHAR, shape);
         surftArray = (ArrayDouble.D1) Array.factory(DataType.DOUBLE, shape);
         stflagArray = (ArrayChar.D1) Array.factory(DataType.CHAR, shape);
         dphofluxArray = (ArrayDouble.D1) Array.factory(DataType.DOUBLE, shape);
         dpfflagArray = (ArrayChar.D1) Array.factory(DataType.CHAR, shape);
         uphofluxArray = (ArrayDouble.D1) Array.factory(DataType.DOUBLE, shape);
         upfflagArray = (ArrayChar.D1) Array.factory(DataType.CHAR, shape);
         latArray = new ArrayDouble.D0();
         lonArray = new ArrayDouble.D0();
         altArray = new ArrayDouble.D0();
// Read data stored in records and put in data arrays applying conversion factor
// if necessary.  If data contains -0.00, change it to 0.00.
         first = true;
         boolean missingAlt = true;
         boolean missingLat = true;
         boolean missingLon = true;
         for (int i = 0; i < count; i++) {
            data = records.get(i);
            Date d = dateTime.parse(data[2] + data[3]);
            timeArray.set(i, d.getTime() / 1000);
            d = dateTime.parse(data[0] + data[1]);
            timeNominalArray.set(i, d.getTime() / 1000);
// Save data from first record to create output file name and global attributes.
// Set latitude, longitude and altitude values to missing values.
            if (first) {
               rhp = data[4];
               rsi = data[5];
               station = data[6];
               startDateTime = sdf.format(d);
               latArray.set(Double.parseDouble("-99.99999"));
               lonArray.set(Double.parseDouble("-999.99999"));
               altArray.set(Double.parseDouble("-999.99"));
               first = false;
            }
// Check if latitude, longitude or altitude are missing values.  If so, then check
// them in the next record.
            if (missingLat) {
               if (! data[7].equals("-99.99999")) {
                  latArray.set(Double.parseDouble(data[7]));
                  missingLat = false;
               }
            }
            if (missingLon) {
               if (! data[8].equals("-999.99999")) {
                  lonArray.set(Double.parseDouble(data[8]));
                  missingLon = false;
               }
            }
            if (missingAlt) {
               if (! data[9].equals("-999.99")) {
                  altArray.set(Double.parseDouble(data[9]));
                  missingAlt = false;
               }
            }            
// Check the current latitude, longitude and altitude saved from the first record to the values
// in the other records.  If the values are different, use the last values for designating where
// the station is located and warn the user.  Also, perform this check for rhp, rsi and station name.
            double parseDouble;
            if (!missingLat) {
               parseDouble = Double.parseDouble(data[7]);
               if ((latArray.get() != parseDouble) && (parseDouble != -99.99999)) {
                  System.out.println("WARNING: Latitude difference for " + station + " - previous: " +
                        latArray.get() + " current: " + parseDouble + ".\n");
                  latArray.set(parseDouble);
               }
            }
            if (!missingLon) {
               parseDouble = Double.parseDouble(data[8]);
               if ((lonArray.get() != parseDouble) && (parseDouble != -999.99999)) {
                  System.out.println("WARNING: Longitude difference for " + station + " - previous: " +
                        lonArray.get() + " current: " + parseDouble + ".\n");
                  lonArray.set(parseDouble);
               }
            }
            if (!missingAlt) {
               parseDouble = Double.parseDouble(data[9]);
               if ((altArray.get() != parseDouble) && (parseDouble != -999.99)) {
                  System.out.println("WARNING: Altitude difference for " + station + " - previous: " +
                        altArray.get() + " current: " + parseDouble + ".\n");
                  altArray.set(parseDouble);
               }
            }
            if (! rhp.equals(data[4])) {
               System.out.println("WARNING: RHP difference for " + station + " - previous: " + rhp +
                     " current: " + data[4] + ".\n");
               previousRhp = rhp;
               rhp = data[4];
               previousRhpDate = data[0];
            }
            if (! rsi.equals(data[5])) {
               System.out.println("WARNING: RSI difference for " + station + " - previous: " + rsi +
                     " current: " + data[5] + ".\n");
               previousRsi = rsi;
               rsi = data[5];
               previousRsiDate = data[0];
            }
            endDateTime = sdf.format(d);
            if (data[10].contains("-0.00")) {
               var = Double.parseDouble("0.00");
            } else {
               var = Double.parseDouble(data[10]);
            }
            if (var != fillValue) {
               var = PressureUtils.convertPressure(var,
                     PressureUtils.HECTOPASCALS, PressureUtils.PASCALS);
            }
            pressArray.set(i, var);
            pflagArray.set(i, data[11].charAt(0));
            if (data[12].contains("-0.00")) {
               var = Double.parseDouble("0.00");
            } else {
               var = Double.parseDouble(data[12]);
            }
            if (var != fillValue) {
               var = TemperatureUtils.convertTemperature(var,
                     TemperatureUtils.CELCIUS,  TemperatureUtils.KELVIN);
            }
            tempArray.set(i, var);
            tflagArray.set(i, data[13].charAt(0));
            if (data[14].contains("-0.00")) {
               var = Double.parseDouble("0.00");
            } else {
               var = Double.parseDouble(data[14]);
            }
            if (var != fillValue) {
               var = TemperatureUtils.convertTemperature(var,
                     TemperatureUtils.CELCIUS,  TemperatureUtils.KELVIN);
            }
            dpArray.set(i, var);
            dpflagArray.set(i, data[15].charAt(0));
            if (data[16].contains("-0.00")) {
               var = Double.parseDouble("0.00");
            } else {
               var = Double.parseDouble(data[16]);
            }
            rhArray.set(i, var);
            rhflagArray.set(i, data[17].charAt(0));
            if (data[18].contains("-0.00")) {
               var = Double.parseDouble("0.00");
            } else {
               var = Double.parseDouble(data[18]);
            }
            if (var != fillValue) {
               var = var / kilogramConversionFactor;
            }
            shArray.set(i, var);
            shflagArray.set(i, data[19].charAt(0));
            if (data[20].contains("-0.00")) {
               var = Double.parseDouble("0.00");
            } else {
               var = Double.parseDouble(data[20]);
            }
            wspdArray.set(i, var);
            wsflagArray.set(i, data[21].charAt(0));
            if (data[22].contains("-0.00")) {
               var = Double.parseDouble("0.00");
            } else {
               var = Double.parseDouble(data[22]);
            }
            wdirArray.set(i, var);
            wdflagArray.set(i, data[23].charAt(0));
            if (data[24].contains("-0.00")) {
               var = Double.parseDouble("0.00");
            } else {
               var = Double.parseDouble(data[24]);
            }
            eastwArray.set(i, var);
            ewflagArray.set(i, data[25].charAt(0));
            if (data[26].contains("-0.00")) {
               var = Double.parseDouble("0.00");
            } else {
               var = Double.parseDouble(data[26]);
            }           
            northwArray.set(i, var);
            nwflagArray.set(i, data[27].charAt(0));
            if (data[28].contains("-0.00")) {
               var = Double.parseDouble("0.00");
            } else {
               var = Double.parseDouble(data[28]);
            }
            precipArray.set(i, var);
            paflagArray.set(i, data[29].charAt(0));
            if (data[30].contains("-0.00")) {
               var = Double.parseDouble("0.00");
            } else {
               var = Double.parseDouble(data[30]);
            }
            if (var != fillValue) {
               var = LengthUtils.convertLength(var,
                     LengthUtils.CENTIMETERS, LengthUtils.METERS);
            }
            snowArray.set(i, var);
            snowflagArray.set(i, data[31].charAt(0));
             if (data[32].contains("-0.00")) {
               var = Double.parseDouble("0.00");
            } else {
               var = Double.parseDouble(data[32]);
            }
            dshortfluxArray.set(i, var);
            dsfflagArray.set(i, data[33].charAt(0));
            if (data[34].contains("-0.00")) {
               var = Double.parseDouble("0.00");
            } else {
               var = Double.parseDouble(data[34]);
            }
            ushortfluxArray.set(i, var);
            usfflagArray.set(i, data[35].charAt(0));
            if (data[36].contains("-0.00")) {
               var = Double.parseDouble("0.00");
            } else {
               var = Double.parseDouble(data[36]);
            }
            dlongfluxArray.set(i, var);
            dlfflagArray.set(i, data[37].charAt(0));
            if (data[38].contains("-0.00")) {
               var = Double.parseDouble("0.00");
            } else {
               var = Double.parseDouble(data[38]);
            }
            ulongfluxArray.set(i, var);
            ulfflagArray.set(i, data[39].charAt(0));
            if (data[40].contains("-0.00")) {
               var = Double.parseDouble("0.00");
            } else {
               var = Double.parseDouble(data[40]);
            }
            radArray.set(i, var);
            radflagArray.set(i, data[41].charAt(0));
            if (data[42].contains("-0.00")) {
               var = Double.parseDouble("0.00");
            } else {
               var = Double.parseDouble(data[42]);
            }
            if (var != fillValue) {
               var = TemperatureUtils.convertTemperature(var,
                     TemperatureUtils.CELCIUS,  TemperatureUtils.KELVIN);
            }
            surftArray.set(i, var);
            stflagArray.set(i, data[43].charAt(0));
            if (data[44].contains("-0.00")) {
               var = Double.parseDouble("0.00");
            } else {
               var = Double.parseDouble(data[44]);
            }
            if (var != fillValue) {
               var = var / moleConversionFactor;
            }
            dphofluxArray.set(i, var);
            dpfflagArray.set(i, data[45].charAt(0));
            if (data[46].contains("-0.00")) {
               var = Double.parseDouble("0.00");
            } else {
               var = Double.parseDouble(data[46]);
            }                                
            if (var != fillValue) {
               var = var / moleConversionFactor;
            }
            uphofluxArray.set(i, var);
            upfflagArray.set(i, data[47].charAt(0));
         }
         if (missingLat) {
            System.out.println("WARNING: Station: " + data[6] + " has missing value for station latitude.\n");
         }
         if (missingLon) {
            System.out.println("WARNING: Station: " + data[6] + " has missing value for station longitude.\n");
         }
         if (missingAlt) {
            System.out.println("WARNING: Station: " + data[6] + " has missing value for station altitude.\n");
         }   
         createOutputFile(count);   /* Call to write out netCDF file */
// reset file pointer to position before start of new station
         if (! endFile) {
            infile.seek(filePosition);
         }
      }
   }

   public String getFileTypeVersion() {
      return("1.0");
   }

   public String getFileTypeDescription() {
      return("CEOP Surface Meteorological and Radiation Observation Data");
   }
   public String getFileTypeId() {
      return("CEOPSurface");
   }

   public void close() throws IOException {
      infile.close();
   }

   protected static void usage(int ret) {
      System.err.println("usage: CEOPsfciosp\n" +
            "-i <inCEOPfile>    CEOP data file to convert\n" +
            "-o <outdirname>    Directory for output file (defaults to current directory)\n");
      System.exit(ret);
   }

   public static void main(String[] args) throws Exception {

// Parse arguments on command line
      Getopt g = new Getopt("CEOPsfciosp", args, "i:o:");

      String infilename = null;
      String testfilename;
      int c;
      while ((c = g.getopt()) != -1) {
         switch (c) {
            case 'i':
               infilename = g.getOptarg();
               break;
            case 'o':
               outdirname = g.getOptarg();
               break;            
            default:
               usage(1);
         }
      }
      if (infilename == null) {
         usage(1);
      }
      System.out.println("Input file is " + infilename + "\n");
      ucar.nc2.NetcdfFile.registerIOProvider(CEOPsfciosp.class);
      NetcdfFile ncfile = null;
// Save information from CEOP data file name to check against data
// in the file after stripping off the directory name (if supplied).
      testfilename = infilename;
      int position = infilename.lastIndexOf("/");
      if (position != -1) {
         testfilename = infilename.substring(position + 1);
      }
      String[] data = testfilename.split("_");
      int size = data.length;
      partialFileName = data[0];
      for (int i = 1; i < size - 1; i++) {
         partialFileName = partialFileName + "_" + data[i];
      }
      instartdate = data[size - 2];
      data = data[size - 1].split("\\.");
      inenddate = data[0];
      fileType = data[1];
      try {
         ncfile = NetcdfFile.open(infilename);
         System.out.println("ncfiles created are \n");
         for (Object fileName : fileNames) {
            String outfilename = (String) fileName;
            System.out.println(outfilename);
         }
      } catch (FileNotFoundException e) {
         throw new IOException(e.getMessage());
      } catch (IOException e) {
// Create message for any IO exceptions that are not FileNotFound exceptions, since the error
// message that is generated from the Unidata isValidFile routine is an incorrect message.     
         throw new IOException("ERROR: Input file " + infilename + " is not a valid CEOP sfc file.");
      } finally {
         if (ncfile != null) {
            ncfile.close();
         }
      }
   }
}
