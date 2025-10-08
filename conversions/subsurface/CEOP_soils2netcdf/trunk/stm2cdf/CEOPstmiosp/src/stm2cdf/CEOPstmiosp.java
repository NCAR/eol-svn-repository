/*
 * CEOPstmiosp.java
 *
 */
package stm2cdf;

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
 * This program reads the CEOP Soil Temperature and Moisture Dataset data
 * files and converts them to a standard netCDF format using CF Conventions for
 * the variable names.  This program utilizes the IOSP (Input/Output Service
 * Provider) interface from Unidata to create the netCDF files.  Each station will
 * be written to a separate netCDF file even though the CEOP data files may
 * contain more than one station.  The name of the output netCDF file is created
 * from the RHP, RSI, and station information for the data.
 * The data contains values at different heights for the same time.  The values
 * for each height for a specific time are written contiguously in the netCDF
 * file.
 * 
 * This program will read through the data file until a different station is found.
 * Then, the netCDF file is created for the first station.  The data file is then
 * read from the start of the second station and continues in this manner until
 * the end of the CEOP data file has been reached.
 *
 * @author $Author: anstett $
 * @version $Revision: 1.0 $
 */
public class CEOPstmiosp extends AbstractIOServiceProvider {
   protected NetcdfFile ncfile;         /* output netCDF file */
   protected RandomAccessFile infile;   /* input CEOP data file */
   protected static String outdirname;  /* output directory name */
// list of netCDF file names created from the CEOP data file.
   protected static ArrayList<String> fileNames;
// data gathered from the CEOP data file name.
   protected static String inrhp, inrsi, instartdate, inenddate, fileType;
   protected static String partialFileName;

   public boolean isValidFile(RandomAccessFile infile) throws IOException {

/**                       
 * Determine quickly whether the input file is of the CEOP soils type.  This
 * checks for slashes and a colon in particular columns where the date and time
 * are located and also checks for a newline character at the end of the
 * expected length of a data line.  The newline character check is to
 * distinguish between the formats of the 4 different CEOP formats, since all
 * the CEOP data files will have the date and time in the same spot in the file.
 * It also checks the suffix of the input file.
 */
      infile.seek(0);
      byte[] b = new byte[140];
      infile.read(b);
      String test = new String(b);
      return (test.substring(4, 5).equals("/") && test.substring(7, 8).equals("/") &&
            test.substring(10, 11).equals(" ") && test.substring(13, 14).equals(":")
            && ((test.charAt(137) == '\n') || ((test.charAt(137) == '\r') &&
            (test.charAt(138) == '\n'))) && fileType.equals("stm"));
   }

   /**
    * Arrays used to store the data for each variable in the netcdf file.
    */
   private ArrayDouble.D1 heightArray;
   private ArrayDouble.D0 latArray, lonArray, altArray;
   private ArrayDouble.D1 timeArray, timeNominalArray;
   private ArrayDouble.D2 tempArray, moistArray;
   private ArrayChar.D2 tflagArray, mflagArray;
// Data saved for the global attributes.
   private String rhp, rsi, station;
// Data saved if previous values found in data to create the comments in the header.
   private String previousRhp, previousRsi;
   private String previousRhpDate, previousRsiDate;
   private Double fillValue; /* missing value for the variables */
// Start and end times saved for the global attributes in the netCDF file.
   private String startDateTime, endDateTime;
// Needed for formatting the dates for the netCDF file.
   private SimpleDateFormat sdf;
// holds the different heights for the station.
   private ArrayList<Double> sensorHeight;

/**
 * This routine opens the CEOP data file, reads the data and creates the netCDF
 * files.  This routine catches any parse exceptions when the String data is
 * parsed into a number and also catches any conversion exception that may occur
 * when converting values between different units (i.e. converting temperatures
 * from Kelvin to Celsius).
 *
 * @param infile - CEOP soils input data file
 * @param ncfile - CEOP soils output netCDF file
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
         throw new IOException("CEOPstmiosp: Parse exception occured.");
      } catch (ConversionException e) {
         e.printStackTrace();
         throw new IOException("CEOPstmiosp: Invalid conversion attempted.");
      }
   }

   /**
    * This routine is called after each station has been read from the CEOP data file.
    * It writes out the netCDF file for that station.
    *
    * @param numRecords number of different times for the current station.
    * @throws IOException if there is a problem writing to the netCDF file.
    */
   void createOutputFile(int numRecords) throws IOException {

      String outfilename;
      int nameLength = 30;
      int[] shapeChar = new int[]{nameLength};
      ArrayChar.D1 idArray = (ArrayChar.D1) Array.factory(DataType.CHAR, shapeChar);

// Create output netCDF file name and add the name to the list of filenames created.
      idArray.setString(station);
      outfilename = rhp + "_" + rsi + "_" + station + "_stm.nc";
      if (outdirname != null) {
         outfilename = outdirname + "/" + outfilename;
      }
      fileNames.add(outfilename);
      String date = sdf.format(Calendar.getInstance().getTime());
// Create global attributes for the netCDF file.
      ncfile.addAttribute(null, new Attribute("Conventions", "CF-1.6-beta"));
      ncfile.addAttribute(null, new Attribute("Conventions_Comment",
            "Used an incomplete draft of the future version CF-1.6 for the time series of profiles representation."));      
      ncfile.addAttribute(null, new Attribute("title", 
            "CEOP Soil Temperature and Moisture Dataset"));
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
      ncfile.addAttribute(null, new Attribute("Data_Type", "stm"));
      ncfile.addAttribute(null, new Attribute("Data_Type_Long_Name", "Soils"));
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
      ncfile.addAttribute(null, new Attribute("featureType", "timeSeriesProfile"));

// Create dimensions for time, heights and name string length.
      Dimension timeDim = new Dimension("time", numRecords, true);
      ncfile.addDimension(null, timeDim);
      int numHeights = sensorHeight.size();
      Dimension sensorDim = new Dimension("height", numHeights, true);
      ncfile.addDimension(null, sensorDim);
      Dimension nameLengthDim = new Dimension("name_strlen", nameLength, true);
      ncfile.addDimension(null, nameLengthDim);

// Create all the variables in the netCDF file.
      Variable height = new Variable(ncfile, null, null, "height");
      height.setDimensions("height");
      height.setDataType(DataType.DOUBLE);
      height.addAttribute(new Attribute("standard_name", "height"));
      height.addAttribute(new Attribute("long_name", "Sensor Height"));
      height.addAttribute(new Attribute("units", "m"));
      height.addAttribute(new Attribute("positive", "up"));
      height.setCachedData(heightArray, false);
      ncfile.addVariable(null, height);
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
      Variable temp = new Variable(ncfile, null, null, "soil_temperature");
      temp.setDimensions("time height");
      temp.setDataType(DataType.DOUBLE);
      temp.addAttribute(new Attribute("standard_name", "soil_temperature"));
      temp.addAttribute(new Attribute("long_name", "Soil Temperature"));
      temp.addAttribute(new Attribute("units", "K"));
      temp.addAttribute(new Attribute("coordinates", "time longitude latitude altitude height"));
      temp.addAttribute(new Attribute("ancillary_variables", "soil_temperature_flag"));
      temp.addAttribute(new Attribute("_FillValue", fillValue));
      temp.addAttribute(new Attribute("missing_value", fillValue));
      temp.setCachedData(tempArray, false);
      ncfile.addVariable(null, temp);
      Variable tempflag = new Variable(ncfile, null, null, "soil_temperature_flag");
      tempflag.setDimensions("time height");
      tempflag.setDataType(DataType.CHAR);
      tempflag.addAttribute(new Attribute("standard_name", "soil_temperature status_flag"));
      tempflag.addAttribute(new Attribute("long_name", "Soil Temperature Flag"));
      tempflag.addAttribute(new Attribute("flag_values", "C M B I D G U"));
      tempflag.addAttribute(new Attribute("flag_meanings",
            "exceeds_field_size missing bad interpolated_or_estimated_or_gap_filled questionable good unchecked"));
      tempflag.addAttribute(new Attribute("coordinates", "time longitude latitude altitude height"));
      tempflag.addAttribute(new Attribute("_FillValue", "M"));
      tempflag.addAttribute(new Attribute("missing_value", "M"));
      tempflag.setCachedData(tflagArray, false);
      ncfile.addVariable(null, tempflag);
      Variable moisture = new Variable(ncfile, null, null, "volume_fraction_of_water_in_soil");
      moisture.setDimensions("time height");
      moisture.setDataType(DataType.DOUBLE);
      moisture.addAttribute(new Attribute("standard_name", "volume_fraction_of_water_in_soil"));
      moisture.addAttribute(new Attribute("long_name", "Soil Moisture"));
      moisture.addAttribute(new Attribute("units", "percent"));
      moisture.addAttribute(new Attribute("coordinates", "time longitude latitude altitude height"));
      moisture.addAttribute(new Attribute("ancillary_variables", "volume_fraction_of_water_in_soil_flag"));
      moisture.addAttribute(new Attribute("_FillValue", fillValue));
      moisture.addAttribute(new Attribute("missing_value", fillValue));
      moisture.setCachedData(moistArray, false);
      ncfile.addVariable(null, moisture);
      Variable mflag = new Variable(ncfile, null, null, "volume_fraction_of_water_in_soil_flag");
      mflag.setDimensions("time height");
      mflag.setDataType(DataType.CHAR);
      mflag.addAttribute(new Attribute("standard_name", "volume_fracton_of_water_in_soil status_flag"));
      mflag.addAttribute(new Attribute("long_name", "Soil Moisture Flag"));
      mflag.addAttribute(new Attribute("flag_values", "C M B I D G U"));
      mflag.addAttribute(new Attribute("flag_meanings",
            "exceeds_field_size missing bad interpolated_or_estimated_or_gap_filled questionable good unchecked"));
      mflag.addAttribute(new Attribute("coordinates", "time longitude latitude altitude height"));
      mflag.addAttribute(new Attribute("_FillValue", "M"));
      mflag.addAttribute(new Attribute("missing_value", "M"));
      mflag.setCachedData(mflagArray, false);
      ncfile.addVariable(null, mflag);
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
    * @param v2 - variable to read.
    * @param section - section
    * @return - null, this routine is never called.
    * @throws IOException - if data can not be read.
    * @throws InvalidRangeException - if range is incorrect.
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
    *                             on a variable.
    * @throws ParseException when there is a problem parsing a string, containing
    *                        a number, to a number.
    */
   void readAllData(RandomAccessFile infile) throws IOException, ConversionException,
         ParseException {

// used to compare station and time just read with station and time last read.
      String saveStation, currentStation, keepTime, currentTime;
      long filePosition; /* keep track of current position in the CEOP data file. */
      String[] data; /* holds the data read from file */
      boolean endFile = false; /* set when at end of CEOP data file */
      boolean firstStation = true; /* only perform start date check on first station */
      int count; /* Counts number of different times for a station. */
      double var;

// Set up variable to hold date
      SimpleDateFormat dateTime = new SimpleDateFormat("yyyy/MM/ddHH:mm");
      dateTime.setTimeZone(TimeZone.getTimeZone("UTC"));
// Start at beginning of CEOP data file (file was previously read to determine
//    if it was a valid CEOP data file).
      infile.seek(0);
      while (! endFile) {    /* loop to read through entire CEOP data file */
         boolean first = true;
         count = 0;
// reset all values for next station
         previousRhp = null;
         previousRsi = null;
         previousRhpDate = null;
         previousRsiDate = null;
         ArrayList<String[]> records = new ArrayList<String[]>();
         sensorHeight = new ArrayList<Double>();
/* Set strings to fake value so not null strings */
         saveStation = "1";
         currentStation = "1";
         keepTime = "1";
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
            currentTime = data[0] + data[1];
            if (first) {
               saveStation = currentStation;
               first = false;
            }
// Check if a different station has just been read.
            if (! saveStation.equals(currentStation)) {
               break;
            }
// Get height for this data and add to sensorHeights if the value is
// not already in the list.  Keep all heights in numerical ascending order.
            var = Double.parseDouble(data[10]);
            if (sensorHeight.isEmpty() | ! sensorHeight.contains(var)) {
               sensorHeight.add(var);
            }
// count number of different times for this station.            
            if (! keepTime.equals(currentTime)) {
               count++;  
            }
            keepTime = currentTime;
            records.add(data);
         }
// create dimensions for time and height for all variables in the netCDF file
         int height = sensorHeight.size();
         int[] shape = new int[]{count, height};
         int[] shape2 = new int[]{height};
         int[] shape3 = new int[]{count};
// check if data in first and last records of station agree with file name,
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
         if (sensorHeight.contains(fillValue)) {
            System.out.println("WARNING: Sensor height is a missing value for station: " + data[6] + "\n");
         }
// Sort heights in ascending order.
         try {
            Collections.sort(sensorHeight);
         } catch (ClassCastException e) {
              e.printStackTrace();
              throw new IOException("CEOPstmiosp: Class cast exception in sorting heights array.");
         } catch (UnsupportedOperationException e) {
              e.printStackTrace();
              throw new IOException("CEOPstmiosp: Unsupported operation exception in sorting heights array.");
         }
         data = records.get(records.size() - 1);
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
// Set size of data arrays.
         timeArray = (ArrayDouble.D1) Array.factory(DataType.DOUBLE, shape3);
         timeNominalArray = (ArrayDouble.D1) Array.factory(DataType.DOUBLE, shape3);
         tempArray = (ArrayDouble.D2) Array.factory(DataType.DOUBLE, shape);
         tflagArray = (ArrayChar.D2) Array.factory(DataType.CHAR, shape);
         moistArray = (ArrayDouble.D2) Array.factory(DataType.DOUBLE, shape);
         mflagArray = (ArrayChar.D2) Array.factory(DataType.CHAR, shape);
         heightArray = (ArrayDouble.D1) Array.factory(DataType.DOUBLE, shape2);
         latArray = new ArrayDouble.D0();
         lonArray = new ArrayDouble.D0();
         altArray = new ArrayDouble.D0();
// Set heights in heightArray
         for (int i = 0; i < height; i++) {
            Double temp = sensorHeight.get(i);
            heightArray.set(i, temp);
         }
// Read data stored in records and put in data arrays applying conversion
// factor if necessary.  If data contains -0.00, change it to 0.00.
         first = true;
         boolean missingAlt = true;
         boolean missingLat = true;
         boolean missingLon = true;
         int recordpos = 0;
         Date d;
         double savetime = 0, checktime;
         char charFill = 'M';
         int recordnum = records.size();
         for (int i = 0; i < count; i++) {
            for (int j = 0; j < height; j++) {
// The following special condition will be true if there is more than one missing record
// that needs to be added at the end of a file.  We have reached the end of the data
// records and there are still more records to be written to the output file.
               if (recordpos == recordnum) {
// No need to set time since they will have been set already, since there is only
// one time for each set of heights.
                  tempArray.set(i, j, fillValue);
                  tflagArray.set(i, j, charFill);
                  moistArray.set(i, j, fillValue);
                  mflagArray.set(i, j, charFill);
               } else {
                  data = records.get(recordpos);
                  d = dateTime.parse(data[0] + data[1]);
                  checktime = d.getTime() / 1000;
                  double heightVar = Double.parseDouble(data[10]);
// Make sure all heights are included for a specific time.
                  if (j == 0) {
                     savetime = checktime;
                  }
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
                     System.out.println("WARNING: RHP difference for " + station + " - previous: " + 
                        rhp + " current: " + data[4] + ".\n");
                     previousRhp = rhp;
                     rhp = data[4];
                     previousRhpDate = data[0];
                  }
                  if (! rsi.equals(data[5])) {
                     System.out.println("WARNING: RSI difference for " + station + " - previous: " + 
                        rsi + " current: " + data[5] + ".\n");
                     previousRsi = rsi;
                     rsi = data[5];
                     previousRsiDate = data[0];
                  }
// Create missing value record if time isn't the same for all heights or if the
// height in the record is not the expected height.
                  if ((savetime != checktime) || (heightArray.get(j) != heightVar)) {
// No need to set time since they will have been set already, since there is only
// one time for each set of heights.
                     tempArray.set(i, j, fillValue);
                     tflagArray.set(i, j, charFill);
                     moistArray.set(i, j, fillValue);
                     mflagArray.set(i, j, charFill);
                  } else {
                     endDateTime = sdf.format(d);
                     timeNominalArray.set(i, checktime);
                     d = dateTime.parse(data[2] + data[3]);
                     timeArray.set(i, d.getTime() / 1000);
                     if (data[11].contains("-0.00")) {
                        var = Double.parseDouble("0.00");
                     } else {
                        var = Double.parseDouble(data[11]);
                     }
                     if (var != fillValue) {
                        var = TemperatureUtils.convertTemperature(var,
                              TemperatureUtils.CELCIUS, TemperatureUtils.KELVIN);
                     }
                     tempArray.set(i, j, var);
                     tflagArray.set(i, j, data[12].charAt(0));
                     if (data[13].contains("-0.00")) {
                        var = Double.parseDouble("0.00");
                     } else {
                        var = Double.parseDouble(data[13]);
                     }
                     moistArray.set(i, j, var);
                     mflagArray.set(i, j, data[14].charAt(0));
                     recordpos++;
                  }
               }
            }
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
         createOutputFile(count); /* Call to write out netCDF file */
// reset file pointer to position before start of new station.
         if (! endFile) {
            infile.seek(filePosition);
         }
      }
   }

   public String getFileTypeVersion() {
      return("1.0");
   }

   public String getFileTypeDescription() {
      return("CEOP Soil Temperature and Moisture Observation Data");
   }
   public String getFileTypeId() {
      return("CEOPSoils");
   }

   public void close() throws IOException {
      infile.close();
   }

   protected static void usage(int ret) {
      System.err.println("usage: CEOPstmiosp\n" +
            "-i <inCEOPfile>   CEOP data file to convert\n" + 
            "-o <outdirname>   Directory for output file (defaults to current directory)\n");
      System.exit(ret);
   }

   public static void main(String[] args) throws Exception {

//Parse arguments on command line.      
      Getopt g = new Getopt("CEOPstmiosp", args, "i:o:");

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
      ucar.nc2.NetcdfFile.registerIOProvider(CEOPstmiosp.class);
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
// Create message for any IO exceptions that are not FileNotFound exceptions, since 
// the error message that is generated from the Unidata isValidFile routine is an 
// incorrect message.
         throw new IOException("ERROR: Input file " + infilename + " is not a valid CEOP stm file.");
      } finally {
         if (ncfile != null) {
            ncfile.close();
         }
      }
   }
}
