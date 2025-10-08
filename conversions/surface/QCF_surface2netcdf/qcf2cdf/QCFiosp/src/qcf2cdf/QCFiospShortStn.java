/*
 * QCFiospShortStn.java
 *
 */
package qcf2cdf;

import ucar.ma2.*;

import ucar.nc2.*;
import ucar.nc2.iosp.*;
import ucar.nc2.util.CancelTask;
import ucar.nc2.NetcdfFile;
import ucar.nc2.write.NetcdfFormatWriter;
import ucar.nc2.write.NetcdfCopier;
import ucar.unidata.io.RandomAccessFile;

import java.io.*;
import java.util.*;
import java.lang.Class;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.text.ParseException;
import java.text.SimpleDateFormat;

import gnu.getopt.*;

import dmg.util.*;

/**
 * Read QCF data files and convert to a NetCDF file.
 * 
 * This QCFiospShortStn program is used to convert the QCF data files with a station
 * value that is 10 characters instead of the usual 15 characters. This also means that
 * each header record is 5 characters shorter.  Example data sets are 1.33 and 1.65. 
 *
 * This program reads the QCF data
 * files and converts them to a standard netCDF format using CF Conventions for
 * the variable names.  This program utilizes the IOSP (Input/Output Service
 * Provider) interface from Unidata to create the netCDF files.  Each station will
 * be written to a separate netCDF file even though the QCF data files may
 * contain more than one station.  The name of the output netCDF file is created
 * from the RHP, RSI, and station information for the data.
 *
 * This program will read through the data file until a different station is found.
 * Then, the netCDF file is created for the first station.  The data file is then
 * read from the start of the second station and continues in this manner until
 * the end of the QCF data file has been reached.
 *
 * @author $Author: anstett $
 * @version $Revision: 1.0 $
 */
public class QCFiospShortStn extends AbstractIOServiceProvider {
   protected static NetcdfFile ncfile; /* output netCDF file */
   protected RandomAccessFile infile;  /* input QCF data file */
   protected static String outdirname; /* output directory name */
// netCDF file name created from the QCF data file
   protected static String outfilename;
// data gathered from the QCF data file name
   protected static String instartdate, fileType;
// Needed to create the netcdf output file
   protected static Group.Builder rootGroup;
// Constants used for connecting to the database.
   private Connection connection;
// Database query statements
   private PreparedStatement datasetStmt, projectStmt, versionStmt, doiStmt, contactStmt;
   private static final String URL = "jdbc:mysql://emdac.eol.ucar.edu/zith9?useSSL=false&allowPublicKeyRetrieval=true";
// Database username and password
   protected static String pass, user;
// Results from the database query
   private ResultSet rs;
// Dataset id
   protected static String datasetId;

/**
 * Determine quickly whether the input file is of the QCF type.  This
 * checks for slashes and a colon in particular columns where the date and time
 * are located.  All of the different types of QCF data files
 * have the same number of characters in a line (1min, 5min, hourly, and misc). 
 * The substring values are high to allow for the header records.  There are 3 header
 * lines at 261 characters each (including new line character).
 */
public boolean isValidFile(RandomAccessFile infile) throws IOException {
      infile.seek(0);
      byte[] b = new byte[1070];
      infile.read(b);
      String test = new String(b);
      return (test.substring(787, 788).equals("/") && test.substring(790, 791).equals("/") &&
            test.substring(796, 797).equals(":") && test.substring(799, 800).equals(":") &&
            (fileType.equals("qcf")));
   }

   /**
    * Arrays used to store the data for each variable in the netcdf file.
    */
   private ArrayDouble.D1 timeArray, timeNominalArray;
   private ArrayDouble.D1 latArray, lonArray, eleArray;
   private ArrayDouble.D1 pressArray, pressRSLArray, pressCSLArray, tempArray, dpArray;
   private ArrayDouble.D1 wspdArray, wdirArray, precipArray, gustspArray, visArray;
   private ArrayDouble.D2 cloudArray;
   private ArrayInt.D1 preswxArray;
   private ArrayInt.D2 cindArray, camtArray;
   private ArrayShort.D1 occArray;
   private ArrayChar.D1 pflagArray, tflagArray, dpflagArray;
   private ArrayChar.D1 pRSLflagArray, pCSLflagArray, visflagArray, gsflagArray;
   private ArrayChar.D1 wsflagArray, wdflagArray, pwflagArray;
   private ArrayChar.D1 paflagArray, gustindArray;
   private ArrayChar.D2 networkArray, idArray, cindflagArray, camtflagArray;
   private Double fillValue;  /* missing value for the variables. */
   private int fillCloudValue; /* missing value for cloud variables. */
// Start and end times saved for the global attributes in the netCDF file.
   private String startDateTime, endDateTime;
// Needed for formatting the dates for the netCDF file.
   private SimpleDateFormat sdf;

/**
 * This routine opens the QCF data file, reads the data and creates the netCDF
 * file.  This routine catches any parse exceptions when the String data is
 * parsed into a number and also catches any conversion exception that may occur
 * when converting values between different units (i.e. converting temperatures
 * from Celsius to Kelvin).
 *
 * @param infile  - QCF input data file
 * @param ncfile  - QCF output netCDF file
 * @param cancelTask - required for the IOSP
 * @throws IOException if there is a problem with either the input or output files.
 */

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
    * This routine reads all the data from the QCF data file.  It calls
    * the build routine to write out the netCDF file.
    *
    * @param infile the QCF data file.
    * @throws IOException when there is a problem with the QCF data file.
    * @throws ConversionException when there is an invalid conversion performed
    *            on a variable.
    * @throws ParseException when there is a problem parsing a string, containing
    *            a number, to a number.
    */
   int readAllData(RandomAccessFile infile) throws IOException, ConversionException,
         ParseException {

      String[] temp, temp2, data;
      boolean endFile = false; /* set when at end of QCF data file */
      double var;
      short shortvar;
      int intvar;
      boolean badDate;

// Temporary start and end date times to compare when reading the file.
// Find the earliest start date time and the latest date time.
      Date tempStartDate = null, tempEndDate = null;

// Set up variable to hold date
      SimpleDateFormat dateTime = new SimpleDateFormat("yyyy/MM/ddHH:mm:ss");
      dateTime.setTimeZone(TimeZone.getTimeZone("UTC"));
      ArrayList<String[]> records = new ArrayList<>();
// Reposition file pointer to beginning of first data record (skip first three
// header lines of data file).  (File was previously
// read to determine if it was a valid QCF file).

      infile.seek(783);
      while (! endFile) {    /* loop to read through entire QCF data file */
         String line = infile.readLine();
         if (line == null) {    /* if at end of file */
            infile.close();
            endFile = true;
            break;
         }
// data2 is used to store the final values of the data record.
         String[] data2 = new String[48];
         
// Since station identifier can have spaces in the name, set first six fields by
// position in data record.
                 
         data2[0] = line.substring(0, 10);
         data2[1] = line.substring(11, 19);
         data2[2] = line.substring(20, 30);
         data2[3] = line.substring(31, 39);
         data2[4] = line.substring(40, 50);
         data2[5] = line.substring(51, 61);
// Start line2 with Latitude field and end with Total Precipitation flag field.
         String line2 = line.substring(62, 176).trim();
         temp = line2.split("\\s+"); /* split data line at white space */
         System.arraycopy(temp, 0, data2, 6, temp.length);
// Set Squall/Gust Indicator field with the actual value, since "blank" is 
// a valid value.
         data2[26] = line.substring(177, 178);
// Start line3 with Squall/Gust Value field to the end of the data record.
         String line3 = line.substring(179).trim();
         temp2 = line3.split("\\s+");
         System.arraycopy(temp2, 0, data2, 27, temp2.length);
         records.add(data2);
      }
      int count = records.size();
      
// create dimension for all variables in netCDF file
      int[] shape = new int[]{count}; /* number of records in the data file. */
      int[] networkShape = new int[]{count, 10};
      int[] cloud_level = new int []{count, 3};

// Set size of data arrays
      timeArray = (ArrayDouble.D1) Array.factory(DataType.DOUBLE, shape);
      timeNominalArray = (ArrayDouble.D1) Array.factory(DataType.DOUBLE, shape);
      networkArray = (ArrayChar.D2) Array.factory(DataType.CHAR, networkShape);
      idArray = (ArrayChar.D2) Array.factory(DataType.CHAR, networkShape);
      latArray = (ArrayDouble.D1) Array.factory(DataType.DOUBLE, shape);
      lonArray = (ArrayDouble.D1) Array.factory(DataType.DOUBLE, shape);
      occArray = (ArrayShort.D1) Array.factory(DataType.SHORT, shape);
      eleArray = (ArrayDouble.D1) Array.factory(DataType.DOUBLE, shape);         
      pressArray = (ArrayDouble.D1) Array.factory(DataType.DOUBLE, shape);
      pflagArray = (ArrayChar.D1) Array.factory(DataType.CHAR, shape);        
      pressRSLArray = (ArrayDouble.D1) Array.factory(DataType.DOUBLE, shape);
      pRSLflagArray = (ArrayChar.D1) Array.factory(DataType.CHAR, shape);
      pressCSLArray = (ArrayDouble.D1) Array.factory(DataType.DOUBLE, shape);
      pCSLflagArray = (ArrayChar.D1) Array.factory(DataType.CHAR, shape);
      tempArray = (ArrayDouble.D1) Array.factory(DataType.DOUBLE, shape);
      tflagArray = (ArrayChar.D1) Array.factory(DataType.CHAR, shape);
      dpArray = (ArrayDouble.D1) Array.factory(DataType.DOUBLE, shape);
      dpflagArray = (ArrayChar.D1) Array.factory(DataType.CHAR, shape);
      wspdArray = (ArrayDouble.D1) Array.factory(DataType.DOUBLE, shape);
      wsflagArray = (ArrayChar.D1) Array.factory(DataType.CHAR, shape);
      wdirArray = (ArrayDouble.D1) Array.factory(DataType.DOUBLE, shape);
      wdflagArray = (ArrayChar.D1) Array.factory(DataType.CHAR, shape);
      precipArray = (ArrayDouble.D1) Array.factory(DataType.DOUBLE, shape);
      paflagArray = (ArrayChar.D1) Array.factory(DataType.CHAR, shape);
      gustindArray = (ArrayChar.D1) Array.factory(DataType.CHAR, shape);
      gustspArray = (ArrayDouble.D1) Array.factory(DataType.DOUBLE, shape);
      gsflagArray = (ArrayChar.D1) Array.factory(DataType.CHAR, shape);
      preswxArray = (ArrayInt.D1) Array.factory(DataType.INT, shape);
      pwflagArray = (ArrayChar.D1) Array.factory(DataType.CHAR, shape);
      visArray = (ArrayDouble.D1) Array.factory(DataType.DOUBLE, shape);
      visflagArray = (ArrayChar.D1) Array.factory(DataType.CHAR, shape);
      cloudArray = (ArrayDouble.D2) Array.factory(DataType.DOUBLE, cloud_level);
      cindArray = (ArrayInt.D2) Array.factory(DataType.INT, cloud_level);
      cindflagArray = (ArrayChar.D2) Array.factory(DataType.CHAR, cloud_level);
      camtArray = (ArrayInt.D2) Array.factory(DataType.INT, cloud_level);
      camtflagArray = (ArrayChar.D2) Array.factory(DataType.CHAR, cloud_level);

// Read data stored in records and put in data arrays applying conversion factor
// if necessary.  If data contains -0.00, change it to 0.00.
      for (int i = 0; i < count; i++) {
         data = records.get(i);
// Parse nominal date time.
         Date d = dateTime.parse(data[0] + data[1]);
         timeNominalArray.set(i, d.getTime() / 1000);
// Check if the 1st 2 digits of the year for the time variable are 00
// This indicates a bad year for the record.  Change the year to the same year
// as the nominal time.  If the minutes of time are > 59 (i.e. invalid), change
// the minutes to 55.
// If any of the flags for this record are G or D, change them to B.
// The data in these records appears to be invalid.
         badDate = false;
         if (data[2].substring(0, 2).equals("00")) {
            badDate = true;
            int badRecord = i + 1;
            System.out.println("Record #: " + badRecord + " Date: " + data[2] + " Time " + data[3]);
            data[2] = data[0].substring(0, 4) + data[2].substring(4);
            if (Integer.parseInt(data[3].substring(3, 5)) > 59) {
               data[3] = data[3].substring(0, 3) + "55" + data[3].substring(5);
            }
            System.out.println("New Date: " + data[2] + " New Time: " + data[3] + "\n");
         }
// Parse actual date time.
// Put time into array once bad dates have been adjusted.
         d = dateTime.parse(data[2] + data[3]);         
         timeArray.set(i, d.getTime() / 1000);
// If first record, save the date time as the start date time.
// Check each following record to see if there is an earlier date time.
// If first record, save the date time as the end date time.
// Check for record with latest date time.
// Use the time variable and not the time_nominal variable.         
         if (i == 0) {
            tempStartDate = d;
            tempEndDate = d;
         }
         if (d.before(tempStartDate)) {
            tempStartDate = d;
         }
         if (d.after(tempEndDate)) {
            tempEndDate = d;
         }         
         networkArray.setString(i, data[4].trim());
         idArray.setString(i, data[5].trim());
         var = Double.parseDouble(data[6]);
         latArray.set(i, var);
         var = Double.parseDouble(data[7]);
         lonArray.set(i, var);            
         shortvar = Short.parseShort(data[8]);
         occArray.set(i, shortvar);
         var = Double.parseDouble(data[9]);
         eleArray.set(i, var);
// Convert pressure from mbar to pascal.
         var = Double.parseDouble(data[10]);
         if (var != fillValue) {
            var = var * 100;
         }
         pressArray.set(i, var); 
         if (badDate && (data[11].equals("G") || data[11].equals("D"))) {
            System.out.println("Old pressure qa code: " + data[11]);
            data[11] = "B";
            System.out.println("New pressure qa code: " + data[11]);            
         }
         pflagArray.set(i, data[11].charAt(0));
         var = Double.parseDouble(data[12]);
         if (var != fillValue) {
            var = var * 100;
         }
         pressRSLArray.set(i, var); 
         if (badDate && (data[13].equals("G") || data[13].equals("D"))) {
            System.out.println("Old pressure RSL qa code: " + data[13]);
            data[13] = "B";
            System.out.println("New pressure RSL qa code: " + data[13]);            
         }
         pRSLflagArray.set(i, data[13].charAt(0));
         var = Double.parseDouble(data[14]);
         if (var != fillValue) {
            var = var * 100;
         }
         pressCSLArray.set(i, var);  
         if (badDate && (data[15].equals("G") || data[15].equals("D"))) {
            System.out.println("Old pressure CSL qa code: " + data[15]);
            data[15] = "B";
            System.out.println("New pressure CSL qa code: " + data[15]);            
         }
         pCSLflagArray.set(i, data[15].charAt(0));
// Convert temperature from Celsius to Kelvin
         var = Double.parseDouble(data[16]);
         if (var != fillValue) {
            var = TemperatureUtils.convertTemperature(var,
                  TemperatureUtils.CELCIUS,  TemperatureUtils.KELVIN);
         }
         tempArray.set(i, var);
         if (badDate && (data[17].equals("G") || data[17].equals("D"))) {
            System.out.println("Old temp qa code: " + data[17]);
            data[17] = "B";
            System.out.println("New temp qa code: " + data[17]);            
         }
         tflagArray.set(i, data[17].charAt(0));
         var = Double.parseDouble(data[18]);
         if (var != fillValue) {
            var = TemperatureUtils.convertTemperature(var,
                  TemperatureUtils.CELCIUS,  TemperatureUtils.KELVIN);
         }
         dpArray.set(i, var);
         if (badDate && (data[19].equals("G") || data[19].equals("D"))) {
            System.out.println("Old dew point qa code: " + data[19]);
            data[19] = "B";
            System.out.println("New dew point qa code: " + data[19]);            
         }
         dpflagArray.set(i, data[19].charAt(0));
         var = Double.parseDouble(data[20]);
         wspdArray.set(i, var);
         if (badDate && (data[21].equals("G") || data[21].equals("D"))) {
            System.out.println("Old wind speed qa code: " + data[21]);
            data[21] = "B";
            System.out.println("New wind speed qa code: " + data[21]);            
         }
         wsflagArray.set(i, data[21].charAt(0));
         var = Double.parseDouble(data[22]);
         wdirArray.set(i, var);
         if (badDate && (data[23].equals("G") || data[23].equals("D"))) {
            System.out.println("Old wind dir qa code: " + data[23]);
            data[23] = "B";
            System.out.println("New wind dir qa code: " + data[23]);            
         } 
         wdflagArray.set(i, data[23].charAt(0));
         var = Double.parseDouble(data[24]);
         precipArray.set(i, var);
         if (badDate && (data[25].equals("G") || data[25].equals("D"))) {
            System.out.println("Old precip qa code: " + data[25]);
            data[25] = "B";
            System.out.println("New precip qa code: " + data[25]);            
         } 
         paflagArray.set(i, data[25].charAt(0));
         gustindArray.set(i, data[26].charAt(0));
         var = Double.parseDouble(data[27]);
         gustspArray.set(i, var);
         if (badDate && (data[28].equals("G") || data[28].equals("D"))) {
            System.out.println("Old gust qa code: " + data[28]);
            data[28] = "B";
            System.out.println("New gust qa code: " + data[28]);            
         } 
         gsflagArray.set(i, data[28].charAt(0));
         intvar = Integer.parseInt(data[29]);          
         preswxArray.set(i, intvar);
         if (badDate && (data[30].equals("G") || data[30].equals("D"))) {
            System.out.println("Old present weather qa code: " + data[30]);
            data[30] = "B";
            System.out.println("New present weather qa code: " + data[30]);            
         } 
         pwflagArray.set(i, data[30].charAt(0));
         var = Double.parseDouble(data[31]);
         visArray.set(i, var);
         if (badDate && (data[32].equals("G") || data[32].equals("D"))) {
            System.out.println("Old vis qa code: " + data[32]);
            data[32] = "B";
            System.out.println("New vis qa code: " + data[32]);            
         } 
         visflagArray.set(i, data[32].charAt(0));
// Original value is hundreds of feet.
// Convert to meters
         var = Double.parseDouble(data[33]);
         if (var != fillValue) {
            var = var * 100;
            var = LengthUtils.convertLength(var, LengthUtils.FEET, LengthUtils.METERS);
         }
         cloudArray.set(i, 0, var);
         intvar = Integer.parseInt(data[34]);
         cindArray.set(i, 0, intvar);
         if (badDate && (data[35].equals("G") || data[35].equals("D"))) {
            System.out.println("Old cind0 qa code: " + data[35]);
            data[35] = "B";
            System.out.println("New cind0 qa code: " + data[35]);            
         } 
         cindflagArray.set(i, 0, data[35].charAt(0));
         intvar = Integer.parseInt(data[36]);
         camtArray.set(i, 0, intvar);
         if (badDate && (data[37].equals("G") || data[37].equals("D"))) {
            System.out.println("Old camt0 qa code: " + data[37]);
            data[37] = "B";
            System.out.println("New camt0 qa code: " + data[37]);            
         } 
         camtflagArray.set(i, 0, data[37].charAt(0));
// Original value is hundreds of feet.
// Convert to meters
         var = Double.parseDouble(data[38]);  
         if (var != fillValue) {
            var = var * 100;
            var = LengthUtils.convertLength(var, LengthUtils.FEET, LengthUtils.METERS);
         }
         cloudArray.set(i, 1, var);
         intvar = Integer.parseInt(data[39]);
         cindArray.set(i, 1, intvar);
         if (badDate && (data[40].equals("G") || data[40].equals("D"))) {
            System.out.println("Old cind1 qa code: " + data[40]);
            data[40] = "B";
            System.out.println("New cind1 qa code: " + data[40]);            
         } 
         cindflagArray.set(i, 1, data[40].charAt(0));
         intvar = Integer.parseInt(data[41]);
         camtArray.set(i, 1, intvar);
         if (badDate && (data[42].equals("G") || data[42].equals("D"))) {
            System.out.println("Old camt1 qa code: " + data[42]);
            data[42] = "B";
            System.out.println("New camt1 qa code: " + data[42]);            
         } 
         camtflagArray.set(i, 1, data[42].charAt(0));
// Original value is hundreds of feet.
// Convert to meters
         var = Double.parseDouble(data[43]);
         if (var != fillValue) {
            var = var * 100;
            var = LengthUtils.convertLength(var, LengthUtils.FEET, LengthUtils.METERS);
         }
         cloudArray.set(i, 2, var);
         intvar = Integer.parseInt(data[44]);
         cindArray.set(i, 2, intvar);
         if (badDate && (data[45].equals("G") || data[45].equals("D"))) {
            System.out.println("Old cind2 qa code: " + data[45]);
            data[45] = "B";
            System.out.println("New cind2 qa code: " + data[45]);            
         } 
         cindflagArray.set(i, 2, data[45].charAt(0));
         intvar = Integer.parseInt(data[46]);
         camtArray.set(i, 2, intvar);
         if (badDate && (data[47].equals("G") || data[47].equals("D"))) {
            System.out.println("Old camt2 qa code: " + data[47]);
            data[47] = "B";
            System.out.println("New camt2 qa code: " + data[47]);            
         } 
         camtflagArray.set(i, 2, data[47].charAt(0));
      }
// Format date time values into readable format
      startDateTime = sdf.format(tempStartDate);
      endDateTime = sdf.format(tempEndDate);
      return count;
   }

/**
 * This routine returns the file type version.
 */
   public String getFileTypeVersion() {
      return("1.0");
   }

/**
 * This routine returns the file type description.
 */
   public String getFileTypeDescription() {
      return("QCF Surface Data");
   }

/**
 * This routine returns the file type id.
 */
   public String getFileTypeId() {
      return("QCFSurface");
   }

/**
 * This routine returns true/false for whether this follows the Builder pattern.
 */
   public boolean isBuilder() {
       return true;
   }

/**
  * This routine calls the readAllData routine to read the QCF data file.
  * Then it creates the netcdf file.
  *
  * @param infile the QCF data file.
  * @param rootGroup used to create the netcdf file.
  * @param cancelTask used to cancel the task if it is taking too long,
  *    not used here since all the conversions happen quickly.
  * @throws IOException when there is a problem with a data conversion.
  *    or there is an invalid conversion performed on a variable.
  */
   public void build(RandomAccessFile infile, Group.Builder rootGroup, CancelTask cancelTask)
           throws IOException {
     
      int nameLength = 10;
      int cloudLevelLength = 3;
      int numRecords;
      fillValue = -999.99;
      fillCloudValue = 15;
      String title, project, doi, versionInfo, authorInfo, tempPrimaryName, homePage, emailAddress;
      int versionSize, contactSize;
      int numContacts = 0;

      sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss Z");
      sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
      String date = sdf.format(Calendar.getInstance().getTime());
      try {
         numRecords = readAllData(infile);
      } catch (ParseException e) {
         e.printStackTrace();
         throw new IOException("QCFiospShortStn: Parse exception occurred.");
      } catch (ConversionException e) {
         e.printStackTrace();
         throw new IOException("QCFiospShortStn: Invalid conversion attempted.");
      }
// Connect to database to get data for global attriutes.
// Establish the connection to the database.
// Create the statements that query the dataset in the database.
      title = "";
      project = "";
      doi = "";
      versionInfo = "";
      authorInfo = "";
      homePage = "";
      emailAddress = "";
      try {
         Class.forName("com.mysql.jdbc.Driver");
         connection = DriverManager.getConnection(URL, user, pass);
         connection.setAutoCommit(false);
         datasetStmt = connection.prepareStatement("SELECT id, title FROM dataset WHERE archive_ident=?");
         projectStmt = connection.prepareStatement("SELECT name FROM project WHERE id IN (SELECT project_id from dataset_project where dataset_id=?)");
         versionStmt = connection.prepareStatement("SELECT version_number, publish_time, description FROM dataset_version WHERE dataset_id=?");
         doiStmt = connection.prepareStatement("SELECT handle FROM doi WHERE dataset_id=?");
         contactStmt = connection.prepareStatement("SELECT primary_name, person_name, organization_name, email, homepage FROM contact WHERE id IN (SELECT contact_id from dataset_contact where dataset_id=?)");
         datasetStmt.setString(1,datasetId);
         datasetStmt.execute();
         rs = datasetStmt.getResultSet();
         rs.next();
         int idVal = rs.getInt("id");
         title = rs.getString("title");
         rs.close();
         projectStmt.setString(1, String.format("%d", idVal));
         projectStmt.execute();
         
         rs = projectStmt.getResultSet();
         rs.next();
         project = rs.getString("name");
         rs.close();
         doiStmt.setString(1, String.format("%d", idVal));
         doiStmt.execute();
         rs = doiStmt.getResultSet();
         rs.next();
         doi = rs.getString("handle");
         rs.close();
         versionStmt.setString(1, String.format("%d", idVal));
         versionStmt.execute();
         rs = versionStmt.getResultSet();
         int i = 0;
         while(rs.next()) {
            if (i == 0) {
               versionInfo = rs.getString("version_number") + " (" + rs.getString("publish_time") + "): " + rs.getString("description");
            } else {
               versionInfo = versionInfo + "\n" + rs.getString("version_number") + " (" + rs.getString("publish_time") + "): " + rs.getString("description");
            }
            i++;
         }
         rs.close();
         ArrayList<String> primaryName = new ArrayList<>();
         ArrayList<String> organizationName = new ArrayList<>();
         ArrayList<String> personName = new ArrayList<>();
         ArrayList<String> authorName = new ArrayList<>();      
         contactStmt.setString(1, String.format("%d", idVal));
         contactStmt.execute();
         rs = contactStmt.getResultSet();
         i = 0;
         while(rs.next()) {
            tempPrimaryName = rs.getString("primary_name");
            primaryName.add(tempPrimaryName);
            if (i == 0) {
               emailAddress = rs.getString("email");
               homePage = rs.getString("homepage");
            }
            if ((tempPrimaryName.equals("position")) || (tempPrimaryName.equals("org"))) {  
               authorName.add(rs.getString("organization_name"));
            } else if (tempPrimaryName.equals("person")) {
               authorName.add(rs.getString("person_name"));
            }
            i++;
         }  
         numContacts = primaryName.size();
         rs.close();
         if (numContacts > 1) {
            authorInfo = "author1: " + authorName.get(0);
         } else {
            authorInfo = authorName.get(0);
         }
         if (numContacts >= 2) {
            authorInfo = authorInfo + "; author2: " + authorName.get(1); 
         } 
         if (numContacts >= 3) {
            authorInfo = authorInfo + "; author3: " + authorName.get(2);
         }
         connection.close();
      } catch (ClassNotFoundException e) {
         e.printStackTrace();
      } catch (SQLException e) {
         e.printStackTrace();
      }

// Create global attributes for the netCDF file.
      rootGroup.addAttribute(new Attribute("Conventions", "CF-1.11"));
      rootGroup.addAttribute(new Attribute("title", title));    
      rootGroup.addAttribute(new Attribute("Data_Type", "qcf"));
      rootGroup.addAttribute(new Attribute("Data_Type_Long_Name", "QCF Surface"));
      rootGroup.addAttribute(new Attribute("project", project));
      rootGroup.addAttribute(new Attribute("doi", doi));      
      rootGroup.addAttribute(new Attribute("source", "Surface meteorological observations from National Weather Service and other national agency weather station networks as well as regional mesonet observations from state and local agencies."));
      rootGroup.addAttribute(new Attribute("author", authorInfo));
// Set home page for EOL as the EOL main web page, and not the EOL Archive page
      if (homePage.equals("https://data.eol.ucar.edu/")) {
         homePage = "https://www.eol.ucar.edu/";
      }
      if (homePage != null) {
         rootGroup.addAttribute(new Attribute("author_url", homePage));
      }
      if (emailAddress != null) {
         rootGroup.addAttribute(new Attribute("author_email", emailAddress));
      }  
      rootGroup.addAttribute(new Attribute("acknowledgment", "NSF NCAR/EOL"));
      rootGroup.addAttribute(new Attribute("date_created", date));
      rootGroup.addAttribute(new Attribute("institution",
            "National Center for Atmospheric Research - Earth Observing Laboratory"));
      rootGroup.addAttribute(new Attribute("history", versionInfo));
      rootGroup.addAttribute(new Attribute("time_coverage_start", startDateTime));
      rootGroup.addAttribute(new Attribute("time_coverage_end", endDateTime));

// Create time and name string length dimensions for netcdf file.

      Dimension recordDim = Dimension.builder("record", numRecords).build();
      rootGroup.addDimension(recordDim);
      Dimension nameLengthDim = Dimension.builder("name_strlen", nameLength).build();
      rootGroup.addDimension(nameLengthDim);
      Dimension cloudLevelDim = Dimension.builder("cloud_level", cloudLevelLength).build();
      rootGroup.addDimension(cloudLevelDim);

// Create all the variables in the netCDF file.
      rootGroup.addVariable(Variable.builder().setName("time").setDataType(DataType.DOUBLE)
              .addDimension(recordDim).addAttribute(new Attribute("standard_name", "time"))
              .addAttribute(new Attribute("long_name", "UTC Actual Date/Time"))
              .addAttribute(new Attribute("units", "seconds since 1970-01-01 00:00:00"))
              .addAttribute(new Attribute("calendar", "standard"))
              .addAttribute(new Attribute("coordinates", "record"))
              .setCachedData(timeArray, false));
//      .addAttribute(new Attribute("_FillValue", fillValue))
//      .addAttribute(new Attribute("missing_value", fillValue)) 
      rootGroup.addVariable(Variable.builder().setName("time_nominal").setDataType(DataType.DOUBLE)
              .addDimension(recordDim).addAttribute(new Attribute("standard_name", "time_nominal"))
              .addAttribute(new Attribute("long_name", "UTC Nominal Date/Time"))
              .addAttribute(new Attribute("units", "seconds since 1970-01-01 00:00:00"))
              .addAttribute(new Attribute("calendar", "standard"))
              .addAttribute(new Attribute("coordinates", "record"))
              .setCachedData(timeNominalArray, false));
//      .addAttribute(new Attribute("_FillValue", fillValue))
//      .addAttribute(new Attribute("missing_value", fillValue))  
      rootGroup.addVariable(Variable.builder().setName("network_name").setDataType(DataType.CHAR)
              .addDimension(recordDim).addDimension(nameLengthDim)
              .addAttribute(new Attribute("standard_name", "network_name"))
              .addAttribute(new Attribute("long_name", "Network Identifier"))
              .addAttribute(new Attribute("coordinates", "name_strlen record"))
              .setCachedData(networkArray, false));   
      rootGroup.addVariable(Variable.builder().setName("platform_id").setDataType(DataType.CHAR)
              .addDimension(recordDim).addDimension(nameLengthDim)
              .addAttribute(new Attribute("standard_name", "platform_id"))
              .addAttribute(new Attribute("long_name", "Station Identifier"))
              .addAttribute(new Attribute("coordinates", "name_strlen record"))
              .setCachedData(idArray, false));
      rootGroup.addVariable(Variable.builder().setName("latitude").setDataType(DataType.DOUBLE)
              .addDimension(recordDim).addAttribute(new Attribute("standard_name", "latitude"))
              .addAttribute(new Attribute("long_name", "Station Latitude"))
              .addAttribute(new Attribute("units", "degree_north"))
              .addAttribute(new Attribute("coordinates", "record"))
              .setCachedData(latArray, false));
//      .addAttribute(new Attribute("_FillValue", fillValue))
//      .addAttribute(new Attribute("missing_value", fillValue))       
      rootGroup.addVariable(Variable.builder().setName("longitude").setDataType(DataType.DOUBLE)
              .addDimension(recordDim).addAttribute(new Attribute("standard_name", "longitude"))
              .addAttribute(new Attribute("long_name", "Station Longitude"))
              .addAttribute(new Attribute("units", "degree_east"))
              .addAttribute(new Attribute("coordinates", "record"))
              .setCachedData(lonArray, false));
//      .addAttribute(new Attribute("_FillValue", fillValue))
//      .addAttribute(new Attribute("missing_value", fillValue)) 
      rootGroup.addVariable(Variable.builder().setName("station_occurrence").setDataType(DataType.SHORT)
              .addDimension(recordDim).addAttribute(new Attribute("standard_name", "station_occurrence"))
              .addAttribute(new Attribute("long_name", "Station Occurrence"))
              .addAttribute(new Attribute("coordinates", "record"))
              .setCachedData(occArray, false));
//      .addAttribute(new Attribute("_FillValue", fillValue))
//      .addAttribute(new Attribute("missing_value", fillValue))       
      rootGroup.addVariable(Variable.builder().setName("surface_altitude").setDataType(DataType.DOUBLE)
              .addDimension(recordDim).addAttribute(new Attribute("standard_name", "surface_altitude"))
              .addAttribute(new Attribute("long_name", "Station Elevation"))
              .addAttribute(new Attribute("units", "m"))
              .addAttribute(new Attribute("coordinates", "record"))
              .setCachedData(eleArray, false));
//      .addAttribute(new Attribute("_FillValue", fillValue))
//      .addAttribute(new Attribute("missing_value", fillValue))      
      rootGroup.addVariable(Variable.builder().setName("surface_air_pressure").setDataType(DataType.DOUBLE)
              .addDimension(recordDim).addAttribute(new Attribute("standard_name", "surface_air_pressure"))
              .addAttribute(new Attribute("long_name", "Station Surface Air Pressure"))
              .addAttribute(new Attribute("units", "Pa"))
              .addAttribute(new Attribute("coordinates", "record"))
              .addAttribute(new Attribute("ancillary_variables", "surface_air_pressure_flag"))
              .addAttribute(new Attribute("_FillValue", fillValue))
              .addAttribute(new Attribute("missing_value", fillValue))
              .setCachedData(pressArray, false));
      rootGroup.addVariable(Variable.builder().setName("surface_air_pressure_flag").setDataType(DataType.CHAR)
              .addDimension(recordDim).addAttribute(new Attribute("standard_name", "surface_air_pressure status_flag"))
              .addAttribute(new Attribute("long_name", "Station Surface Air Pressure Flag"))
              .addAttribute(new Attribute("flag_values", "U G M D B N X E C T I"))
              .addAttribute(new Attribute("flag_meanings",
          "unchecked good missing questionable unlikely not_available_or_not_observed glitch estimated exceeds_9999.99mm_or_negative trace_amount_recorded not_computed_due_to_insufficient_data"))
              .addAttribute(new Attribute("coordinates", "record"))
              .addAttribute(new Attribute("_FillValue", "M"))
              .addAttribute(new Attribute("missing_value", "M"))
              .setCachedData(pflagArray, false));
      rootGroup.addVariable(Variable.builder().setName("air_pressure_at_mean_sea_level").setDataType(DataType.DOUBLE)
              .addDimension(recordDim).addAttribute(new Attribute("standard_name", "air_pressure_at_mean_sea_level"))
              .addAttribute(new Attribute("long_name", "Station Pressure at Mean Sea Level"))
              .addAttribute(new Attribute("units", "Pa"))
              .addAttribute(new Attribute("coordinates", "record"))
              .addAttribute(new Attribute("ancillary_variables", "air_pressure_at_mean_sea_level_flag"))
              .addAttribute(new Attribute("_FillValue", fillValue))
              .addAttribute(new Attribute("missing_value", fillValue))
              .setCachedData(pressRSLArray, false));
      rootGroup.addVariable(Variable.builder().setName("air_pressure_at_mean_sea_level_flag").setDataType(DataType.CHAR)
              .addDimension(recordDim).addAttribute(new Attribute("standard_name","air_pressure_at_mean_sea_level status_flag"))
              .addAttribute(new Attribute("long_name", "Station Pressure at Mean Sea Level Flag"))
              .addAttribute(new Attribute("flag_values", "U G M D B N X E C T I"))
              .addAttribute(new Attribute("flag_meanings",
          "unchecked good missing questionable unlikely not_available_or_not_observed glitch estimated exceeds_9999.99mm_or_negative trace_amount_recorded not_computed_due_to_insufficient_data"))
              .addAttribute(new Attribute("coordinates", "record"))
              .addAttribute(new Attribute("_FillValue", "M"))
              .addAttribute(new Attribute("missing_value", "M"))
              .setCachedData(pRSLflagArray, false));
      rootGroup.addVariable(Variable.builder().setName("air_pressure_at_mean_sea_level_computed").setDataType(DataType.DOUBLE)
              .addDimension(recordDim).addAttribute(new Attribute("standard_name", "air_pressure_at_mean_sea_level_computed"))
              .addAttribute(new Attribute("long_name", "Station Pressure at Mean Sea Level Computed"))
              .addAttribute(new Attribute("units", "Pa"))
              .addAttribute(new Attribute("coordinates", "record"))
              .addAttribute(new Attribute("ancillary_variables", "air_pressure_at_mean_sea_level_computed_flag"))
              .addAttribute(new Attribute("_FillValue", fillValue))
              .addAttribute(new Attribute("missing_value", fillValue))
              .setCachedData(pressCSLArray, false));
      rootGroup.addVariable(Variable.builder().setName("air_pressure_at_mean_sea_level_computed_flag").setDataType(DataType.CHAR)
              .addDimension(recordDim).addAttribute(new Attribute("standard_name","air_pressure_at_mean_sea_level_computed status_flag"))
              .addAttribute(new Attribute("long_name", "Station Pressure at Mean Sea Level Computed Flag"))
              .addAttribute(new Attribute("flag_values", "U G M D B N X E C T I"))
              .addAttribute(new Attribute("flag_meanings",
          "unchecked good missing questionable unlikely not_available_or_not_observed glitch estimated exceeds_9999.99mm_or_negative trace_amount_recorded not_computed_due_to_insufficient_data"))
              .addAttribute(new Attribute("coordinates", "record"))
              .addAttribute(new Attribute("_FillValue", "M"))
              .addAttribute(new Attribute("missing_value", "M"))
              .setCachedData(pCSLflagArray, false));
      rootGroup.addVariable(Variable.builder().setName("air_temperature").setDataType(DataType.DOUBLE)
              .addDimension(recordDim).addAttribute(new Attribute("standard_name", "air_temperature"))
              .addAttribute(new Attribute("long_name", "Air Temperature"))
              .addAttribute(new Attribute("units", "K"))
              .addAttribute(new Attribute("coordinates", "record"))
              .addAttribute(new Attribute("ancillary_variables", "air_temperature_flag"))
              .addAttribute(new Attribute("_FillValue", fillValue))
              .addAttribute(new Attribute("missing_value", fillValue))
              .setCachedData(tempArray, false));
      rootGroup.addVariable(Variable.builder().setName("air_temperature_flag").setDataType(DataType.CHAR)
              .addDimension(recordDim).addAttribute(new Attribute("standard_name", "air_temperature status_flag"))
              .addAttribute(new Attribute("long_name", "Air Temperature Flag"))
              .addAttribute(new Attribute("flag_values", "U G M D B N X E C T I"))
              .addAttribute(new Attribute("flag_meanings",
          "unchecked good missing questionable unlikely not_available_or_not_observed glitch estimated exceeds_9999.99mm_or_negative trace_amount_recorded not_computed_due_to_insufficient_data"))
              .addAttribute(new Attribute("coordinates", "record"))
              .addAttribute(new Attribute("_FillValue", "M"))
              .addAttribute(new Attribute("missing_value", "M"))
              .setCachedData(tflagArray, false));
      rootGroup.addVariable(Variable.builder().setName("dew_point_temperature").setDataType(DataType.DOUBLE)
              .addDimension(recordDim).addAttribute(new Attribute("standard_name", "dew_point_temperature"))
              .addAttribute(new Attribute("long_name", "Dew Point Temperature"))
              .addAttribute(new Attribute("units", "K"))
              .addAttribute(new Attribute("coordinates", "record"))
              .addAttribute(new Attribute("ancillary_variables", "dew_point_temperature_flag"))
              .addAttribute(new Attribute("_FillValue", fillValue))
              .addAttribute(new Attribute("missing_value", fillValue))
              .setCachedData(dpArray, false));
      rootGroup.addVariable(Variable.builder().setName("dew_point_temperature_flag").setDataType(DataType.CHAR)
              .addDimension(recordDim).addAttribute(new Attribute("standard_name","dew_point_temperature status_flag"))
              .addAttribute(new Attribute("long_name", "Dew Point Temperature Flag"))
              .addAttribute(new Attribute("flag_values", "U G M D B N X E C T I"))
              .addAttribute(new Attribute("flag_meanings",
          "unchecked good missing questionable unlikely not_available_or_not_observed glitch estimated exceeds_9999.99mm_or_negative trace_amount_recorded not_computed_due_to_insufficient_data"))
              .addAttribute(new Attribute("coordinates", "record"))
              .addAttribute(new Attribute("_FillValue", "M"))
              .addAttribute(new Attribute("missing_value", "M"))
              .setCachedData(dpflagArray, false));
      rootGroup.addVariable(Variable.builder().setName("wind_speed").setDataType(DataType.DOUBLE)
              .addDimension(recordDim).addAttribute(new Attribute("standard_name", "wind_speed"))
              .addAttribute(new Attribute("long_name", "Wind Speed"))
              .addAttribute(new Attribute("units", "m s-1"))
              .addAttribute(new Attribute("coordinates", "record"))
              .addAttribute(new Attribute("ancillary_variables", "wind_speed_flag"))
              .addAttribute(new Attribute("_FillValue", fillValue))
              .addAttribute(new Attribute("missing_value", fillValue))
              .setCachedData(wspdArray, false));
      rootGroup.addVariable(Variable.builder().setName("wind_speed_flag").setDataType(DataType.CHAR)
              .addDimension(recordDim).addAttribute(new Attribute("standard_name", "wind_speed status_flag"))
              .addAttribute(new Attribute("long_name", "Wind Speed Flag"))
              .addAttribute(new Attribute("flag_values", "U G M D B N X E C T I"))
              .addAttribute(new Attribute("flag_meanings",
          "unchecked good missing questionable unlikely not_available_or_not_observed glitch estimated exceeds_9999.99mm_or_negative trace_amount_recorded not_computed_due_to_insufficient_data"))
              .addAttribute(new Attribute("coordinates", "record"))
              .addAttribute(new Attribute("_FillValue", "M"))
              .addAttribute(new Attribute("missing_value", "M"))
              .setCachedData(wsflagArray, false));
      rootGroup.addVariable(Variable.builder().setName("wind_from_direction").setDataType(DataType.DOUBLE)
              .addDimension(recordDim).addAttribute(new Attribute("standard_name", "wind_from_direction"))
              .addAttribute(new Attribute("long_name", "Wind Direction"))
              .addAttribute(new Attribute("units", "degree"))
              .addAttribute(new Attribute("coordinates", "record"))
              .addAttribute(new Attribute("ancillary_variables", "wind_from_direction_flag"))
              .addAttribute(new Attribute("_FillValue", fillValue))
              .addAttribute(new Attribute("missing_value", fillValue))
              .setCachedData(wdirArray, false));
      rootGroup.addVariable(Variable.builder().setName("wind_from_direction_flag").setDataType(DataType.CHAR)
              .addDimension(recordDim).addAttribute(new Attribute("standard_name", "wind_from_direction status_flag"))
              .addAttribute(new Attribute("long_name", "Wind Direction Flag"))
              .addAttribute(new Attribute("flag_values", "U G M D B N X E C T I"))
              .addAttribute(new Attribute("flag_meanings",
          "unchecked good missing questionable unlikely not_available_or_not_observed glitch estimated exceeds_9999.99mm_or_negative trace_amount_recorded not_computed_due_to_insufficient_data"))
              .addAttribute(new Attribute("coordinates", "record"))
              .addAttribute(new Attribute("_FillValue", "M"))
              .addAttribute(new Attribute("missing_value", "M"))
              .setCachedData(wdflagArray, false));
      rootGroup.addVariable(Variable.builder().setName("precipitation_amount").setDataType(DataType.DOUBLE)
              .addDimension(recordDim).addAttribute(new Attribute("standard_name", "precipitation_amount"))
              .addAttribute(new Attribute("long_name", "Precipitation"))
              .addAttribute(new Attribute("units", "kg m-2"))
              .addAttribute(new Attribute("coordinates", "record"))
              .addAttribute(new Attribute("ancillary_variables", "precipitation_amount_flag"))
              .addAttribute(new Attribute("_FillValue", fillValue))
              .addAttribute(new Attribute("missing_value", fillValue))
              .setCachedData(precipArray, false));
      rootGroup.addVariable(Variable.builder().setName("precipitation_amount_flag").setDataType(DataType.CHAR)
              .addDimension(recordDim).addAttribute(new Attribute("standard_name", "precipitation_amount status_flag"))
              .addAttribute(new Attribute("long_name", "Precipitation Flag"))
              .addAttribute(new Attribute("flag_values", "U G M D B N X E C T I"))
              .addAttribute(new Attribute("flag_meanings",
          "unchecked good missing questionable unlikely not_available_or_not_observed glitch estimated exceeds_9999.99mm_or_negative trace_amount_recorded not_computed_due_to_insufficient_data"))
              .addAttribute(new Attribute("coordinates", "record"))
              .addAttribute(new Attribute("_FillValue", "M"))
              .addAttribute(new Attribute("missing_value", "M"))
              .setCachedData(paflagArray, false));
      rootGroup.addVariable(Variable.builder().setName("gust_indicator").setDataType(DataType.CHAR)
              .addDimension(recordDim).addAttribute(new Attribute("standard_name", "gust_indicator"))
              .addAttribute(new Attribute("long_name", "Squall/Gust Indicator"))
              .addAttribute(new Attribute("code_values", "blank S G"))
              .addAttribute(new Attribute("code_values_meanings", "no_squall_or_gust squall gust"))
              .addAttribute(new Attribute("coordinates", "record"))
              .setCachedData(gustindArray, false));      
      rootGroup.addVariable(Variable.builder().setName("wind_speed_of_gust").setDataType(DataType.DOUBLE)
              .addDimension(recordDim).addAttribute(new Attribute("standard_name", "wind_speed_of_gust"))
              .addAttribute(new Attribute("long_name", "Wind Speed of Gust"))
              .addAttribute(new Attribute("units", "m s-1"))
              .addAttribute(new Attribute("coordinates", "record"))
              .addAttribute(new Attribute("ancillary_variables", "wind_speed_of_gust_flag"))
              .addAttribute(new Attribute("_FillValue", fillValue))
              .addAttribute(new Attribute("missing_value", fillValue))
              .setCachedData(gustspArray, false));
      rootGroup.addVariable(Variable.builder().setName("wind_speed_of_gust_flag").setDataType(DataType.CHAR)
              .addDimension(recordDim).addAttribute(new Attribute("standard_name", "wind_speed_of_gust status_flag"))
              .addAttribute(new Attribute("long_name", "Wind Speed of Gust Flag"))
              .addAttribute(new Attribute("flag_values", "U G M D B N X E C T I"))
              .addAttribute(new Attribute("flag_meanings",
          "unchecked good missing questionable unlikely not_available_or_not_observed glitch estimated exceeds_9999.99mm_or_negative trace_amount_recorded not_computed_due_to_insufficient_data"))
              .addAttribute(new Attribute("coordinates", "record"))
              .addAttribute(new Attribute("_FillValue", "M"))
              .addAttribute(new Attribute("missing_value", "M"))
              .setCachedData(gsflagArray, false));
      rootGroup.addVariable(Variable.builder().setName("present_weather").setDataType(DataType.INT)
              .addDimension(recordDim).addAttribute(new Attribute("standard_name", "present_weather"))
              .addAttribute(new Attribute("long_name", "Present Weather"))
              .addAttribute(new Attribute("coordinates", "record"))
              .addAttribute(new Attribute("code_values", "See table 0-20-003 or 4677 at https://data.eol.ucar.edu/file/download/422947384D30/WMO-0-20-003-present-weather.csv"))
              .addAttribute(new Attribute("ancillary_variables", "present_weather_flag"))
              .addAttribute(new Attribute("_FillValue", -999))
              .addAttribute(new Attribute("missing_value", -999))
              .setCachedData(preswxArray, false));
      rootGroup.addVariable(Variable.builder().setName("present_weather_flag").setDataType(DataType.CHAR)
              .addDimension(recordDim).addAttribute(new Attribute("standard_name", "present_weather status_flag"))
              .addAttribute(new Attribute("long_name", "Present Weather Status Flag"))
              .addAttribute(new Attribute("flag_values", "U G M D B N X E C T I"))
              .addAttribute(new Attribute("flag_meanings",
          "unchecked good missing questionable unlikely not_available_or_not_observed glitch estimated exceeds_9999.99mm_or_negative trace_amount_recorded not_computed_due_to_insufficient_data"))
              .addAttribute(new Attribute("coordinates", "record"))
              .addAttribute(new Attribute("_FillValue", "M"))
              .addAttribute(new Attribute("missing_value", "M"))
              .setCachedData(pwflagArray, false));
      rootGroup.addVariable(Variable.builder().setName("visibility_in_air").setDataType(DataType.DOUBLE)
              .addDimension(recordDim).addAttribute(new Attribute("standard_name", "visibility_in_air"))
              .addAttribute(new Attribute("long_name", "Visibility"))
              .addAttribute(new Attribute("units", "m"))
              .addAttribute(new Attribute("coordinates", "record"))
              .addAttribute(new Attribute("ancillary_variables", "visibility_in_air_flag"))
              .addAttribute(new Attribute("_FillValue", fillValue))
              .addAttribute(new Attribute("missing_value", fillValue))
              .setCachedData(visArray, false));
      rootGroup.addVariable(Variable.builder().setName("visibility_in_air_flag").setDataType(DataType.CHAR)
              .addDimension(recordDim).addAttribute(new Attribute("standard_name", "visibility_in_air status_flag"))
              .addAttribute(new Attribute("long_name", "Visibility Flag"))
              .addAttribute(new Attribute("flag_values", "U G M D B N X E C T I"))
              .addAttribute(new Attribute("flag_meanings",
          "unchecked good missing questionable unlikely not_available_or_not_observed glitch estimated exceeds_9999.99mm_or_negative trace_amount_recorded not_computed_due_to_insufficient_data"))
              .addAttribute(new Attribute("coordinates", "record"))
              .addAttribute(new Attribute("_FillValue", "M"))
              .addAttribute(new Attribute("missing_value", "M"))
              .setCachedData(visflagArray, false));
      rootGroup.addVariable(Variable.builder().setName("cloud_base_altitude").setDataType(DataType.DOUBLE)
              .addDimension(recordDim).addDimension(cloudLevelDim).addAttribute(new Attribute("standard_name", "cloud_base_altitude"))
              .addAttribute(new Attribute("long_name", "Cloud Base Altitude"))
              .addAttribute(new Attribute("units", "m"))
              .addAttribute(new Attribute("coordinates", "cloud_level record"))
              .addAttribute(new Attribute("_FillValue", fillValue))
              .addAttribute(new Attribute("missing_value", fillValue))
              .setCachedData(cloudArray, false));
      rootGroup.addVariable(Variable.builder().setName("cloud_base_indicator").setDataType(DataType.INT)
              .addDimension(recordDim).addDimension(cloudLevelDim).addAttribute(new Attribute("standard_name", "cloud_base_indicator"))
              .addAttribute(new Attribute("long_name", "Cloud Base Indicator"))
              .addAttribute(new Attribute("code_values", "0 1 2 3 4 5 6 7 8 9 10 11 15"))
              .addAttribute(new Attribute("code_values_meanings", "none thin clear_below_12,000ft estimated measured indefinite balloon aircraft measured/variable clear_below_6,000ft(AUTOB) estimated/variable indefinite/variable missing"))
              .addAttribute(new Attribute("coordinates", "cloud_level record"))
              .addAttribute(new Attribute("ancillary_variables", "cloud_base_indicator_flag"))
              .addAttribute(new Attribute("_FillValue", fillCloudValue))
              .addAttribute(new Attribute("missing_value", fillCloudValue))
              .setCachedData(cindArray, false));
      rootGroup.addVariable(Variable.builder().setName("cloud_base_indicator_flag").setDataType(DataType.CHAR)
              .addDimension(recordDim).addDimension(cloudLevelDim).addAttribute(new Attribute("standard_name","cloud_base_indicator status_flag"))
              .addAttribute(new Attribute("long_name", "Cloud Base Indicator Flag"))
              .addAttribute(new Attribute("flag_values", "U G M D B N X E C T I"))
              .addAttribute(new Attribute("flag_meanings",
          "unchecked good missing questionable unlikely not_available_or_not_observed glitch estimated exceeds_9999.99mm_or_negative trace_amount_recorded not_computed_due_to_insufficient_data"))
              .addAttribute(new Attribute("coordinates", "cloud_level record"))
              .addAttribute(new Attribute("_FillValue", "M"))
              .addAttribute(new Attribute("missing_value", "M"))
              .setCachedData(cindflagArray, false));
      rootGroup.addVariable(Variable.builder().setName("cloud_area_fraction").setDataType(DataType.INT)
              .addDimension(recordDim).addDimension(cloudLevelDim).addAttribute(new Attribute("standard_name", "cloud_area_fraction"))
              .addAttribute(new Attribute("long_name", "Cloud Area Fraction"))
              .addAttribute(new Attribute("coordinates", "cloud_level record"))
              .addAttribute(new Attribute("code_values", "See table 0-20-011 or 2700 at https://data.eol.ucar.edu/file/download/422947384D32/WMO-0-20-011-cloud-amount.csv"))
              .addAttribute(new Attribute("ancillary_variables", "cloud_area_fraction_flag"))
              .addAttribute(new Attribute("_FillValue", fillCloudValue))
              .addAttribute(new Attribute("missing_value", fillCloudValue))
              .setCachedData(camtArray, false));
      rootGroup.addVariable(Variable.builder().setName("cloud_area_fraction_flag").setDataType(DataType.CHAR)
              .addDimension(recordDim).addDimension(cloudLevelDim).addAttribute(new Attribute("standard_name", "cloud_area_fraction status_flag"))
              .addAttribute(new Attribute("long_name", "Cloud Area Fraction Flag"))
              .addAttribute(new Attribute("flag_values", "U G M D B N X E C T I"))
              .addAttribute(new Attribute("flag_meanings",
          "unchecked good missing questionable unlikely not_available_or_not_observed glitch estimated exceeds_9999.99mm_or_negative trace_amount_recorded not_computed_due_to_insufficient_data"))
              .addAttribute(new Attribute("coordinates", "cloud_level record"))
              .addAttribute(new Attribute("_FillValue", "M"))
              .addAttribute(new Attribute("missing_value", "M"))
              .setCachedData(camtflagArray, false));
   }

/**
  * Print out command usage if arguments incorrect
  */
   protected static void usage(int ret) {
      System.err.println("usage: QCFiospShortStn\n" +
            "-i <inQCFfile>     QCF data file to convert\n" +
            "-o <outdirname>    Directory for output file (defaults to current directory)\n" +
            "-u <username>      User name for database access\n" +
            "-p <password>      Password for database access\n" +
            "-d <dataset>       Dataset id (xxx.xxx)");
      System.exit(ret);
   }

   public static void main(String[] args) throws Exception {

// Parse arguments on command line
      Getopt g = new Getopt("QCFiospShortStn", args, "i:o:u:p:d:");

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
            case 'u':
               user = g.getOptarg();
               break;
            case 'p':
               pass = g.getOptarg();
               break;
            case 'd':
               datasetId = g.getOptarg();
               break;
            default:
               usage(1);
         }
      }
      if (infilename == null) {
         usage(1);
      }
      System.out.println("Input file is " + infilename + "\n");
      ucar.nc2.NetcdfFiles.registerIOProvider(QCFiospShortStn.class);
      ncfile = null;
// Save information from QCF data file name to check against data
// in the file after stripping off the directory name (if supplied).
      testfilename = infilename;
      int position = infilename.lastIndexOf("/");
      if (position != -1) {
         testfilename = infilename.substring(position + 1);
      }
// Create output netCDF file name.
      position = testfilename.lastIndexOf(".");
      if (position == -1) {
         System.out.println("File name does not have a file extension: " + infilename + "\n");
         System.exit(1);
      }
      fileType = testfilename.substring(position + 1);
      outfilename = testfilename.substring(0, position) + "_" + fileType + ".nc";
      if (outdirname != null) {
         outfilename = outdirname + "/" + outfilename;
      }
      try {
         ncfile = NetcdfFiles.open(infilename);
         System.out.println("ncfile created is " + outfilename + "\n");
// Write all data out to the netCDF file.
         NetcdfFormatWriter.Builder writerb = NetcdfFormatWriter.createNewNetcdf3(outfilename);
         NetcdfCopier copier = NetcdfCopier.create(ncfile, writerb);
         ncfile = copier.write(null);
      } catch (FileNotFoundException e) {
         e.printStackTrace();
         throw new IOException(e.getMessage());
      } catch (IOException e) {
// Create message for any IO exceptions that are not FileNotFound exceptions, since the error
// message that is generated from the Unidata isValidFile routine is an incorrect message. 
         e.printStackTrace();
         throw new IOException(e.getMessage());
//         throw new IOException("ERROR: Input file " + infilename + " is not a valid QCF file.");
      } catch (Exception e) {
         e.printStackTrace();
         throw new Exception(e.getMessage());
      } finally {
         if (ncfile != null) {
            ncfile.close();
         }
      }
   }
}
