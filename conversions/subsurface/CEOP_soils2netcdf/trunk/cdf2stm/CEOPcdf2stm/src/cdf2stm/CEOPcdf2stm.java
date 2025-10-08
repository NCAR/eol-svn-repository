/*
 * CEOPcdf2stm.java
 *
 */
package cdf2stm;

import ucar.ma2.*;

import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;
import ucar.nc2.Attribute;

import java.io.*;
import java.util.*;
import java.lang.String;
import java.text.SimpleDateFormat;

import gnu.getopt.*;

import dmg.util.*;

/**
 * Read CEOP Soils netCDF data files and convert to a CEOP Soils ascii file.
 *
 * This program reads the CEOP Meteorological Soils Dataset netCDF data
 * files and converts them to the original CEOP Soils ascii data format.
 *
 * @author $Author: anstett $
 * @version $Revision: 1.0 $
 */
public class CEOPcdf2stm {
   protected NetcdfFile ncfile = null;         /* input netCDF file */
   protected BufferedWriter outfile = null;    /* output CEOP data file */
   protected static String outfilename;        /* Name of the output file. */

   /**
    * Arrays used to store the data for each variable in the netcdf file.
    */
   private double latitude, longitude, altitude;
   private ArrayDouble.D1 heightArray;
   private ArrayDouble.D1 timeArray, timeNominalArray;
   private ArrayDouble.D2 tempArray, moistArray;
   private ArrayChar.D2 tflagArray, mflagArray;
//  Data saved for the global attributes.
   private String rhp = null, rsi = null, station = null;

   /**
    * This routine reads the netCDF file and stores all the data in arrays.
    *
    * @param ncfile - input netCDF file to convert to CEOP soils format.
    * @throws IOException if there is a problem reading the netCDF file.
    */
   protected void readNetcdfFile(NetcdfFile ncfile) throws IOException {

      this.ncfile = ncfile;
      Attribute attribute = ncfile.findGlobalAttribute("RHP_Identifier_(formerly_known_as_CSE)");
      if (attribute.isString()) {
         rhp = attribute.getStringValue();
      }
      attribute = ncfile.findGlobalAttribute("Reference_Site_Identifier");
      if (attribute.isString()) {
         rsi = attribute.getStringValue();
      }
      attribute = ncfile.findGlobalAttribute("Station_Identifier");
      if (attribute.isString()) {
         station = attribute.getStringValue();
      }
      String varName = "height";
      Variable v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         heightArray = (ArrayDouble.D1) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "longitude";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         longitude = v.readScalarDouble();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "latitude";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         latitude = v.readScalarDouble();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "altitude";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         altitude = v.readScalarDouble();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "time";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         timeArray = (ArrayDouble.D1) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "time_nominal";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         timeNominalArray = (ArrayDouble.D1) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "soil_temperature";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         tempArray = (ArrayDouble.D2) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "soil_temperature_flag";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         tflagArray = (ArrayChar.D2) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "volume_fraction_of_water_in_soil";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         moistArray = (ArrayDouble.D2) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "volume_fraction_of_water_in_soil_flag";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         mflagArray = (ArrayChar.D2) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
// Create dates to add to the output filename.
      SimpleDateFormat dateTime = new SimpleDateFormat("yyyyMMdd");
      dateTime.setTimeZone(TimeZone.getTimeZone("UTC"));
      int[] shape = timeArray.getShape();
      Date d = new Date((long)(timeArray.get(0) * 1000)); 
      outfilename += "_" + dateTime.format(d);
      d = new Date((long)(timeArray.get(shape[0] - 1) * 1000));
      outfilename += "_" + dateTime.format(d);
   }  

   /**
    * This routine writes all the data read from the CEOP soils netCDF data file.
    * It creates a CEOP soils ascii file from the data read in.
    *
    * @param outfile - output CEOP surface ascii file.
    * @throws IOException when there is a problem with any of the conversions
    * attempted on the data.
    */

   protected void writeCEOPSoils(BufferedWriter outfile) throws IOException {

      double fillValue = -999.99;
      String outrecord, outrecord2;
      double var;
      double moleConversionFactor = 1000000.0;

      this.outfile = outfile;
      SimpleDateFormat dateTime = new SimpleDateFormat("yyyy/MM/dd HH:mm");
      dateTime.setTimeZone(TimeZone.getTimeZone("UTC"));
      int[] shape = timeArray.getShape();
      int[] shape2 = heightArray.getShape();
      for (int i = 0; i < shape[0]; i++) {
         Date d = new Date((long)(timeNominalArray.get(i) * 1000));         
         outrecord = dateTime.format(d) + " ";
         d = new Date((long)(timeArray.get(i) * 1000));
         outrecord += dateTime.format(d) + " ";
         outrecord += String.format("%-10s %-15s %-15s %10.5f %11.5f %7.2f ",
               rhp, rsi, station, latitude, longitude, altitude);
         for (int j = 0; j < shape2[0]; j++) {
            outrecord2 = String.format("%7.2f ", heightArray.get(j));
            var = tempArray.get(i, j);
            try {
               if (var != fillValue) {
                  var = TemperatureUtils.convertTemperature(var, TemperatureUtils.KELVIN,
                        TemperatureUtils.CELCIUS);
               }
            } catch (ConversionException e) {
               throw new IOException("Invalid DMGUtil conversion attempted " + e + " \n");
            }
            outrecord2 += String.format("%8.2f %c %8.2f %c", var, tflagArray.get(i, j),
               moistArray.get(i,j), mflagArray.get(i,j));
            outfile.write(outrecord + outrecord2);
            outfile.newLine();
         }
      }
   }

   protected void usage(int ret) {
      System.err.println("usage: CEOPcdf2stm\n" +
            "-i <inCEOPfile>    CEOP soils netCDF file to convert" +
            "-o <outdirname>    Directory for output file (defaults to current directory)\n");
      System.exit(ret);
   }

   public static void main(String[] args) throws Exception {

// Parse arguments on command line
      Getopt g = new Getopt("CEOPcdf2stm", args, "i:o:");

      String infilename = null;
      String infiletype = null;
      String outdirname = null;
      int c, inlength;

      CEOPcdf2stm conversion = new CEOPcdf2stm();
      while ((c = g.getopt()) != -1) {
         switch (c) {
            case 'i':
               infilename = g.getOptarg();
               int tmp = infilename.length() - 6;
               infiletype = infilename.substring(tmp, tmp + 3);
               break;
            case 'o':
               outdirname = g.getOptarg();
               break;
            default:
               conversion.usage(1);
         }
      }
      if (infilename == null) {
         conversion.usage(1);
      }
      if (! infiletype.equals("stm")) {
         System.out.println("ERROR: Input file is not CEOP soils data in netCDF format.");
         System.exit(1);
      }
      inlength = infilename.length() - 7;
      outfilename = infilename.substring(0, inlength);
      int position = infilename.lastIndexOf("/");
      if (position != -1) {
         outfilename = infilename.substring(position + 1, inlength);
      }
      if (outdirname != null) {
         outfilename = outdirname + "/" + outfilename;
      }

      NetcdfFile ncfile = null;
      BufferedWriter outfile = null;
      try {
         ncfile = NetcdfFile.open(infilename);
         conversion.readNetcdfFile(ncfile);
      } catch (IOException e) {
         throw new IOException(e.getMessage());
      } finally {
         if (ncfile != null) try {
            ncfile.close();
            System.out.println("Input file is " + infilename + "\n");
         } catch (IOException e) {
            throw new IOException(e.getMessage());
         }
      }
      outfilename += "_nc." + infiletype;
      try {
         outfile = new BufferedWriter(new FileWriter(outfilename));
         conversion.writeCEOPSoils(outfile);
      } catch (IOException e) {
         throw new IOException(e.getMessage());
      } finally {
         if (outfile != null) try {
            outfile.close();
            System.out.println("CEOP soils ascii file created: " + outfilename + "\n");
         } catch (IOException e) {
            throw new IOException(e.getMessage());
         }
      }
   }
}
