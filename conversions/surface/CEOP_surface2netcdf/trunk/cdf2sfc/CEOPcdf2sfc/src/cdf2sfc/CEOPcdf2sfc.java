/*
 * CEOPcdf2sfc.java
 *
 */

package cdf2sfc;

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
 * Read CEOP Surface netCDF data files and convert to a CEOP Surface ascii file.
 *
 * This program reads the CEOP Surface Meteorological and Radiation Dataset netCDF
 * data files and converts them to the original CEOP Surface ascii data format.
 *
 * @author $Author: anstett $
 * @version $Revision: 1.0 $
 */
public class CEOPcdf2sfc {
   protected NetcdfFile ncfile = null;        /* input netCDF file */
   protected BufferedWriter outfile = null;   /* output CEOP data file */
   protected static String outfilename;       /* Name of the output file. */

   /**
    * Arrays used to store the data for each variable in the netcdf file.
    */
   private double latitude, longitude, altitude;
   private ArrayDouble.D1 timeArray, timeNominalArray, dphofluxArray, uphofluxArray, shArray;
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
   private String rhp = null, rsi = null, station = null;

   /**
    * This routine reads the netCDF file and stores all the data in arrays.
    *
    * @param ncfile - input netCDF file to convert to CEOP surface format.
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
      String varName = "longitude";
      Variable v = ncfile.findVariable(varName);
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
      varName = "surface_air_pressure";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         pressArray = (ArrayDouble.D1) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "surface_air_pressure_flag";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         pflagArray = (ArrayChar.D1) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "air_temperature";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         tempArray = (ArrayDouble.D1) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "air_temperature_flag";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         tflagArray = (ArrayChar.D1) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "dew_point_temperature";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         dpArray = (ArrayDouble.D1) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "dew_point_temperature_flag";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         dpflagArray = (ArrayChar.D1) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "relative_humidity";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         rhArray = (ArrayDouble.D1) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "relative_humidity_flag";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         rhflagArray = (ArrayChar.D1) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "specific_humidity";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         shArray = (ArrayDouble.D1) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "specific_humidity_flag";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         shflagArray = (ArrayChar.D1) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "wind_speed";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         wspdArray = (ArrayDouble.D1) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "wind_speed_flag";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         wsflagArray = (ArrayChar.D1) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "wind_from_direction";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         wdirArray = (ArrayDouble.D1) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "wind_from_direction_flag";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         wdflagArray = (ArrayChar.D1) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "eastward_wind";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         eastwArray = (ArrayDouble.D1) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "eastward_wind_flag";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         ewflagArray = (ArrayChar.D1) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "northward_wind";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         northwArray = (ArrayDouble.D1) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "northward_wind_flag";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         nwflagArray = (ArrayChar.D1) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "precipitation_amount";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         precipArray = (ArrayDouble.D1) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "precipitation_amount_flag";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         paflagArray = (ArrayChar.D1) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "surface_snow_thickness";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         snowArray = (ArrayDouble.D1) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "surface_snow_thickness_flag";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         snowflagArray = (ArrayChar.D1) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "surface_downwelling_shortwave_flux_in_air";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         dshortfluxArray = (ArrayDouble.D1) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "surface_downwelling_shortwave_flux_in_air_flag";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         dsfflagArray = (ArrayChar.D1) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "surface_upwelling_shortwave_flux_in_air";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         ushortfluxArray = (ArrayDouble.D1) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "surface_upwelling_shortwave_flux_in_air_flag";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         usfflagArray = (ArrayChar.D1) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "surface_downwelling_longwave_flux_in_air";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         dlongfluxArray = (ArrayDouble.D1) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "surface_downwelling_longwave_flux_in_air_flag";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         dlfflagArray = (ArrayChar.D1) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "surface_upwelling_longwave_flux_in_air";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         ulongfluxArray = (ArrayDouble.D1) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "surface_upwelling_longwave_flux_in_air_flag";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         ulfflagArray = (ArrayChar.D1) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "surface_net_downward_radiative_flux";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         radArray = (ArrayDouble.D1) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "surface_net_downward_radiative_flux_flag";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         radflagArray = (ArrayChar.D1) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "surface_temperature";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         surftArray = (ArrayDouble.D1) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "surface_temperature_flag";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         stflagArray = (ArrayChar.D1) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "surface_downwelling_photosynthetic_photon_flux_in_air";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         dphofluxArray = (ArrayDouble.D1) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "surface_downwelling_photosynthetic_photon_flux_in_air_flag";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         dpfflagArray = (ArrayChar.D1) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "surface_upwelling_photosynthetic_photon_flux_in_air";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         uphofluxArray = (ArrayDouble.D1) v.read();
      } catch (IOException e) {
         throw new IOException("Trying to read variable: " + varName);
      }
      varName = "surface_upwelling_photosynthetic_photon_flux_in_air_flag";
      v = ncfile.findVariable(varName);
      if (v == null) {
         throw new IOException("Cannot find variable: " + varName);
      }
      try {
         upfflagArray = (ArrayChar.D1) v.read();
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
    * This routine writes all the data read from the CEOP surface netCDF data file.
    * It creates a CEOP surface ascii file from the data read in.
    *
    * @param outfile - output CEOP surface ascii file.
    * @throws IOException when there is a problem with any of the conversions
    * attempted on the data.
    */

   protected void writeCEOPSurface(BufferedWriter outfile) throws IOException {

      double fillValue = -999.99;
      String outrecord;
      double var;
      double kilogramConversionFactor = 1000.0;
      double moleConversionFactor = 1000000.0;

      this.outfile = outfile;
      SimpleDateFormat dateTime = new SimpleDateFormat("yyyy/MM/dd HH:mm");
      dateTime.setTimeZone(TimeZone.getTimeZone("UTC"));
      int[] shape = timeArray.getShape();
      for (int i = 0; i < shape[0]; i++) {
         Date d = new Date((long)(timeNominalArray.get(i) * 1000));
         outrecord = dateTime.format(d) + " ";
         d = new Date((long)(timeArray.get(i) * 1000));
         outrecord += dateTime.format(d) + " ";
         outrecord += String.format("%-10s %-15s %-15s %10.5f %11.5f %7.2f ",
               rhp, rsi, station, latitude, longitude, altitude);
         var = pressArray.get(i);
         try {
            if (var != fillValue) {
               var = PressureUtils.convertPressure(var, PressureUtils.PASCALS,
                     PressureUtils.HECTOPASCALS);
            }
         } catch (ConversionException e) {
            throw new IOException("Invalid DMGUtil conversion attempted " + e + " \n");
         }
         outrecord += String.format("%7.2f %c ", var, pflagArray.get(i));
         var = tempArray.get(i);
         try {
            if (var != fillValue) {
               var = TemperatureUtils.convertTemperature(var, TemperatureUtils.KELVIN,
                     TemperatureUtils.CELCIUS);
            }
         } catch (ConversionException e) {
            throw new IOException("Invalid DMGUtil conversion attempted " + e + " \n");
         }
         outrecord += String.format("%7.2f %c ", var, tflagArray.get(i));
         var = dpArray.get(i);
         try {
            if (var != fillValue) {
               var = TemperatureUtils.convertTemperature(var, TemperatureUtils.KELVIN,  
                     TemperatureUtils.CELCIUS);
            }
         } catch (ConversionException e) {
            throw new IOException("Invalid DMGUtil conversion attempted " + e + " \n");
         }
         outrecord += String.format("%7.2f %c %7.2f %c ", var, dpflagArray.get(i),
               rhArray.get(i), rhflagArray.get(i));
         var = shArray.get(i);
         if (var != fillValue) {
            var = var * kilogramConversionFactor;
         }
         outrecord += String.format("%7.2f %c %7.2f %c %7.2f %c %7.2f %c %7.2f %c %7.2f %c ",
               var, shflagArray.get(i), wspdArray.get(i), wsflagArray.get(i), wdirArray.get(i),
               wdflagArray.get(i), eastwArray.get(i), ewflagArray.get(i), northwArray.get(i),
               nwflagArray.get(i), precipArray.get(i), paflagArray.get(i));
         var = snowArray.get(i);
         try {
            if (var != fillValue) {
               var = LengthUtils.convertLength(var,
                     LengthUtils.METERS, LengthUtils.CENTIMETERS);
            }
         } catch (ConversionException e) {
            throw new IOException("Invalid DMGUtil conversion attempted " + e + " \n");
         }
         outrecord += String.format("%7.2f %c %8.2f %c %8.2f %c %8.2f %c %8.2f %c %8.2f %c ",
               var, snowflagArray.get(i), dshortfluxArray.get(i), dsfflagArray.get(i), ushortfluxArray.get(i),
               usfflagArray.get(i), dlongfluxArray.get(i), dlfflagArray.get(i), ulongfluxArray.get(i),
               ulfflagArray.get(i), radArray.get(i), radflagArray.get(i));
         var = surftArray.get(i);
         try {
            if (var != fillValue) {
               var = TemperatureUtils.convertTemperature(var, TemperatureUtils.KELVIN,
                     TemperatureUtils.CELCIUS);
            }
         } catch (ConversionException e) {
            throw new IOException("Invalid DMGUtil conversion attempted " + e + " \n");
         }
         outrecord += String.format("%8.2f %c ", var, stflagArray.get(i));
         var = dphofluxArray.get(i);
         if (var != fillValue) {
            var = var * moleConversionFactor;
         }
         outrecord += String.format("%8.2f %c ", var, dpfflagArray.get(i));
         var = uphofluxArray.get(i);
         if (var != fillValue) {
            var = var * moleConversionFactor;
         }
         outrecord += String.format("%8.2f %c", var, upfflagArray.get(i));                  
         outfile.write(outrecord);
         outfile.newLine();
      }
   }

   protected void usage(int ret) {
      System.err.println("usage: CEOPcdf2sfc\n" +
            "-i <inCEOPfile>    CEOP surface netCDF file to convert\n" +
            "-o <outdirname>    Directory for output file (defaults to current directory)\n");
      System.exit(ret);
   }

   public static void main(String[] args) throws Exception {

// Parse arguments on command line
      Getopt g = new Getopt("CEOPsfc2cdf", args, "i:o:");

      String infilename = null;
      String infiletype = null;
      String outdirname = null;
      int c, inlength;

      CEOPcdf2sfc conversion = new CEOPcdf2sfc();
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
      if (! infiletype.equals("sfc")) {
         System.out.println("ERROR: Input file is not CEOP surface data in netCDF format.");
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
         conversion.writeCEOPSurface(outfile);
      } catch (IOException e) {
         throw new IOException(e.getMessage());
      } finally {
         if (outfile != null) try {
            outfile.close();
            System.out.println("CEOP surface ascii file created: " + outfilename + "\n");
         } catch (IOException e) {
            throw new IOException(e.getMessage());
         }
      }
   }
}
